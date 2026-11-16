package skuaka.db

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import skuaka.shared.*

import java.util.UUID

/** カレンダーの1行（自分から見たもの. role はチームへの公開から加えたものなら公開の範囲）. */
final case class CalendarRow(id: UUID, ownerId: UUID, name: String, description: String, color: String, primary: Boolean,
    publicToken: Option[String], publicAccess: String, teamAccess: String, role: String, viaTeam: Boolean, myColor: Option[String], visible: Boolean)

/** チームに公開されたカレンダー（自分の一覧に入っているか）. */
final case class TeamCalendarRow(id: UUID, ownerId: UUID, name: String, description: String, color: String, primary: Boolean,
    access: String, added: Boolean)

/**
 * calendar_member の role = 'team' は「チームに公開されたカレンダーを自分で加えた」印で、権限は calendar.team_access に従う.
 * 所有者が公開をやめたら（none）、その行は何も見せない. 別名は calendar が c、calendar_member が m.
 */
object Member:
  val Team = "team"
  /** 実際の権限（公開をやめたチームの行は null）. */
  def role(c: String, m: String): Fragment =
    Fragment.const(s"(case when $m.role = 'team' then nullif($c.team_access, 'none') else $m.role end)")
  /** 権限のある行か. */
  def active(c: String, m: String): Fragment = Fragment.const(s"($m.role <> 'team' or $c.team_access <> 'none')")

/** カレンダー・共有の相手・表示の設定. */
final class CalendarStore(xa: Transactor[IO], users: UserStore) extends TxStore(xa):

  private val cols = fr"c.id, c.owner_id, c.name, c.description, c.color, c.is_primary, c.public_token, c.public_access, c.team_access," ++
    Member.role("c", "m") ++ fr", m.role = 'team', m.color, m.visible"

  /** 自分のカレンダー（ログインのたびに確かめ、なければ作る）. */
  def ensurePrimary(u: User): IO[Unit] = tx(
    sql"select exists(select 1 from calendar where owner_id = ${u.id} and is_primary)".query[Boolean].unique.flatMap { has =>
      if has then ().pure[ConnectionIO]
      else createC(u.id, CalendarReq(u.name, Palette.Default, "", Some(TeamAccess.Busy)), primary = true).void
    }
  )

  private def createC(owner: UUID, r: CalendarReq, primary: Boolean): ConnectionIO[UUID] =
    val id = UUID.randomUUID()
    val access = r.teamAccess.getOrElse(TeamAccess.None)
    sql"""insert into calendar (id, owner_id, name, description, color, is_primary, team_access)
          values ($id, $owner, ${r.name}, ${r.description}, ${r.color}, $primary, $access)
          on conflict do nothing""".update.run *>
      sql"insert into calendar_member (calendar_id, user_id, role) values ($id, $owner, 'owner') on conflict do nothing".update.run.as(id)

  def create(owner: UUID, r: CalendarReq): IO[UUID] = tx(createC(owner, r, primary = false))

  def list(uid: UUID): IO[List[CalendarRow]] = tx(
    (fr"select" ++ cols ++ fr"""from calendar c join calendar_member m on m.calendar_id = c.id and m.user_id = $uid
          where""" ++ Member.active("c", "m") ++ fr"""order by (c.owner_id = $uid) desc, c.is_primary desc, c.name""").query[CalendarRow].to[List]
  )

  def get(uid: UUID, id: UUID): IO[Option[CalendarRow]] = tx(
    (fr"select" ++ cols ++ fr"from calendar c join calendar_member m on m.calendar_id = c.id and m.user_id = $uid where c.id = $id and" ++
      Member.active("c", "m")).query[CalendarRow].option
  )

  def owners(ids: List[UUID]): IO[Map[UUID, UserBrief]] = tx(users.briefs(ids))

  def update(id: UUID, r: CalendarReq): IO[Unit] = tx(
    (fr"update calendar set name = ${r.name}, description = ${r.description}, color = ${r.color}" ++
      r.teamAccess.fold(Fragment.empty)(a => fr", team_access = $a") ++ fr"where id = $id").update.run.void
  )

  def setPrefs(uid: UUID, id: UUID, p: CalendarPrefReq): IO[Unit] = tx {
    val sets = List(p.color.map(c => fr"color = ${Option(c).filter(_.nonEmpty)}"), p.visible.map(v => fr"visible = $v")).flatten
    if sets.isEmpty then ().pure[ConnectionIO]
    else (fr"update calendar_member" ++ Sql.set(sets) ++ fr"where calendar_id = $id and user_id = $uid").update.run.void
  }

  def delete(id: UUID): IO[Unit] = tx(sql"delete from calendar where id = $id and not is_primary".update.run.void)

  def members(id: UUID): IO[List[MemberDto]] = tx(
    (fr"select u.id, u.name, u.email," ++ UserCols.avatar("u") ++ fr""", m.role from calendar_member m join app_user u on u.id = m.user_id
          where m.calendar_id = $id and m.role <> 'team' order by (m.role = 'owner') desc, u.name""")
      .query[(UUID, String, Option[String], Option[String], String)].map((i, n, e, a, r) => MemberDto(UserBrief(i, n, e, a), r)).to[List]
  )

  def setMember(id: UUID, uid: UUID, role: String): IO[Unit] = tx(
    sql"""insert into calendar_member (calendar_id, user_id, role) values ($id, $uid, $role)
          on conflict (calendar_id, user_id) do update set role = excluded.role where calendar_member.role <> 'owner'""".update.run.void
  )

  def removeMember(id: UUID, uid: UUID): IO[Unit] = tx(
    sql"delete from calendar_member where calendar_id = $id and user_id = $uid and role <> 'owner'".update.run.void
  )

  /**
   * ほかの人がチームに公開しているカレンダー. q は持ち主の名前・メールアドレスかカレンダーの名前（空ならすべて）.
   * 持ち主の名前の順、同じ人ならメインのカレンダーを先に.
   */
  def team(uid: UUID, q: String, limit: Int): IO[List[TeamCalendarRow]] = tx {
    val like = Sql.like(q)
    val find = if q.trim.isEmpty then Fragment.empty else fr"and (u.name ilike $like or u.email ilike $like or c.name ilike $like)"
    (fr"""select c.id, c.owner_id, c.name, c.description, c.color, c.is_primary, c.team_access, m.user_id is not null
          from calendar c join app_user u on u.id = c.owner_id and not u.disabled
          left join calendar_member m on m.calendar_id = c.id and m.user_id = $uid
          where c.team_access <> 'none' and c.owner_id <> $uid""" ++ find ++
      fr"order by u.name, c.is_primary desc, c.name limit $limit").query[TeamCalendarRow].to[List]
  }

  /**
   * チームに公開されたカレンダーを自分の一覧に加える（公開されていなければ false. 共有されていればそのまま）.
   * 自分の画面での色は、一覧でまだ使っていない色にする（同じ既定の色ばかりで見分けられなくならないように）.
   */
  def addTeam(uid: UUID, id: UUID): IO[Boolean] = tx(
    sql"""select coalesce(m.color, c.color) from calendar_member m join calendar c on c.id = m.calendar_id where m.user_id = $uid"""
      .query[String].to[List].flatMap { used =>
        val order = Palette.colors.drop(4) ++ Palette.colors.take(4) // 若葉色から（赤は警告に見えるので後に）
        val free  = order.map(_._1).filterNot(c => used.exists(_.equalsIgnoreCase(c)))
        val color = free.headOption.getOrElse(order(used.size % order.size)._1)
        sql"""insert into calendar_member (calendar_id, user_id, role, color)
              select id, $uid, 'team', case when lower(color) = lower($color) then null else $color end
              from calendar where id = $id and team_access <> 'none' and owner_id <> $uid
              on conflict do nothing""".update.run
      }.flatMap(n =>
      if n > 0 then true.pure[ConnectionIO]
      else sql"select exists(select 1 from calendar where id = $id and team_access <> 'none' and owner_id <> $uid)".query[Boolean].unique)
  )

  /** 公開リンクの範囲を変える. newToken があれば秘密の値をそれにする（なければ今の値. 公開していなければ公開しない）. */
  def setPublic(id: UUID, access: String, newToken: Option[String]): IO[Unit] = tx(
    sql"update calendar set public_access = $access, public_token = coalesce($newToken, public_token) where id = $id".update.run.void
  )

  def stopPublic(id: UUID): IO[Unit] = tx(sql"update calendar set public_token = null where id = $id".update.run.void)

  /** 公開リンクの秘密の値からカレンダー（持ち主はメールアドレスを除く）. */
  def byPublicToken(token: String): IO[Option[PublicCalendarDto]] = tx(
    sql"""select c.id, c.name, c.description, c.color, u.id, u.name, c.public_access from calendar c join app_user u on u.id = c.owner_id
          where c.public_token = $token"""
      .query[(UUID, String, String, String, UUID, String, String)].map((i, n, d, c, ui, un, a) => PublicCalendarDto(i, n, d, c, UserBrief(ui, un, None, None), a))
      .option
  )
