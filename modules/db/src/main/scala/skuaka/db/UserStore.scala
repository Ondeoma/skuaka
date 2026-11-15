package skuaka.db

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import skuaka.shared.*

import java.time.Instant
import java.util.UUID

final case class User(id: UUID, sub: String, name: String, email: Option[String], avatarUrl: Option[String], timezone: String, disabled: Boolean):
  def me: MeDto        = MeDto(id, name, email, avatarUrl, timezone)
  def brief: UserBrief = me.brief

/** ユーザー・セッション・ログイン途中の状態. */
final class UserStore(xa: Transactor[IO]) extends TxStore(xa):

  private val cols = fr"id, sub, name, email, avatar_url, timezone, disabled"

  /** ログインのたびに作成・更新する（名前・メール・アイコンは IdP の値に合わせる）. */
  def upsert(sub: String, name: String, email: Option[String], avatar: Option[String]): IO[User] = tx(
    (fr"""insert into app_user (id, sub, name, email, avatar_url, last_login_at) values (${UUID.randomUUID()}, $sub, $name, $email, $avatar, now())
          on conflict (sub) do update set name = excluded.name, email = excluded.email,
            avatar_url = coalesce(excluded.avatar_url, app_user.avatar_url), last_login_at = now()
          returning""" ++ cols).query[User].unique
  )

  def find(id: UUID): IO[Option[User]] = tx(findC(id))

  def findC(id: UUID): ConnectionIO[Option[User]] = (fr"select" ++ cols ++ fr"from app_user where id = $id").query[User].option

  def briefs(ids: List[UUID]): ConnectionIO[Map[UUID, UserBrief]] =
    if ids.isEmpty then Map.empty[UUID, UserBrief].pure[ConnectionIO]
    else (fr"select" ++ cols ++ fr"from app_user where id = any(${ids.distinct.toArray})").query[User].to[List].map(_.map(u => u.id -> u.brief).toMap)

  /** 名前・メールで探す（共有・ゲストの候補）. */
  def search(q: String, limit: Int): IO[List[UserBrief]] = tx {
    val like = Sql.like(q)
    (fr"select" ++ cols ++ fr"from app_user where not disabled and (name ilike $like or email ilike $like) order by name limit $limit")
      .query[User].to[List].map(_.map(_.brief))
  }

  /** メールアドレス・ID で1人を決める（共有の相手の指定）. */
  def resolve(s: String): IO[Option[User]] = tx {
    val t = s.trim
    scala.util.Try(UUID.fromString(t)).toOption match
      case Some(id) => findC(id)
      case None => (fr"select" ++ cols ++ fr"from app_user where not disabled and lower(email) = lower($t) order by last_login_at desc nulls last limit 1")
          .query[User].option
  }

  def updateMe(id: UUID, name: Option[String], timezone: Option[String]): IO[Unit] = tx {
    val sets = List(name.map(_.trim).filter(_.nonEmpty).map(v => fr"name = $v"), timezone.map(v => fr"timezone = $v")).flatten
    if sets.isEmpty then ().pure[ConnectionIO] else (fr"update app_user" ++ Sql.set(sets) ++ fr"where id = $id").update.run.void
  }

  // ---------------------------------------------------------------- セッション

  def createSession(hash: Array[Byte], userId: UUID, idToken: Option[String], expires: Instant): IO[Unit] = tx(
    sql"insert into session (token_hash, user_id, id_token, expires_at) values ($hash, $userId, $idToken, $expires)".update.run.void
  )

  def findSession(hash: Array[Byte], now: Instant): IO[Option[User]] = tx(
    sql"select user_id from session where token_hash = $hash and expires_at > $now".query[UUID].option.flatMap {
      case None      => none[User].pure[ConnectionIO]
      case Some(uid) => findC(uid).map(_.filterNot(_.disabled))
    }
  )

  /** セッションを消し、IdP のログアウトに渡す ID トークンを返す. */
  def deleteSession(hash: Array[Byte]): IO[Option[String]] = tx(
    sql"delete from session where token_hash = $hash returning id_token".query[Option[String]].option.map(_.flatten)
  )

  def purgeExpired(now: Instant): IO[Unit] = tx(
    sql"delete from session where expires_at < $now".update.run *>
      sql"delete from login_state where created_at < now() - interval '1 hour'".update.run.void
  )

  // ---------------------------------------------------------------- API トークン

  private val tokenCols = fr"id, name, prefix, scope, created_at, expires_at, last_used_at"

  private def tokenDto(t: (UUID, String, String, String, Instant, Option[Instant], Option[Instant])): ApiTokenDto =
    ApiTokenDto(t._1, t._2, t._3, t._4, t._5.toString, t._6.map(_.toString), t._7.map(_.toString))

  def createToken(userId: UUID, name: String, prefix: String, hash: Array[Byte], scope: String, expires: Option[Instant]): IO[ApiTokenDto] = tx(
    (fr"insert into api_token (id, user_id, name, prefix, token_hash, scope, expires_at) values (${UUID.randomUUID()}, $userId, $name, $prefix, $hash, $scope, $expires) returning" ++ tokenCols)
      .query[(UUID, String, String, String, Instant, Option[Instant], Option[Instant])].unique.map(tokenDto)
  )

  /** 失効していないもの（期限切れも含む. 画面で期限切れと分かるように）. */
  def tokens(userId: UUID): IO[List[ApiTokenDto]] = tx(
    (fr"select" ++ tokenCols ++ fr"from api_token where user_id = $userId and revoked_at is null order by created_at desc")
      .query[(UUID, String, String, String, Instant, Option[Instant], Option[Instant])].to[List].map(_.map(tokenDto))
  )

  def revokeToken(userId: UUID, id: UUID): IO[Boolean] = tx(
    sql"update api_token set revoked_at = now() where id = $id and user_id = $userId and revoked_at is null".update.run.map(_ > 0)
  )

  /** 有効なトークンの持ち主とスコープ. 最後に使った日時は1分に1回だけ書く. */
  def findToken(hash: Array[Byte], now: Instant): IO[Option[(User, UUID, String)]] = tx(
    sql"""select id, user_id, scope from api_token
          where token_hash = $hash and revoked_at is null and (expires_at is null or expires_at > $now)"""
      .query[(UUID, UUID, String)].option.flatMap {
        case None => none[(User, UUID, String)].pure[ConnectionIO]
        case Some((id, uid, scope)) =>
          sql"update api_token set last_used_at = $now where id = $id and (last_used_at is null or last_used_at < $now - interval '1 minute')".update.run *>
            findC(uid).map(_.filterNot(_.disabled).map(u => (u, id, scope)))
      }
  )

  // ---------------------------------------------------------------- ログイン途中の状態（OIDC）

  def saveLoginState(state: String, verifier: String, nonce: String, returnTo: String): IO[Unit] = tx(
    sql"insert into login_state (state, verifier, nonce, return_to) values ($state, $verifier, $nonce, $returnTo)".update.run.void
  )

  /** 取り出して消す（1回だけ・15分以内）. */
  def takeLoginState(state: String): IO[Option[(String, String, String)]] = tx(
    sql"""delete from login_state where state = $state and created_at > now() - interval '15 minutes'
          returning verifier, nonce, return_to""".query[(String, String, String)].option
  )
