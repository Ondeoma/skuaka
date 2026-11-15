package skuaka.db

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import skuaka.core.*
import skuaka.shared.*

import java.time.{Instant, LocalDate, ZoneId, ZoneOffset}
import java.util.UUID

/** 予定の1行. */
final case class EventRow(id: UUID, calendarId: UUID, organizerId: Option[UUID], title: String, description: String, location: String,
    allDay: Boolean, startAt: Option[Instant], endAt: Option[Instant], startDate: Option[LocalDate], endDate: Option[LocalDate],
    timezone: String, color: Option[String], rrule: Option[String], exdates: List[String], reminders: List[Int], version: Int,
    updatedAt: Instant):

  def span: Span =
    if allDay then Span.AllDay(startDate.get, endDate.get) else Span.Timed(startAt.get, endAt.get)

  def zone: ZoneId = Time.zone(timezone).getOrElse(ZoneOffset.UTC)

  def recurrence: Option[Recurrence] = rrule.flatMap(Recurrence.parse)

/**
 * 自分から見た予定（カレンダーの権限・招待への返事・ゲストの人数）.
 * busy は予定あり・なしだけ見える予定で、題名・説明・場所・色・通知・ゲストの人数を伏せてある（招待されていれば伏せない）.
 */
final case class VisibleEvent(row: EventRow, role: Option[String], response: Option[String], guests: Int):
  def busy: Boolean = role.contains(CalRole.Busy) && response.isEmpty

object VisibleEvent:
  def of(row: EventRow, role: Option[String], response: Option[String], guests: Int): VisibleEvent =
    val v = VisibleEvent(row, role, response, guests)
    if !v.busy then v
    else v.copy(row = row.copy(title = TeamAccess.BusyTitle, description = "", location = "", color = None, reminders = Nil), guests = 0)

object EventRow:
  /** 期間で探すための、最初の回の始まりと最後の回の終わり（終わりのない繰り返しは None）. */
  def range(span: Span, zone: ZoneId, rule: Option[Recurrence]): (Instant, Option[Instant]) =
    val (s, e) = span.bounds(ZoneOffset.UTC)
    rule match
      case None => (s, Some(e))
      case Some(r) if r.until.isDefined || r.count.isDefined =>
        // 回数の指定は最後の回まで数える（最大でも数千回）
        val last = Expand.all(span, zone, r).take(10000).foldLeft(Option.empty[Span])((_, x) => Some(x))
        (s, Some(last.fold(e)(_.bounds(ZoneOffset.UTC)._2)))
      case Some(_) => (s, None)

/** 予定・ゲスト. */
final class EventStore(xa: Transactor[IO]) extends TxStore(xa):

  private val cols = fr"""e.id, e.calendar_id, e.organizer_id, e.title, e.description, e.location, e.all_day, e.start_at, e.end_at,
                          e.start_date, e.end_date, e.timezone, e.color, e.rrule, e.exdates, e.reminders, e.version, e.updated_at"""

  /**
   * 自分に見える予定（カレンダーのメンバーか、ゲスト）のうち cond（別名 x）に合うもの.
   * 2つの経路を union で先に絞るので、カレンダー・ゲストの索引が使える（OR の外部結合だと event 全体を読む）.
   * details なら、予定あり・なししか見えないカレンダーの予定を除く（題名などで探すとき）.
   */
  private def visible(uid: UUID, cond: Fragment, tail: Fragment = Fragment.empty, details: Boolean = false): ConnectionIO[List[VisibleEvent]] =
    val member = if details then fr"(cm.role <> 'team' or cc.team_access = 'view')" else Member.active("cc", "cm")
    (fr"select" ++ cols ++ fr"," ++ Member.role("c", "m") ++ fr""", a.response, (select count(*)::int from event_attendee g where g.event_id = e.id)
         from event e
         join calendar c on c.id = e.calendar_id
         left join calendar_member m on m.calendar_id = e.calendar_id and m.user_id = $uid
         left join event_attendee a on a.event_id = e.id and a.user_id = $uid
         where e.id in (
           select x.id from event x join calendar_member cm on cm.calendar_id = x.calendar_id and cm.user_id = $uid
             join calendar cc on cc.id = x.calendar_id where""" ++ member ++ fr"and" ++ cond ++
      fr"union select x.id from event x join event_attendee xa on xa.event_id = x.id and xa.user_id = $uid where" ++ cond ++ fr")" ++ tail)
      .query[(EventRow, Option[String], Option[String], Int)].map(VisibleEvent.of).to[List]

  /** [from, to) に重なりうる予定（別名 a. 繰り返しの展開は呼び出し側で行う. 終日の予定のタイムゾーンの差のため1日広く取る）. */
  private def overlaps(a: String, from: Instant, to: Instant): Fragment =
    Fragment.const(s"$a.range_start <") ++ fr"${to.plusSeconds(86400)} and (" ++ Fragment.const(s"$a.range_end is null or $a.range_end >") ++
      fr"${from.minusSeconds(86400)})"

  def window(uid: UUID, from: Instant, to: Instant): IO[List[VisibleEvent]] = tx(visible(uid, overlaps("x", from, to)))

  /** 1つのカレンダーの [from, to) に重なりうる予定（公開リンクで見るとき）. */
  def calendarWindow(calendarId: UUID, from: Instant, to: Instant): IO[List[EventRow]] = tx(
    (fr"select" ++ cols ++ fr"from event e where e.calendar_id = $calendarId and" ++ overlaps("e", from, to)).query[EventRow].to[List]
  )

  def visibleOne(uid: UUID, id: UUID): IO[Option[VisibleEvent]] = tx(visible(uid, fr"x.id = $id").map(_.headOption))

  /** タイトル・場所・説明で探す（新しい順）. */
  def search(uid: UUID, q: String, limit: Int): IO[List[VisibleEvent]] = tx {
    val like = Sql.like(q)
    visible(uid, fr"(x.title ilike $like or x.location ilike $like or x.description ilike $like)", fr"order by e.range_start desc limit $limit", details = true)
  }

  def get(id: UUID): IO[Option[EventRow]] = tx((fr"select" ++ cols ++ fr"from event e where e.id = $id").query[EventRow].option)

  def forCalendar(calendarId: UUID): IO[List[EventRow]] = tx(
    (fr"select" ++ cols ++ fr"from event e where e.calendar_id = $calendarId order by e.range_start").query[EventRow].to[List]
  )

  def attendees(id: UUID): IO[List[AttendeeDto]] = tx(
    sql"""select u.id, u.name, u.email, u.avatar_url, a.response from event_attendee a join app_user u on u.id = a.user_id
          where a.event_id = $id order by u.name"""
      .query[(UUID, String, Option[String], Option[String], String)].map((i, n, e, av, r) => AttendeeDto(UserBrief(i, n, e, av), r)).to[List]
  )

  private def rangeOf(r: EventRow): (Instant, Option[Instant]) = EventRow.range(r.span, r.zone, r.recurrence)

  /** ゲストを入れ替える（残る人の返事はそのまま）. */
  private def setAttendees(id: UUID, users: List[UUID]): ConnectionIO[Unit] =
    val us = users.distinct
    sql"delete from event_attendee where event_id = $id and not (user_id = any(${us.toArray}))".update.run *>
      us.traverse_(u => sql"insert into event_attendee (event_id, user_id) values ($id, $u) on conflict do nothing".update.run)

  def create(r: EventRow, attendees: List[UUID]): IO[Unit] = tx(createC(r, attendees))

  /** 変更する（version が違えば false. 別の人が先に変えた）. */
  def update(r: EventRow, attendees: Option[List[UUID]], expectedVersion: Option[Int]): IO[Boolean] = tx(updateC(r, attendees, expectedVersion))

  /** 繰り返しを切った元の予定を書き、切ったところからの新しい予定を作る（1つのトランザクションで）. */
  def split(cut: EventRow, next: EventRow, attendees: List[UUID]): IO[Unit] = tx(updateC(cut, None, None) *> createC(next, attendees))

  private def createC(r: EventRow, attendees: List[UUID]): ConnectionIO[Unit] = {
    val (rs, re) = rangeOf(r)
    sql"""insert into event (id, calendar_id, organizer_id, title, description, location, all_day, start_at, end_at, start_date, end_date,
            timezone, color, rrule, exdates, reminders, range_start, range_end)
          values (${r.id}, ${r.calendarId}, ${r.organizerId}, ${r.title}, ${r.description}, ${r.location}, ${r.allDay}, ${r.startAt}, ${r.endAt},
            ${r.startDate}, ${r.endDate}, ${r.timezone}, ${r.color}, ${r.rrule}, ${r.exdates.toArray}, ${r.reminders.toArray}, $rs, $re)""".update.run *>
      setAttendees(r.id, attendees)
  }

  private def updateC(r: EventRow, attendees: Option[List[UUID]], expectedVersion: Option[Int]): ConnectionIO[Boolean] = {
    val (rs, re) = rangeOf(r)
    val check = expectedVersion.fold(Fragment.empty)(v => fr"and version = $v")
    (fr"""update event set calendar_id = ${r.calendarId}, title = ${r.title}, description = ${r.description}, location = ${r.location},
            all_day = ${r.allDay}, start_at = ${r.startAt}, end_at = ${r.endAt}, start_date = ${r.startDate}, end_date = ${r.endDate},
            timezone = ${r.timezone}, color = ${r.color}, rrule = ${r.rrule}, exdates = ${r.exdates.toArray}, reminders = ${r.reminders.toArray},
            range_start = $rs, range_end = $re, version = version + 1, updated_at = now()
          where id = ${r.id}""" ++ check).update.run.flatMap { n =>
      if n == 0 then false.pure[ConnectionIO] else attendees.traverse_(setAttendees(r.id, _)).as(true)
    }
  }

  def delete(id: UUID): IO[Unit] = tx(sql"delete from event where id = $id".update.run.void)

  def rsvp(id: UUID, uid: UUID, response: String): IO[Boolean] = tx(
    sql"update event_attendee set response = $response where event_id = $id and user_id = $uid".update.run.map(_ > 0)
  )

  /** ゲストから外れる（招待された予定を自分のカレンダーから消す）. */
  def leave(id: UUID, uid: UUID): IO[Unit] = tx(sql"delete from event_attendee where event_id = $id and user_id = $uid".update.run.void)
