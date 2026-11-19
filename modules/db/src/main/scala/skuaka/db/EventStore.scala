package skuaka.db

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import skuaka.shared.*

import java.time.Instant
import java.util.UUID

/**
 * 自分から見た予定（カレンダーの権限・招待への返事・ゲストの人数）.
 * busy は予定あり・なしだけ見える予定で、題名・説明・場所・色・通知・ゲストの人数を伏せてある（招待されていれば伏せない）.
 */
final case class VisibleEvent(
  row: EventRow,
  role: Option[String],
  response: Option[String],
  guests: Int) {
  def busy: Boolean = role.contains(CalRole.Busy) && response.isEmpty
}

object VisibleEvent {
  def of(
    row: EventRow,
    role: Option[String],
    response: Option[String],
    guests: Int
  ): VisibleEvent = {
    val v = VisibleEvent(row, role, response, guests)
    if !v.busy then v
    else
      v.copy(
        row = row.copy(title = TeamAccess.BusyTitle, description = "", location = "", color = None, reminders = Nil),
        guests = 0
      )
  }
}

/** 予定・ゲスト. */
final class EventStore(
  xa: Transactor[IO]) extends TxStore(xa) {

  private val cols =
    fr"""e.id, e.calendar_id, e.organizer_id, e.title, e.description, e.location, e.all_day, e.start_at, e.end_at,
                          e.start_date, e.end_date, e.timezone, e.color, e.rrule, e.exdates, e.reminders, e.version, e.updated_at"""

  /**
   * 自分に見える予定（カレンダーのメンバーか、ゲスト）のうち cond（別名 x）に合うもの.
   * 2つの経路を union で先に絞るので、カレンダー・ゲストの索引が使える（OR の外部結合だと event 全体を読む）.
   * details なら、予定あり・なししか見えないカレンダーの予定を除く（題名などで探すとき）.
   */
  private def visible(
    uid: UUID,
    cond: Fragment,
    tail: Fragment = Fragment.empty,
    details: Boolean = false
  ): ConnectionIO[List[VisibleEvent]] = {
    val member = if details then fr"(cm.role <> 'team' or cc.team_access = 'view')" else Member.active("cc", "cm")
    (fr"select" ++ cols ++ fr"," ++ Member.eventRole("c", "m") ++
      fr""", a.response, (select count(*)::int from event_attendee g where g.event_id = e.id)
         from event e
         join calendar c on c.id = e.calendar_id
         left join calendar_member m on m.calendar_id = e.calendar_id and m.user_id = $uid
         left join event_attendee a on a.event_id = e.id and a.user_id = $uid
         where e.id in (
           select x.id from event x join calendar_member cm on cm.calendar_id = x.calendar_id and cm.user_id = $uid
             join calendar cc on cc.id = x.calendar_id where""" ++ member ++ fr"and" ++ cond ++
      fr"union select x.id from event x join event_attendee xa on xa.event_id = x.id and xa.user_id = $uid where" ++
      cond ++ fr")" ++ tail)
      .query[(EventRow, Option[String], Option[String], Int)].map(VisibleEvent.of).to[List]
  }

  /** [from, to) に重なりうる予定（別名 a. 繰り返しの展開は呼び出し側で行う. 終日の予定のタイムゾーンの差のため1日広く取る）. */
  private def overlaps(
    a: String,
    from: Instant,
    to: Instant
  ): Fragment =
    Fragment.const(s"$a.range_start <") ++ fr"${to.plusSeconds(86400)} and (" ++
      Fragment.const(s"$a.range_end is null or $a.range_end >") ++
      fr"${from.minusSeconds(86400)})"

  def window(
    uid: UUID,
    from: Instant,
    to: Instant
  ): IO[List[VisibleEvent]] = tx(visible(uid, overlaps("x", from, to)))

  /** 1つのカレンダーの [from, to) に重なりうる予定（公開リンクで見るとき）. */
  def calendarWindow(
    calendarId: UUID,
    from: Instant,
    to: Instant
  ): IO[List[EventRow]] = tx(
    (fr"select" ++ cols ++ fr"from event e where e.calendar_id = $calendarId and" ++ overlaps("e", from, to)).query[
      EventRow
    ].to[List]
  )

  def visibleOne(
    uid: UUID,
    id: UUID
  ): IO[Option[VisibleEvent]] = tx(visible(uid, fr"x.id = $id").map(_.headOption))

  /** タイトル・場所・説明で探す（新しい順）. */
  def search(
    uid: UUID,
    q: String,
    limit: Int
  ): IO[List[VisibleEvent]] = tx {
    val like = Sql.like(q)
    visible(
      uid,
      fr"(x.title ilike $like or x.location ilike $like or x.description ilike $like)",
      fr"order by e.range_start desc limit $limit",
      details = true
    )
  }

  def get(
    id: UUID
  ): IO[Option[EventRow]] = tx((fr"select" ++ cols ++ fr"from event e where e.id = $id").query[EventRow].option)

  def forCalendar(
    calendarId: UUID
  ): IO[List[EventRow]] = tx(
    (fr"select" ++ cols ++ fr"from event e where e.calendar_id = $calendarId order by e.range_start").query[
      EventRow
    ].to[List]
  )

  def attendees(
    id: UUID
  ): IO[List[AttendeeDto]] = tx(
    (fr"select u.id, u.name, u.email," ++ UserCols.avatar("u") ++
      fr""", a.response from event_attendee a join app_user u on u.id = a.user_id
          where a.event_id = $id order by u.name""")
      .query[(UUID, String, Option[String], Option[String], String)].map(
        (
          i,
          n,
          e,
          av,
          r
        ) => AttendeeDto(UserBrief(i, n, e, av), r)
      ).to[List]
  )

  private def rangeOf(
    r: EventRow
  ): (Instant, Option[Instant]) = EventRow.range(r.span, r.zone, r.recurrence)

  /** ゲストを入れ替える（残る人の返事はそのまま）. */
  private def setAttendees(
    id: UUID,
    users: List[UUID]
  ): ConnectionIO[Unit] = {
    val us = users.distinct
    sql"delete from event_attendee where event_id = $id and not (user_id = any(${us.toArray}))".update.run *>
      us.traverse_(u =>
        sql"insert into event_attendee (event_id, user_id) values ($id, $u) on conflict do nothing".update.run
      )
  }

  def create(
    r: EventRow,
    attendees: List[UUID]
  ): IO[Unit] = tx(createC(r, attendees))

  /** 変更する（version が違えば false. 別の人が先に変えた）. */
  def update(
    r: EventRow,
    attendees: Option[List[UUID]],
    expectedVersion: Option[Int]
  ): IO[Boolean] = tx(updateC(r, attendees, expectedVersion))

  /** 繰り返しを切った元の予定を書き、切ったところからの新しい予定を作る（1つのトランザクションで）. */
  def split(
    cut: EventRow,
    next: EventRow,
    attendees: List[UUID]
  ): IO[Unit] = tx(updateC(cut, None, None) *> createC(next, attendees))

  private def createC(
    r: EventRow,
    attendees: List[UUID]
  ): ConnectionIO[Unit] = {
    val (rs, re) = rangeOf(r)
    sql"""insert into event (id, calendar_id, organizer_id, title, description, location, all_day, start_at, end_at, start_date, end_date,
            timezone, color, rrule, exdates, reminders, range_start, range_end)
          values (${r.id}, ${r.calendarId}, ${r.organizerId}, ${r.title}, ${r.description}, ${r.location}, ${r.allDay}, ${r.startAt}, ${r.endAt},
            ${r.startDate}, ${r.endDate}, ${r.timezone}, ${r.color}, ${r.rrule}, ${r.exdates.toArray}, ${r.reminders.toArray}, $rs, $re)""".update.run *>
      setAttendees(r.id, attendees)
  }

  private def updateC(
    r: EventRow,
    attendees: Option[List[UUID]],
    expectedVersion: Option[Int]
  ): ConnectionIO[Boolean] = {
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

  def delete(
    id: UUID
  ): IO[Unit] = tx(sql"delete from event where id = $id".update.run.void)

  /**
   * 取り込んだ予定を書く（元の UID ごとに、なければ作り、あれば変わったところだけ上書きする. 1つのトランザクションで）.
   * removeMissing なら、このカレンダーの取り込んだ予定のうち rows にないものを消す（購読で元から消えた予定）.
   * 取り込みで作った予定にはゲストを付けない. 戻り値は (作った数, 変えた数, 消した数).
   */
  def importEvents(
    calendarId: UUID,
    rows: List[(String, EventRow)],
    removeMissing: Boolean
  ): IO[(Int, Int, Int)] = tx {
    val upserts = rows.traverse {
      (
        uid,
        r
      ) =>
        val (rs, re) = rangeOf(r)
        sql"""insert into event as e (id, calendar_id, organizer_id, title, description, location, all_day, start_at, end_at, start_date, end_date,
              timezone, color, rrule, exdates, reminders, range_start, range_end, source_uid)
            values (${r.id}, $calendarId, ${r.organizerId}, ${r.title}, ${r.description}, ${r.location}, ${r.allDay}, ${r.startAt}, ${r.endAt},
              ${r.startDate}, ${r.endDate}, ${r.timezone}, ${r.color}, ${r.rrule}, ${r.exdates.toArray}, ${r.reminders.toArray}, $rs, $re, $uid)
            on conflict (calendar_id, source_uid) where source_uid is not null do update set
              title = excluded.title, description = excluded.description, location = excluded.location, all_day = excluded.all_day,
              start_at = excluded.start_at, end_at = excluded.end_at, start_date = excluded.start_date, end_date = excluded.end_date,
              timezone = excluded.timezone, rrule = excluded.rrule, exdates = excluded.exdates,
              range_start = excluded.range_start, range_end = excluded.range_end, version = e.version + 1, updated_at = now()
            where (e.title, e.description, e.location, e.all_day, e.start_at, e.end_at, e.start_date, e.end_date, e.timezone, e.rrule, e.exdates)
              is distinct from (excluded.title, excluded.description, excluded.location, excluded.all_day, excluded.start_at, excluded.end_at,
              excluded.start_date, excluded.end_date, excluded.timezone, excluded.rrule, excluded.exdates)
            returning (xmax = 0)""".query[Boolean].option
    }
    val remove =
      if !removeMissing then 0.pure[ConnectionIO]
      else sql"""delete from event where calendar_id = $calendarId and source_uid is not null
                 and not (source_uid = any(${rows.map(_._1).toArray}))""".update.run
    for {
      rs <- upserts
      n <- remove
    } yield (rs.count(_.contains(true)), rs.count(_.contains(false)), n)
  }

  def rsvp(
    id: UUID,
    uid: UUID,
    response: String
  ): IO[Boolean] = tx(
    sql"update event_attendee set response = $response where event_id = $id and user_id = $uid".update.run.map(_ > 0)
  )

  /** ゲストから外れる（招待された予定を自分のカレンダーから消す）. */
  def leave(
    id: UUID,
    uid: UUID
  ): IO[Unit] = tx(sql"delete from event_attendee where event_id = $id and user_id = $uid".update.run.void)

  // ---------------------------------------------------------------- リマインダー（プッシュ通知）・メトリクス

  /** すべてのカレンダーの [from, to) に重なりうる予定（timedWithReminders なら、通知の付いた時刻のある予定だけ）. 繰り返しの展開は呼び出し側で行う. */
  def overlapping(
    from: Instant,
    to: Instant,
    timedWithReminders: Boolean = false
  ): IO[List[EventRow]] = tx(
    (fr"select" ++ cols ++ fr"from event e where" ++ overlaps("e", from, to) ++
      (if timedWithReminders then fr"and not e.all_day and cardinality(e.reminders) > 0" else Fragment.empty)).query[
      EventRow
    ].to[List]
  )

  /**
   * リマインダーを受け取る人（ユーザーの ID とタイムゾーン）: カレンダーの所有者・共有された人（チームへの公開で加えた人は除く）と、
   * ゲスト. 欠席と返事した人・無効なユーザーは除く.
   */
  def reminderTargets(
    id: UUID,
    calendarId: UUID
  ): IO[List[(UUID, String)]] = tx(
    sql"""select u.id, u.timezone from app_user u
          where not u.disabled and u.id in (
            select m.user_id from calendar_member m where m.calendar_id = $calendarId and m.role in ('owner', 'edit', 'view')
            union select a.user_id from event_attendee a where a.event_id = $id)
          and not exists (select 1 from event_attendee d where d.event_id = $id and d.user_id = u.id and d.response = 'declined')"""
      .query[(UUID, String)].to[List]
  )

  /** リマインダーを送った印を付ける. 初めてなら true（ほかのノードが先に付けていれば false. 送らない）. */
  def markReminder(
    id: UUID,
    occurrence: String,
    minutes: Int
  ): IO[Boolean] = tx(
    sql"insert into reminder_sent (event_id, occurrence, minutes) values ($id, $occurrence, $minutes) on conflict do nothing".update.run.map(
      _ > 0
    )
  )

  /** 古いリマインダーの送信の記録を消す（2日より前）. */
  def purgeReminders: IO[Unit] =
    tx(sql"delete from reminder_sent where sent_at < now() - interval '2 days'".update.run.void)
}
