package skuaka.db

import skuaka.core.*
import skuaka.shared.*

import java.time.{Instant, LocalDate, ZoneId, ZoneOffset}
import java.util.UUID

/** 予定の1行. */
final case class EventRow(id: UUID,
                          calendarId: UUID,
                          organizerId: Option[UUID],
                          title: String,
                          description: String,
                          location: String,
                          allDay: Boolean,
                          startAt: Option[Instant],
                          endAt: Option[Instant],
                          startDate: Option[LocalDate],
                          endDate: Option[LocalDate],
                          timezone: String,
                          color: Option[String],
                          rrule: Option[String],
                          exdates: List[String],
                          reminders: List[Int],
                          version: Int,
                          updatedAt: Instant) {

  def span: Span =
    if allDay then Span.AllDay(startDate.get, endDate.get) else Span.Timed(startAt.get, endAt.get)

  def zone: ZoneId = Time.zone(timezone).getOrElse(ZoneOffset.UTC)

  def recurrence: Option[Recurrence] = rrule.flatMap(Recurrence.parse)
}

object EventRow {

  /** 期間で探すための、最初の回の始まりと最後の回の終わり（終わりのない繰り返しは None）. */
  def range(span: Span, zone: ZoneId, rule: Option[Recurrence]): (Instant, Option[Instant]) = {
    val (s, e) = span.bounds(ZoneOffset.UTC)
    rule match {
      case None => (s, Some(e))
      case Some(r) if r.until.isDefined || r.count.isDefined =>
        // 回数の指定は最後の回まで数える（最大でも数千回）
        val last = Expand.all(span, zone, r).take(10000).foldLeft(Option.empty[Span])((_, x) => Some(x))
        (s, Some(last.fold(e)(_.bounds(ZoneOffset.UTC)._2)))
      case Some(_) => (s, None)
    }
  }
}
