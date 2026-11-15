package skuaka.core

import skuaka.shared.Recurrence

import java.nio.charset.StandardCharsets
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId, ZoneOffset}

/** iCalendar（RFC 5545）の書き出しに使う予定. */
final case class IcsEvent(uid: String, title: String, description: String, location: String, span: Span, zone: ZoneId,
    rule: Option[Recurrence], exdates: List[String], updatedAt: Instant)

/** iCalendar の書き出し（カレンダーアプリから購読できる）. */
object Ics:
  private val utc   = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss'Z'").withZone(ZoneOffset.UTC)
  private val local = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss")
  private val date  = DateTimeFormatter.BASIC_ISO_DATE

  def escape(s: String): String =
    s.replace("\\", "\\\\").replace(";", "\\;").replace(",", "\\,").replace("\r\n", "\\n").replace("\n", "\\n").replace("\r", "")

  /** 75 オクテットごとに折り返す（マルチバイト文字の途中では切らない）. */
  def fold(line: String): String =
    val out = new StringBuilder
    var len = 0
    line.codePoints().forEach { cp =>
      val ch = new String(Character.toChars(cp))
      val n  = ch.getBytes(StandardCharsets.UTF_8).length
      if len + n > 75 then
        out.append("\r\n ")
        len = 1
      out.append(ch)
      len += n
    }
    out.toString

  private def time(name: String, at: Instant, zone: ZoneId): String =
    s"$name;TZID=${zone.getId}:${local.format(at.atZone(zone).toLocalDateTime)}"

  def event(e: IcsEvent, stamp: Instant): List[String] =
    val times = e.span match
      case Span.Timed(s, en) => List(time("DTSTART", s, e.zone), time("DTEND", en, e.zone))
      case Span.AllDay(s, en) => List(s"DTSTART;VALUE=DATE:${date.format(s)}", s"DTEND;VALUE=DATE:${date.format(en.plusDays(1))}")
    val ex = e.exdates.flatMap(k => Expand.occurrence(e.span, e.zone, k)).map {
      case Span.Timed(s, _)  => time("EXDATE", s, e.zone)
      case Span.AllDay(s, _) => s"EXDATE;VALUE=DATE:${date.format(s)}"
    }
    List("BEGIN:VEVENT", s"UID:${e.uid}", s"DTSTAMP:${utc.format(stamp)}", s"LAST-MODIFIED:${utc.format(e.updatedAt)}") ++
      times ++
      e.rule.map(r => s"RRULE:${r.toRrule}").toList ++
      ex ++
      List(s"SUMMARY:${escape(e.title)}") ++
      Option.when(e.description.nonEmpty)(s"DESCRIPTION:${escape(e.description)}").toList ++
      Option.when(e.location.nonEmpty)(s"LOCATION:${escape(e.location)}").toList ++
      List("END:VEVENT")

  def calendar(name: String, events: List[IcsEvent], stamp: Instant): String =
    val lines = List("BEGIN:VCALENDAR", "VERSION:2.0", "PRODID:-//skuaka//skuaka//JA", "CALSCALE:GREGORIAN", "METHOD:PUBLISH",
      s"X-WR-CALNAME:${escape(name)}") ++ events.flatMap(event(_, stamp)) ++ List("END:VCALENDAR")
    lines.map(fold).mkString("", "\r\n", "\r\n")
