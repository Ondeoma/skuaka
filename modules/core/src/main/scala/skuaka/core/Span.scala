package skuaka.core

import java.time.*
import scala.util.Try

/** 予定の期間. 時刻のある予定は日時（終わりを含まない）、終日の予定は日付（終わりの日を含む）. */
enum Span {
  case Timed(
    start: Instant,
    end: Instant)
  case AllDay(
    start: LocalDate,
    end: LocalDate)

  /** 繰り返しの1回を表すキー（その回の元の開始）. */
  def key: String = this match {
    case Timed(s, _) => s.toString
    case AllDay(s, _) => s.toString
  }

  /** 表示するタイムゾーンでの、開始と終わり（終わりを含まない）の日時. */
  def bounds(view: ZoneId): (Instant, Instant) = this match {
    case Timed(s, e) => (s, e)
    case AllDay(s, e) => (s.atStartOfDay(view).toInstant, e.plusDays(1).atStartOfDay(view).toInstant)
  }

  def startText: String = this match {
    case Timed(s, _) => s.toString
    case AllDay(s, _) => s.toString
  }

  def endText: String = this match {
    case Timed(_, e) => e.toString
    case AllDay(_, e) => e.toString
  }
}

object Span {

  /** API の文字列から作る（終日なら yyyy-MM-dd、そうでなければ ISO 8601 の日時）. */
  def parse(allDay: Boolean, start: String, end: String): Either[String, Span] =
    if allDay then
      (Try(LocalDate.parse(start.take(10))).toOption, Try(LocalDate.parse(end.take(10))).toOption) match {
        case (Some(s), Some(e)) if !e.isBefore(s) => Right(AllDay(s, e))
        case (Some(_), Some(_)) => Left("終わりの日は開始の日より後にしてください")
        case _ => Left("日付は yyyy-MM-dd で指定してください")
      }
    else
      (Time.instant(start), Time.instant(end)) match {
        case (Some(s), Some(e)) if e.isAfter(s) => Right(Timed(s, e))
        case (Some(s), Some(e)) if e == s => Right(Timed(s, e.plusSeconds(1800)))
        case (Some(_), Some(_)) => Left("終わりの日時は開始の日時より後にしてください")
        case _ => Left("日時は ISO 8601 で指定してください")
      }
}
