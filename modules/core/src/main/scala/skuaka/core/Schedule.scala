package skuaka.core

import skuaka.shared.Recurrence

import java.time.*
import java.time.temporal.{ChronoUnit, TemporalAdjusters}
import scala.util.Try

/** 予定の期間. 時刻のある予定は日時（終わりを含まない）、終日の予定は日付（終わりの日を含む）. */
enum Span:
  case Timed(start: Instant, end: Instant)
  case AllDay(start: LocalDate, end: LocalDate)

  /** 繰り返しの1回を表すキー（その回の元の開始）. */
  def key: String = this match
    case Timed(s, _)  => s.toString
    case AllDay(s, _) => s.toString

  /** 表示するタイムゾーンでの、開始と終わり（終わりを含まない）の日時. */
  def bounds(view: ZoneId): (Instant, Instant) = this match
    case Timed(s, e)  => (s, e)
    case AllDay(s, e) => (s.atStartOfDay(view).toInstant, e.plusDays(1).atStartOfDay(view).toInstant)

  def startText: String = this match
    case Timed(s, _)  => s.toString
    case AllDay(s, _) => s.toString

  def endText: String = this match
    case Timed(_, e)  => e.toString
    case AllDay(_, e) => e.toString

object Span:
  /** API の文字列から作る（終日なら yyyy-MM-dd、そうでなければ ISO 8601 の日時）. */
  def parse(allDay: Boolean, start: String, end: String): Either[String, Span] =
    if allDay then
      (Try(LocalDate.parse(start.take(10))).toOption, Try(LocalDate.parse(end.take(10))).toOption) match
        case (Some(s), Some(e)) if !e.isBefore(s) => Right(AllDay(s, e))
        case (Some(_), Some(_))                   => Left("終わりの日は開始の日より後にしてください")
        case _                                    => Left("日付は yyyy-MM-dd で指定してください")
    else
      (Time.instant(start), Time.instant(end)) match
        case (Some(s), Some(e)) if e.isAfter(s) => Right(Timed(s, e))
        case (Some(s), Some(e)) if e == s       => Right(Timed(s, e.plusSeconds(1800)))
        case (Some(_), Some(_))                 => Left("終わりの日時は開始の日時より後にしてください")
        case _                                  => Left("日時は ISO 8601 で指定してください")

object Time:
  def instant(s: String): Option[Instant] =
    Try(Instant.parse(s)).orElse(Try(OffsetDateTime.parse(s).toInstant)).toOption

  def zone(s: String): Option[ZoneId] = Try(ZoneId.of(s)).toOption

/** 繰り返しの展開. */
object Expand:

  /** 1つの予定の繰り返しのうち、[from, to) に重なる回（表示のタイムゾーン view で終日の予定の日付を日時にする）. */
  def occurrences(span: Span, zone: ZoneId, rule: Option[Recurrence], exdates: Set[String], from: Instant, to: Instant,
      view: ZoneId, limit: Int = 2000): List[Span] =
    rule match
      case None =>
        val (s, e) = span.bounds(view)
        if s.isBefore(to) && e.isAfter(from) then List(span) else Nil
      case Some(r) =>
        // 期間より前の回は飛ばす（長い予定・タイムゾーンの差で重なりうる分は余分に戻る）
        val (s, e) = span.bounds(zone)
        val back   = Duration.between(s, e).toDays + 2
        all(span, zone, r, after = Some(from.atZone(view).toLocalDate.minusDays(back)))
          .takeWhile(o => o.bounds(view)._1.isBefore(to))
          .filter(o => o.bounds(view)._2.isAfter(from) && !exdates.contains(o.key))
          .take(limit)
          .toList

  /** 繰り返しの最初の回の日付. */
  def startDate(span: Span, zone: ZoneId): LocalDate = span match
    case Span.Timed(s, _)  => s.atZone(zone).toLocalDate
    case Span.AllDay(s, _) => s

  /** 繰り返しの回のキー（その回の元の開始）の日付. */
  def keyDate(span: Span, zone: ZoneId, key: String): Option[LocalDate] = span match
    case Span.Timed(_, _)  => Time.instant(key).map(_.atZone(zone).toLocalDate)
    case Span.AllDay(_, _) => Try(LocalDate.parse(key)).toOption

  /** キーで指定した回の期間. */
  def occurrence(span: Span, zone: ZoneId, key: String): Option[Span] = keyDate(span, zone, key).map(at(span, zone, _))

  /**
   * 繰り返しのすべての回（終わりのない繰り返しでは無限に続く）.
   * after を渡すと、その日より前の期間を計算せずに飛ばす（回数の指定があるときは数えるため飛ばさない）. その日より前の回が少し混ざることはある.
   */
  def all(span: Span, zone: ZoneId, r: Recurrence, after: Option[LocalDate] = None): Iterator[Span] =
    val start = startDate(span, zone)
    val until = r.until.flatMap(u => Try(LocalDate.parse(u)).toOption)
    val skip  = after.filter(_ => r.count.isEmpty).fold(0L)(firstPeriod(start, r, _))
    val dates = candidateDates(start, r, skip).takeWhile(d => until.forall(u => !d.isAfter(u)))
    val counted = r.count.fold(dates)(dates.take)
    counted.map(d => at(span, zone, d))

  /** 同じ時刻・長さで、別の日に移した回. */
  def at(span: Span, zone: ZoneId, d: LocalDate): Span = span match
    case Span.Timed(s, e) =>
      val local = s.atZone(zone)
      val ns    = ZonedDateTime.of(d, local.toLocalTime, zone).toInstant
      Span.Timed(ns, ns.plus(Duration.between(s, e)))
    case Span.AllDay(s, e) =>
      Span.AllDay(d, d.plusDays(ChronoUnit.DAYS.between(s, e)))

  /** 繰り返しの日付（開始の日以降、昇順）. 期間の数は大きく制限する（壊れた繰り返しで止まらなくならないように）. */
  private def candidateDates(start: LocalDate, r: Recurrence, firstK: Long): Iterator[LocalDate] =
    val n     = math.max(1, r.interval)
    val days  = r.byDay.flatMap(Recurrence.parseDay)
    val steps = Iterator.range(firstK.toInt, firstK.toInt + 100000)
    val perPeriod: Int => List[LocalDate] = r.freq match
      case Recurrence.Daily => k => List(start.plusDays(k.toLong * n))
      case Recurrence.Weekly =>
        val dows   = (if days.isEmpty then List(start.getDayOfWeek.getValue) else days.map(_._2)).distinct.sorted
        val week0  = monday(start)
        k => dows.map(d => week0.plusWeeks(k.toLong * n).plusDays(d - 1L))
      case Recurrence.Monthly =>
        val first = start.withDayOfMonth(1)
        k =>
          val month = first.plusMonths(k.toLong * n)
          if days.isEmpty then
            Option.when(start.getDayOfMonth <= month.lengthOfMonth)(month.withDayOfMonth(start.getDayOfMonth)).toList
          else days.flatMap((pos, dow) => inMonth(month, pos, dow)).distinct.sorted
      case _ => // YEARLY
        k =>
          val y = start.getYear + k * n
          Try(LocalDate.of(y, start.getMonthValue, start.getDayOfMonth)).toOption.toList
    steps.flatMap(k => perPeriod(k).filterNot(_.isBefore(start)))

  private def monday(d: LocalDate): LocalDate = d.`with`(TemporalAdjusters.previousOrSame(DayOfWeek.MONDAY))

  /** after を含む期間の1つ前の期間の番号（0 から. 繰り返しの間隔の単位）. */
  private def firstPeriod(start: LocalDate, r: Recurrence, after: LocalDate): Long =
    val n = math.max(1, r.interval).toLong
    val periods = r.freq match
      case Recurrence.Daily   => ChronoUnit.DAYS.between(start, after)
      case Recurrence.Weekly  => ChronoUnit.WEEKS.between(monday(start), monday(after))
      case Recurrence.Monthly => ChronoUnit.MONTHS.between(start.withDayOfMonth(1), after.withDayOfMonth(1))
      case _                  => (after.getYear - start.getYear).toLong
    math.max(0L, periods / n - 1)

  /** 月の中の曜日（pos が None ならその曜日のすべて、2 なら第2、-1 なら最終）. */
  private def inMonth(month: LocalDate, pos: Option[Int], dow: Int): List[LocalDate] =
    val d = DayOfWeek.of(dow)
    pos match
      case None =>
        val first = month.`with`(TemporalAdjusters.firstInMonth(d))
        Iterator.iterate(first)(_.plusWeeks(1)).takeWhile(_.getMonth == month.getMonth).toList
      case Some(p) if p > 0 =>
        val x = month.`with`(TemporalAdjusters.dayOfWeekInMonth(p, d))
        Option.when(x.getMonth == month.getMonth)(x).toList
      case Some(p) =>
        val x = month.`with`(TemporalAdjusters.lastInMonth(d)).minusWeeks((-p - 1).toLong)
        Option.when(x.getMonth == month.getMonth)(x).toList

  /** 「これ以降」の変更・削除のため、その回の前の日で繰り返しを終わらせる（回数の指定は日付に置き換える）. */
  def endBefore(span: Span, zone: ZoneId, r: Recurrence, occurrenceKey: String): Recurrence =
    keyDate(span, zone, occurrenceKey).fold(r)(d => r.copy(until = Some(d.minusDays(1).toString), count = None))
