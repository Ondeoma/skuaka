package skuaka.core

import skuaka.shared.Recurrence

import java.time.*
import java.time.temporal.{ChronoUnit, TemporalAdjusters}
import scala.util.Try

/** 繰り返しの展開. */
object Expand {

  /** 1つの予定の繰り返しのうち、[from, to) に重なる回（表示のタイムゾーン view で終日の予定の日付を日時にする）. */
  def occurrences(span: Span,
                  zone: ZoneId,
                  rule: Option[Recurrence],
                  exdates: Set[String],
                  from: Instant,
                  to: Instant,
                  view: ZoneId,
                  limit: Int = 2000): List[Span] =
    rule match {
      case None =>
        val (s, e) = span.bounds(view)
        if s.isBefore(to) && e.isAfter(from) then List(span) else Nil
      case Some(r) =>
        // 期間より前の回は飛ばす（長い予定・タイムゾーンの差で重なりうる分は余分に戻る）
        val (s, e) = span.bounds(zone)
        val back = Duration.between(s, e).toDays + 2
        all(span, zone, r, after = Some(from.atZone(view).toLocalDate.minusDays(back)))
          .takeWhile(o => o.bounds(view)._1.isBefore(to))
          .filter(o => o.bounds(view)._2.isAfter(from) && !exdates.contains(o.key))
          .take(limit)
          .toList
    }

  /** 繰り返しの最初の回の日付. */
  def startDate(span: Span, zone: ZoneId): LocalDate = span match {
    case Span.Timed(s, _) => s.atZone(zone).toLocalDate
    case Span.AllDay(s, _) => s
  }

  /** 繰り返しの回のキー（その回の元の開始）の日付. */
  def keyDate(span: Span, zone: ZoneId, key: String): Option[LocalDate] = span match {
    case Span.Timed(_, _) => Time.instant(key).map(_.atZone(zone).toLocalDate)
    case Span.AllDay(_, _) => Try(LocalDate.parse(key)).toOption
  }

  /** キーで指定した回の期間. */
  def occurrence(span: Span, zone: ZoneId, key: String): Option[Span] = keyDate(span, zone, key).map(at(span, zone, _))

  /**
   * 繰り返しのすべての回（終わりのない繰り返しでは無限に続く）.
   * after を渡すと、その日より前の期間を計算せずに飛ばす（回数の指定があるときは数えるため飛ばさない）. その日より前の回が少し混ざることはある.
   */
  def all(span: Span, zone: ZoneId, r: Recurrence, after: Option[LocalDate] = None): Iterator[Span] = {
    val start = startDate(span, zone)
    val until = r.until.flatMap(u => Try(LocalDate.parse(u)).toOption)
    val skip = after.filter(_ => r.count.isEmpty).fold(0L)(firstPeriod(start, r, _))
    val dates = candidateDates(start, r, skip).takeWhile(d => until.forall(u => !d.isAfter(u)))
    val counted = r.count.fold(dates)(dates.take)
    counted.map(d => at(span, zone, d))
  }

  /** 同じ時刻・長さで、別の日に移した回. */
  def at(span: Span, zone: ZoneId, d: LocalDate): Span = span match {
    case Span.Timed(s, e) =>
      val local = s.atZone(zone)
      val ns = ZonedDateTime.of(d, local.toLocalTime, zone).toInstant
      Span.Timed(ns, ns.plus(Duration.between(s, e)))
    case Span.AllDay(s, e) =>
      Span.AllDay(d, d.plusDays(ChronoUnit.DAYS.between(s, e)))
  }

  /**
   * 繰り返しの日付（開始の日以降、昇順）. 期間の数は大きく制限する（壊れた繰り返しで止まらなくならないように）.
   * RFC 5545 のとおり、期間（日・週・月・年）ごとに候補の日を作り、BYMONTH・BYMONTHDAY・BYDAY で広げるか絞り、最後に BYSETPOS で選ぶ.
   */
  private def candidateDates(start: LocalDate, r: Recurrence, firstK: Long): Iterator[LocalDate] = {
    val n = math.max(1, r.interval)
    val steps = Iterator.range(firstK.toInt, firstK.toInt + 100000)
    steps.flatMap(k => setPos(periodDates(start, r, k.toLong * n), r.bySetPos).filterNot(_.isBefore(start)))
  }

  /** BYSETPOS: 期間の候補（昇順）のうち、指定した番目のもの（1 は最初、-1 は最後）. */
  private def setPos(ds: List[LocalDate], pos: List[Int]): List[LocalDate] =
    if pos.isEmpty then ds
    else pos.flatMap(p => if p > 0 then ds.lift(p - 1) else ds.lift(ds.size + p)).distinct.sorted

  /** 曜日の指定（第何週か、ISO の曜日）. */
  private def days(r: Recurrence): List[(Option[Int], Int)] = r.byDay.flatMap(Recurrence.parseDay)

  /** 日にちの指定（-1 は末日）をその月の日付に. ない日（2月30日など）は飛ばす. */
  private def monthDays(month: LocalDate, ds: List[Int]): List[LocalDate] =
    ds.flatMap(d =>
      if d > 0 then Option.when(d <= month.lengthOfMonth)(month.withDayOfMonth(d))
      else
        Option.when(-d <= month.lengthOfMonth)(month.withDayOfMonth(month.lengthOfMonth + d + 1))
    )

  /** BYMONTH・BYMONTHDAY・BYDAY で絞る（期間を広げない項目. 毎日・毎週の繰り返しと、月の中の日にちを曜日で絞るとき）. */
  private def keep(d: LocalDate, r: Recurrence, monthDay: Boolean = true, dow: Boolean = true): Boolean =
    (r.byMonth.isEmpty || r.byMonth.contains(d.getMonthValue)) &&
      (!monthDay || r.byMonthDay.isEmpty || monthDays(d.withDayOfMonth(1), r.byMonthDay).contains(d)) &&
      (!dow || r.byDay.isEmpty || days(r).exists(_._2 == d.getDayOfWeek.getValue))

  /** 月の中の候補の日（日にち → 曜日で絞る. 日にちがなければ曜日で広げる. どちらもなければ開始と同じ日）. */
  private def inMonthDates(month: LocalDate, start: LocalDate, r: Recurrence): List[LocalDate] =
    if r.byMonthDay.nonEmpty then monthDays(month, r.byMonthDay).filter(keep(_, r, monthDay = false))
    else if r.byDay.nonEmpty then
      days(r).flatMap((pos, dow) => inMonth(month, pos, dow))
    else Option.when(start.getDayOfMonth <= month.lengthOfMonth)(month.withDayOfMonth(start.getDayOfMonth)).toList

  /** k 番目の期間（間隔を掛けたもの）の候補の日（昇順・重なりなし）. */
  private def periodDates(start: LocalDate, r: Recurrence, k: Long): List[LocalDate] = {
    val out = r.freq match {
      case Recurrence.Daily =>
        List(start.plusDays(k)).filter(keep(_, r))
      case Recurrence.Weekly =>
        val dows = (if r.byDay.isEmpty then List(start.getDayOfWeek.getValue) else days(r).map(_._2)).distinct
        dows.map(d => monday(start).plusWeeks(k).plusDays(d - 1L)).filter(keep(_, r, dow = false))
      case Recurrence.Monthly =>
        val month = start.withDayOfMonth(1).plusMonths(k)
        if r.byMonth.nonEmpty && !r.byMonth.contains(month.getMonthValue) then Nil else inMonthDates(month, start, r)
      case _ => // YEARLY
        val year = start.getYear + k.toInt
        if r.byMonth.nonEmpty then r.byMonth.flatMap(m => inMonthDates(LocalDate.of(year, m, 1), start, r))
        else if r.byMonthDay.nonEmpty then
          (1 to 12).toList.flatMap(m => inMonthDates(LocalDate.of(year, m, 1), start, r))
        else if r.byDay.nonEmpty then
          days(r).flatMap((pos, dow) => inYear(year, pos, dow))
        else Try(LocalDate.of(year, start.getMonthValue, start.getDayOfMonth)).toOption.toList
    }
    out.distinct.sorted
  }

  /** 年の中の曜日（pos が None ならその曜日のすべて、20 なら第20、-1 なら最終）. */
  private def inYear(year: Int, pos: Option[Int], dow: Int): List[LocalDate] = {
    val d = DayOfWeek.of(dow)
    val first = LocalDate.of(year, 1, 1).`with`(TemporalAdjusters.nextOrSame(d))
    val all = Iterator.iterate(first)(_.plusWeeks(1)).takeWhile(_.getYear == year).toList
    pos.fold(all)(p => (if p > 0 then all.lift(p - 1) else all.lift(all.size + p)).toList)
  }

  private def monday(d: LocalDate): LocalDate = d.`with`(TemporalAdjusters.previousOrSame(DayOfWeek.MONDAY))

  /** after を含む期間の1つ前の期間の番号（0 から. 繰り返しの間隔の単位）. */
  private def firstPeriod(start: LocalDate, r: Recurrence, after: LocalDate): Long = {
    val n = math.max(1, r.interval).toLong
    val periods = r.freq match {
      case Recurrence.Daily => ChronoUnit.DAYS.between(start, after)
      case Recurrence.Weekly => ChronoUnit.WEEKS.between(monday(start), monday(after))
      case Recurrence.Monthly => ChronoUnit.MONTHS.between(start.withDayOfMonth(1), after.withDayOfMonth(1))
      case _ => (after.getYear - start.getYear).toLong
    }
    math.max(0L, periods / n - 1)
  }

  /** 月の中の曜日（pos が None ならその曜日のすべて、2 なら第2、-1 なら最終）. */
  private def inMonth(month: LocalDate, pos: Option[Int], dow: Int): List[LocalDate] = {
    val d = DayOfWeek.of(dow)
    pos match {
      case None =>
        val first = month.`with`(TemporalAdjusters.firstInMonth(d))
        Iterator.iterate(first)(_.plusWeeks(1)).takeWhile(_.getMonth == month.getMonth).toList
      case Some(p) if p > 0 =>
        val x = month.`with`(TemporalAdjusters.dayOfWeekInMonth(p, d))
        Option.when(x.getMonth == month.getMonth)(x).toList
      case Some(p) =>
        val x = month.`with`(TemporalAdjusters.lastInMonth(d)).minusWeeks((-p - 1).toLong)
        Option.when(x.getMonth == month.getMonth)(x).toList
    }
  }

  /** 「これ以降」の変更・削除のため、その回の前の日で繰り返しを終わらせる（回数の指定は日付に置き換える）. */
  def endBefore(span: Span, zone: ZoneId, r: Recurrence, occurrenceKey: String): Recurrence =
    keyDate(span, zone, occurrenceKey).fold(r)(d => r.copy(until = Some(d.minusDays(1).toString), count = None))
}
