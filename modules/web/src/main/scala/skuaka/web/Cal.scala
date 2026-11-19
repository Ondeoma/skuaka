package skuaka.web

import scala.scalajs.js

/**
 * 日付・時刻の計算（画面のタイムゾーン = ブラウザのタイムゾーン）.
 * 日付は "yyyy-MM-dd" の文字列、日時は ISO 8601 の文字列で持つ（Scala.js で java.time を使わないため）.
 */
object Cal {

  val WeekNames: Vector[String] = Vector("日", "月", "火", "水", "木", "金", "土")

  def parse(
    d: String
  ): js.Date = new js.Date(d.take(4).toInt, d.slice(5, 7).toInt - 1, d.slice(8, 10).toInt)

  def iso(
    d: js.Date
  ): String = f"${d.getFullYear().toInt}%04d-${d.getMonth().toInt + 1}%02d-${d.getDate().toInt}%02d"

  def today(): String = iso(new js.Date())

  def year(
    d: String
  ): Int = d.take(4).toInt
  def month(
    d: String
  ): Int = d.slice(5, 7).toInt
  def day(
    d: String
  ): Int = d.slice(8, 10).toInt

  def addDays(
    d: String,
    n: Int
  ): String = iso(new js.Date(year(d), month(d) - 1, day(d) + n))

  /** n か月後（月末を超える日はその月の末日）. */
  def addMonths(
    d: String,
    n: Int
  ): String = {
    val first = new js.Date(year(d), month(d) - 1 + n, 1)
    val (y, m) = (first.getFullYear().toInt, first.getMonth().toInt)
    val last = new js.Date(y, m + 1, 0).getDate().toInt
    iso(new js.Date(y, m, math.min(day(d), last)))
  }

  /** 曜日（0 = 日曜日）. */
  def dow(
    d: String
  ): Int = parse(d).getDay().toInt

  def diffDays(
    a: String,
    b: String
  ): Int = {
    def utc(
      d: String
    ) = js.Date.UTC(year(d), month(d) - 1, day(d))
    math.round((utc(b) - utc(a)) / 86400000).toInt
  }

  /** その日を含む週の始まり（start は週の始まりの曜日）. */
  def weekStart(
    d: String,
    start: Int
  ): String = addDays(d, -((dow(d) - start + 7) % 7))

  def monthStart(
    d: String
  ): String = d.take(8) + "01"

  def range(
    from: String,
    n: Int
  ): List[String] = (0 until n).toList.map(addDays(from, _))

  /** 日時（ISO）のブラウザのタイムゾーンでの日付. */
  def dateOf(
    instant: String
  ): String = iso(new js.Date(instant))

  /** 日時の、その日の0時からの分. */
  def minutesOf(
    instant: String
  ): Int = { val d = new js.Date(instant); d.getHours().toInt * 60 + d.getMinutes().toInt }

  /** 日付と、その日の0時からの分 → 日時（ISO. 24時以降は次の日になる）. */
  def instant(
    d: String,
    minutes: Int
  ): String = new js.Date(year(d), month(d) - 1, day(d), 0, minutes).toISOString()

  def hm(
    minutes: Int
  ): String = f"${minutes / 60}%02d:${minutes % 60}%02d"

  def parseHm(
    s: String
  ): Int = s.split(':') match {
    case Array(h, m) => h.toIntOption.getOrElse(0) * 60 + m.toIntOption.getOrElse(0)
    case _ => 0
  }

  /** 日時の時刻（13:05）. */
  def time(
    instant: String
  ): String = hm(minutesOf(instant))

  /** 11月15日(日). */
  def label(
    d: String
  ): String = s"${month(d)}月${day(d)}日(${WeekNames(dow(d))})"

  /** 2026年11月15日(日). */
  def longLabel(
    d: String
  ): String = s"${year(d)}年" + label(d)

  /** ブラウザのタイムゾーン（Asia/Tokyo）. */
  def timezone(): String =
    js.Dynamic.global.Intl.DateTimeFormat().resolvedOptions().timeZone.asInstanceOf[String]

  /** ブラウザが知っているタイムゾーンの一覧. */
  def timezones(): List[String] =
    scala.util.Try(
      js.Dynamic.global.Intl.supportedValuesOf("timeZone").asInstanceOf[js.Array[String]].toList
    ).getOrElse(List(timezone()))

  /** 「第2火曜日」の 2（月の中で何回目のその曜日か）と、最後の週か. */
  def nthInMonth(
    d: String
  ): (Int, Boolean) = ((day(d) - 1) / 7 + 1, day(addDays(d, 7)) < day(d))
}
