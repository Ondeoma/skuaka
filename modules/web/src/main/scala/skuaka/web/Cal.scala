package skuaka.web

import scala.scalajs.js

/**
 * 日付・時刻の計算（画面のタイムゾーン = ブラウザのタイムゾーン）.
 * 日付は "yyyy-MM-dd" の文字列、日時は ISO 8601 の文字列で持つ（Scala.js で java.time を使わないため）.
 */
object Cal {

  val WeekNames: Vector[String] = Vector("日", "月", "火", "水", "木", "金", "土")

  def parse(d: String): js.Date = new js.Date(d.take(4).toInt, d.slice(5, 7).toInt - 1, d.slice(8, 10).toInt)

  def iso(d: js.Date): String = f"${d.getFullYear().toInt}%04d-${d.getMonth().toInt + 1}%02d-${d.getDate().toInt}%02d"

  def today(): String = iso(new js.Date())

  def year(d: String): Int = d.take(4).toInt
  def month(d: String): Int = d.slice(5, 7).toInt
  def day(d: String): Int = d.slice(8, 10).toInt

  def addDays(d: String, n: Int): String = iso(new js.Date(year(d), month(d) - 1, day(d) + n))

  /** n か月後（月末を超える日はその月の末日）. */
  def addMonths(d: String, n: Int): String = {
    val first = new js.Date(year(d), month(d) - 1 + n, 1)
    val (y, m) = (first.getFullYear().toInt, first.getMonth().toInt)
    val last  = new js.Date(y, m + 1, 0).getDate().toInt
    iso(new js.Date(y, m, math.min(day(d), last)))
  }

  /** 曜日（0 = 日曜日）. */
  def dow(d: String): Int = parse(d).getDay().toInt

  def diffDays(a: String, b: String): Int = {
    def utc(d: String) = js.Date.UTC(year(d), month(d) - 1, day(d))
    math.round((utc(b) - utc(a)) / 86400000).toInt
  }

  /** その日を含む週の始まり（start は週の始まりの曜日）. */
  def weekStart(d: String, start: Int): String = addDays(d, -((dow(d) - start + 7) % 7))

  def monthStart(d: String): String = d.take(8) + "01"

  def range(from: String, n: Int): List[String] = (0 until n).toList.map(addDays(from, _))

  /** 日時（ISO）のブラウザのタイムゾーンでの日付. */
  def dateOf(instant: String): String = iso(new js.Date(instant))

  /** 日時の、その日の0時からの分. */
  def minutesOf(instant: String): Int = { val d = new js.Date(instant); d.getHours().toInt * 60 + d.getMinutes().toInt }

  /** 日付と、その日の0時からの分 → 日時（ISO. 24時以降は次の日になる）. */
  def instant(d: String, minutes: Int): String = new js.Date(year(d), month(d) - 1, day(d), 0, minutes).toISOString()

  def hm(minutes: Int): String = f"${minutes / 60}%02d:${minutes % 60}%02d"

  def parseHm(s: String): Int = s.split(':') match {
    case Array(h, m) => h.toIntOption.getOrElse(0) * 60 + m.toIntOption.getOrElse(0)
    case _           => 0
  }

  /** 日時の時刻（13:05）. */
  def time(instant: String): String = hm(minutesOf(instant))

  /** 11月15日(日). */
  def label(d: String): String = s"${month(d)}月${day(d)}日(${WeekNames(dow(d))})"

  /** 2026年11月15日(日). */
  def longLabel(d: String): String = s"${year(d)}年" + label(d)

  /** ブラウザのタイムゾーン（Asia/Tokyo）. */
  def timezone(): String =
    js.Dynamic.global.Intl.DateTimeFormat().resolvedOptions().timeZone.asInstanceOf[String]

  /** ブラウザが知っているタイムゾーンの一覧. */
  def timezones(): List[String] =
    scala.util.Try(js.Dynamic.global.Intl.supportedValuesOf("timeZone").asInstanceOf[js.Array[String]].toList).getOrElse(List(timezone()))

  /** 「第2火曜日」の 2（月の中で何回目のその曜日か）と、最後の週か. */
  def nthInMonth(d: String): (Int, Boolean) = ((day(d) - 1) / 7 + 1, day(addDays(d, 7)) < day(d))
}

/** 日本の祝日（祝日法. 振替休日・国民の休日を含む. 春分・秋分は近似式で 2099 年まで）. */
object Holidays {

  private var cache: Map[Int, Map[String, String]] = Map.empty

  def name(d: String): Option[String] = of(Cal.year(d)).get(d)

  /** 赤く表示する日（祝日・日曜日）. */
  def red(d: String): Boolean = Cal.dow(d) == 0 || name(d).isDefined

  /** 休みの日（祝日・土日）. */
  def off(d: String): Boolean = Cal.dow(d) == 6 || red(d)

  /** 日付の見出しのクラス（今日は today、祝日・日曜日は hol を足す）. */
  def dayClass(base: String, d: String): String =
    base + (if (d == Cal.today()) " today" else "") + (if (red(d)) " hol" else "")

  def of(y: Int): Map[String, String] = cache.getOrElse(y, { val m = compute(y); cache = cache.updated(y, m); m })

  private def ymd(y: Int, m: Int, d: Int): String = f"$y%04d-$m%02d-$d%02d"

  /** m 月の第 n 月曜日. */
  private def monday(y: Int, m: Int, n: Int): String = {
    val first = ymd(y, m, 1)
    val off   = (8 - Cal.dow(first)) % 7
    Cal.addDays(first, off + (n - 1) * 7)
  }

  private def compute(y: Int): Map[String, String] = {
    val k       = y - 1980
    val spring  = (20.8431 + 0.242194 * k - math.floor(k / 4.0)).toInt
    val autumn  = (23.2488 + 0.242194 * k - math.floor(k / 4.0)).toInt
    val base = List(
      ymd(y, 1, 1) -> "元日", monday(y, 1, 2) -> "成人の日", ymd(y, 2, 11) -> "建国記念の日", ymd(y, 2, 23) -> "天皇誕生日",
      ymd(y, 3, spring) -> "春分の日", ymd(y, 4, 29) -> "昭和の日", ymd(y, 5, 3) -> "憲法記念日", ymd(y, 5, 4) -> "みどりの日",
      ymd(y, 5, 5) -> "こどもの日", monday(y, 7, 3) -> "海の日", ymd(y, 8, 11) -> "山の日", monday(y, 9, 3) -> "敬老の日",
      ymd(y, 9, autumn) -> "秋分の日", monday(y, 10, 2) -> "スポーツの日", ymd(y, 11, 3) -> "文化の日", ymd(y, 11, 23) -> "勤労感謝の日"
    ).toMap
    // 国民の休日: 祝日に挟まれた平日
    val citizens = base.keys.toList.flatMap { d =>
      val next = Cal.addDays(d, 2)
      val mid  = Cal.addDays(d, 1)
      Option.when(base.contains(next) && !base.contains(mid) && Cal.dow(mid) != 0)(mid -> "国民の休日")
    }.toMap
    val all = base ++ citizens
    // 振替休日: 日曜日の祝日の後の、最初の祝日でない日
    val substitutes = all.keys.toList.filter(Cal.dow(_) == 0).map { d =>
      Iterator.iterate(Cal.addDays(d, 1))(Cal.addDays(_, 1)).find(x => !all.contains(x)).get -> "振替休日"
    }.toMap
    all ++ substitutes
  }
}
