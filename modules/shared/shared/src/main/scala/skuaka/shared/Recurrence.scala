package skuaka.shared

import io.circe.*
import io.circe.derivation.{Configuration, ConfiguredDecoder}

/**
 * 予定の繰り返し（iCalendar の RRULE の一部）.
 *  - freq: DAILY / WEEKLY / MONTHLY / YEARLY
 *  - byDay: 曜日（MO〜SU）. 毎月の「第2火曜日」は 2TU、「最終金曜日」は -1FR（毎年で月の指定がなければ、年の中の第何週）
 *  - byMonthDay: 日にち（1〜31. -1 は末日、-2 は末日の前の日）
 *  - byMonth: 月（1〜12）
 *  - bySetPos: 1つの期間（月・年など）の候補の日のうち何番目か（1 は最初、-1 は最後. 「毎月の最終平日」は BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-1）
 *  - until: 最後の日（yyyy-MM-dd. その日を含む）、count: 回数. どちらもなければ終わりなし
 * 省いた項目は既定の値になる（API で古い形の JSON を送ってもよい）.
 */
final case class Recurrence(freq: String,
                            interval: Int = 1,
                            byDay: List[String] = Nil,
                            until: Option[String] = None,
                            count: Option[Int] = None,
                            byMonthDay: List[Int] = Nil,
                            byMonth: List[Int] = Nil,
                            bySetPos: List[Int] = Nil)
  derives Encoder.AsObject {

  /** RRULE の値（FREQ=WEEKLY;BYDAY=MO,WE）. until は終日の予定と同じく日付で書く. */
  def toRrule: String = {
    def list(name: String, xs: List[Any]) = Option.when(xs.nonEmpty)(s"$name=${xs.mkString(",")}")
    val parts = List(
      Some(s"FREQ=$freq"),
      Option.when(interval > 1)(s"INTERVAL=$interval"),
      list("BYMONTH", byMonth),
      list("BYMONTHDAY", byMonthDay),
      list("BYDAY", byDay),
      list("BYSETPOS", bySetPos),
      until.map(u => s"UNTIL=${u.replace("-", "")}"),
      count.map(c => s"COUNT=$c")
    )
    parts.flatten.mkString(";")
  }

  /** 「毎週 月・水曜日」「毎月 15日・末日」「毎年 3月 第2日曜日」のような説明. */
  def describe: String = {
    val every = freq match {
      case Recurrence.Daily => if (interval > 1) s"${interval}日ごと" else "毎日"
      case Recurrence.Weekly => if (interval > 1) s"${interval}週間ごと" else "毎週"
      case Recurrence.Monthly => if (interval > 1) s"${interval}か月ごと" else "毎月"
      case Recurrence.Yearly => if (interval > 1) s"${interval}年ごと" else "毎年"
      case other => other
    }
    val months = if (byMonth.isEmpty) "" else " " + byMonth.sorted.map(m => s"${m}月").mkString("・")
    val mdays = if (byMonthDay.isEmpty) ""
    else " " + Recurrence.sortedMonthDays(byMonthDay).map(Recurrence.describeMonthDay).mkString("・")
    val inYear = freq == Recurrence.Yearly && byMonth.isEmpty && byMonthDay.isEmpty
    val days =
      if (byDay.isEmpty) ""
      else if (byDay.toSet == Recurrence.Weekdays.toSet) " 平日（月〜金）"
      else if (byDay.toSet == Recurrence.Days.toSet) " 毎日"
      else " " + byDay.map(d => Recurrence.describeDay(d, inYear)).mkString("・")
    // 日にちと曜日の両方があれば、曜日は絞り込み（「13日の金曜日」）
    val daysPart = if (byMonthDay.nonEmpty && byDay.nonEmpty) mdays + "で" + days.trim + "のとき" else mdays + days
    val pos = if (bySetPos.isEmpty) "" else "のうち" + bySetPos.map(Recurrence.describePos).mkString("・")
    val end = until.map(u => s"、${u.replace('-', '/')} まで").orElse(count.map(c => s"、$c 回")).getOrElse("")
    every + months + daysPart + pos + end
  }

  def valid: Either[String, Recurrence] =
    if (!Recurrence.freqs.contains(freq)) Left("繰り返しの種類が正しくありません")
    else if (interval < 1 || interval > 999) Left("繰り返しの間隔は 1〜999 にしてください")
    else if (count.exists(c => c < 1 || c > 9999)) Left("繰り返しの回数は 1〜9999 にしてください")
    else if (byDay.exists(d => Recurrence.parseDay(d).isEmpty)) Left("曜日の指定が正しくありません")
    else if (
      byDay.exists(d => Recurrence.parseDay(d).exists(_._1.isDefined)) &&
      !Set(Recurrence.Monthly, Recurrence.Yearly).contains(freq)
    )
      Left("「第n曜日」は毎月・毎年の繰り返しでだけ使えます")
    else if (byMonthDay.exists(d => d == 0 || d < -31 || d > 31)) Left("日にちは 1〜31（末日は -1）で指定してください")
    else if (byMonthDay.nonEmpty && freq == Recurrence.Weekly) Left("毎週の繰り返しには日にちを指定できません")
    else if (byMonth.exists(m => m < 1 || m > 12)) Left("月は 1〜12 で指定してください")
    else if (bySetPos.exists(p => p == 0 || p < -366 || p > 366)) Left("何番目かの指定（BYSETPOS）が正しくありません")
    else if (bySetPos.nonEmpty && byDay.isEmpty && byMonthDay.isEmpty && byMonth.isEmpty)
      Left("何番目かの指定（BYSETPOS）は、曜日・日にち・月と一緒に指定してください")
    else if (List(byDay, byMonthDay, byMonth, bySetPos).exists(_.size > 31)) Left("指定が多すぎます")
    else if (until.exists(u => !u.matches("""\d{4}-\d{2}-\d{2}"""))) Left("繰り返しの終わりの日は yyyy-MM-dd で指定してください")
    else Right(copy(
      byDay = byDay.distinct,
      byMonthDay = byMonthDay.distinct,
      byMonth = byMonth.distinct.sorted,
      bySetPos = bySetPos.distinct
    ))
}

object Recurrence {
  private given Configuration = Configuration.default.withDefaults
  given Decoder[Recurrence] = ConfiguredDecoder.derived[Recurrence]

  val Daily = "DAILY"
  val Weekly = "WEEKLY"
  val Monthly = "MONTHLY"
  val Yearly = "YEARLY"
  val freqs: List[String] = List(Daily, Weekly, Monthly, Yearly)

  /** RRULE のうち、skuaka で表せる項目（WKST は月曜日始まりとして読み飛ばす）. */
  val supportedKeys: Set[String] =
    Set("FREQ", "INTERVAL", "BYDAY", "BYMONTHDAY", "BYMONTH", "BYSETPOS", "UNTIL", "COUNT", "WKST")

  /** 月曜日始まりの曜日（ISO の 1〜7 の順）. */
  val Days: List[String] = List("MO", "TU", "WE", "TH", "FR", "SA", "SU")
  val Weekdays: List[String] = Days.take(5)
  val DayNames: Map[String, String] = Days.zip(List("月", "火", "水", "木", "金", "土", "日")).toMap

  /** "2TU" → Some((Some(2), 2)) （第何週か、ISO の曜日 1〜7）. 毎年の繰り返しでは第53週まで. */
  def parseDay(s: String): Option[(Option[Int], Int)] = {
    val code = s.takeRight(2)
    val n = s.dropRight(2).stripPrefix("+")
    val dow = Days.indexOf(code) + 1
    if (dow == 0) None
    else if (n.isEmpty) Some((None, dow))
    else n.toIntOption.filter(x => x != 0 && x >= -53 && x <= 53).map(x => (Some(x), dow))
  }

  /** 第何週の説明（-1 は最終、-2 は最後から2番目）. */
  private def nth(n: Int): String = if (n == -1) "最終" else if (n < 0) s"最後から${-n}番目の" else s"第$n"

  def describeDay(s: String, inYear: Boolean = false): String = parseDay(s) match {
    case Some((None, d)) => DayNames(Days(d - 1)) + "曜日"
    case Some((Some(n), d)) => (if (inYear) "年の" else "") + nth(n) + DayNames(Days(d - 1)) + "曜日"
    case None => s
  }

  /** 日にちの説明（-1 は末日）. */
  def describeMonthDay(d: Int): String = if (d == -1) "末日" else if (d < 0) s"末日の${-d - 1}日前" else s"${d}日"

  /** 日にちを月の中の順に（1日〜31日、そのあとに末日に近いもの）. */
  def sortedMonthDays(ds: List[Int]): List[Int] = ds.filter(_ > 0).sorted ++ ds.filter(_ < 0).sorted

  def describePos(p: Int): String = if (p == -1) "最後" else if (p < 0) s"最後から${-p}番目" else s"${p}番目"

  /** RRULE の値を読む（知らない項目は無視する）. */
  def parse(rrule: String): Option[Recurrence] = {
    val kv = rrule.stripPrefix("RRULE:").split(';').toList.flatMap { p =>
      p.split('=') match {
        case Array(k, v) => Some(k.trim.toUpperCase -> v.trim)
        case _ => None
      }
    }.toMap
    def ints(k: String): List[Int] =
      kv.get(k).map(_.split(',').toList.flatMap(_.trim.stripPrefix("+").toIntOption)).getOrElse(Nil)
    kv.get("FREQ").map(_.toUpperCase).filter(freqs.contains).map { f =>
      Recurrence(
        freq = f,
        interval = kv.get("INTERVAL").flatMap(_.toIntOption).getOrElse(1),
        byDay = kv.get("BYDAY").map(_.split(',').toList.map(_.trim.toUpperCase).filter(_.nonEmpty)).getOrElse(Nil),
        until = kv.get("UNTIL").filter(_.length >= 8).map(u => s"${u.take(4)}-${u.slice(4, 6)}-${u.slice(6, 8)}"),
        count = kv.get("COUNT").flatMap(_.toIntOption),
        byMonthDay = ints("BYMONTHDAY"),
        byMonth = ints("BYMONTH"),
        bySetPos = ints("BYSETPOS")
      )
    }
  }
}
