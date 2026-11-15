package skuaka.shared

import io.circe.*

/**
 * 予定の繰り返し（iCalendar の RRULE の一部）.
 *  - freq: DAILY / WEEKLY / MONTHLY / YEARLY
 *  - byDay: 曜日（MO〜SU）. 毎月の「第2火曜日」は 2TU、「最終金曜日」は -1FR
 *  - until: 最後の日（yyyy-MM-dd. その日を含む）、count: 回数. どちらもなければ終わりなし
 */
final case class Recurrence(freq: String, interval: Int = 1, byDay: List[String] = Nil, until: Option[String] = None,
                            count: Option[Int] = None) derives Encoder.AsObject, Decoder {

  /** RRULE の値（FREQ=WEEKLY;BYDAY=MO,WE）. until は終日の予定と同じく日付で書く. */
  def toRrule: String = {
    val parts = List(
      Some(s"FREQ=$freq"),
      Option.when(interval > 1)(s"INTERVAL=$interval"),
      Option.when(byDay.nonEmpty)(s"BYDAY=${byDay.mkString(",")}"),
      until.map(u => s"UNTIL=${u.replace("-", "")}"),
      count.map(c => s"COUNT=$c")
    )
    parts.flatten.mkString(";")
  }

  /** 「毎週 月・水曜日」のような説明. */
  def describe: String = {
    val every = freq match {
      case Recurrence.Daily   => if (interval > 1) s"${interval}日ごと" else "毎日"
      case Recurrence.Weekly  => if (interval > 1) s"${interval}週間ごと" else "毎週"
      case Recurrence.Monthly => if (interval > 1) s"${interval}か月ごと" else "毎月"
      case Recurrence.Yearly  => if (interval > 1) s"${interval}年ごと" else "毎年"
      case other              => other
    }
    val days =
      if (byDay.isEmpty) ""
      else if (freq == Recurrence.Weekly && byDay.toSet == Recurrence.Weekdays.toSet) " 平日（月〜金）"
      else " " + byDay.map(Recurrence.describeDay).mkString("・")
    val end = until.map(u => s"、${u.replace('-', '/')} まで").orElse(count.map(c => s"、$c 回")).getOrElse("")
    every + days + end
  }

  def valid: Either[String, Recurrence] =
    if (!Recurrence.freqs.contains(freq)) Left("繰り返しの種類が正しくありません")
    else if (interval < 1 || interval > 999) Left("繰り返しの間隔は 1〜999 にしてください")
    else if (count.exists(c => c < 1 || c > 9999)) Left("繰り返しの回数は 1〜9999 にしてください")
    else if (byDay.exists(d => Recurrence.parseDay(d).isEmpty)) Left("曜日の指定が正しくありません")
    else if (until.exists(u => !u.matches("""\d{4}-\d{2}-\d{2}"""))) Left("繰り返しの終わりの日は yyyy-MM-dd で指定してください")
    else Right(this)
}

object Recurrence {
  val Daily = "DAILY"
  val Weekly = "WEEKLY"
  val Monthly = "MONTHLY"
  val Yearly = "YEARLY"
  val freqs: List[String] = List(Daily, Weekly, Monthly, Yearly)

  /** 月曜日始まりの曜日（ISO の 1〜7 の順）. */
  val Days: List[String] = List("MO", "TU", "WE", "TH", "FR", "SA", "SU")
  val Weekdays: List[String] = Days.take(5)
  val DayNames: Map[String, String] = Days.zip(List("月", "火", "水", "木", "金", "土", "日")).toMap

  /** "2TU" → Some((Some(2), 2)) （第何週か、ISO の曜日 1〜7）. */
  def parseDay(s: String): Option[(Option[Int], Int)] = {
    val code = s.takeRight(2)
    val n = s.dropRight(2)
    val dow = Days.indexOf(code) + 1
    if (dow == 0) None
    else if (n.isEmpty) Some((None, dow))
    else n.toIntOption.filter(x => x != 0 && x >= -5 && x <= 5).map(x => (Some(x), dow))
  }

  def describeDay(s: String): String = parseDay(s) match {
    case Some((None, d))     => DayNames(Days(d - 1)) + "曜日"
    case Some((Some(-1), d)) => "最終" + DayNames(Days(d - 1)) + "曜日"
    case Some((Some(n), d))  => s"第$n" + DayNames(Days(d - 1)) + "曜日"
    case None                => s
  }

  /** RRULE の値を読む（知らない項目は無視する）. */
  def parse(rrule: String): Option[Recurrence] = {
    val kv = rrule.stripPrefix("RRULE:").split(';').toList.flatMap { p =>
      p.split('=') match {
        case Array(k, v) => Some(k.trim.toUpperCase -> v.trim)
        case _           => None
      }
    }.toMap
    kv.get("FREQ").map(_.toUpperCase).filter(freqs.contains).map { f =>
      Recurrence(
        freq = f,
        interval = kv.get("INTERVAL").flatMap(_.toIntOption).getOrElse(1),
        byDay = kv.get("BYDAY").map(_.split(',').toList.map(_.trim.toUpperCase).filter(_.nonEmpty)).getOrElse(Nil),
        until = kv.get("UNTIL").filter(_.length >= 8).map(u => s"${u.take(4)}-${u.slice(4, 6)}-${u.slice(6, 8)}"),
        count = kv.get("COUNT").flatMap(_.toIntOption)
      )
    }
  }
}
