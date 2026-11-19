package skuaka.web

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
    val off = (8 - Cal.dow(first)) % 7
    Cal.addDays(first, off + (n - 1) * 7)
  }

  private def compute(y: Int): Map[String, String] = {
    val k = y - 1980
    val spring = (20.8431 + 0.242194 * k - math.floor(k / 4.0)).toInt
    val autumn = (23.2488 + 0.242194 * k - math.floor(k / 4.0)).toInt
    val base = List(
      ymd(y, 1, 1) -> "元日",
      monday(y, 1, 2) -> "成人の日",
      ymd(y, 2, 11) -> "建国記念の日",
      ymd(y, 2, 23) -> "天皇誕生日",
      ymd(y, 3, spring) -> "春分の日",
      ymd(y, 4, 29) -> "昭和の日",
      ymd(y, 5, 3) -> "憲法記念日",
      ymd(y, 5, 4) -> "みどりの日",
      ymd(y, 5, 5) -> "こどもの日",
      monday(y, 7, 3) -> "海の日",
      ymd(y, 8, 11) -> "山の日",
      monday(y, 9, 3) -> "敬老の日",
      ymd(y, 9, autumn) -> "秋分の日",
      monday(y, 10, 2) -> "スポーツの日",
      ymd(y, 11, 3) -> "文化の日",
      ymd(y, 11, 23) -> "勤労感謝の日"
    ).toMap
    // 国民の休日: 祝日に挟まれた平日
    val citizens = base.keys.toList.flatMap { d =>
      val next = Cal.addDays(d, 2)
      val mid = Cal.addDays(d, 1)
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
