package skuaka.core

import skuaka.shared.Recurrence

import java.time.*

class CoreSpec extends munit.FunSuite:

  private val tokyo = ZoneId.of("Asia/Tokyo")
  private val ny    = ZoneId.of("America/New_York")
  private def at(d: String, t: String, z: ZoneId) = LocalDateTime.parse(s"${d}T$t").atZone(z).toInstant
  private def days(xs: List[Span]) = xs.map {
    case Span.AllDay(s, _) => s.toString
    case Span.Timed(s, _)  => s.atZone(tokyo).toLocalDate.toString
  }
  private def range(a: String, b: String) = (LocalDate.parse(a).atStartOfDay(tokyo).toInstant, LocalDate.parse(b).atStartOfDay(tokyo).toInstant)

  test("RRULE の読み書き") {
    val r = Recurrence(Recurrence.Weekly, 2, List("MO", "WE"), Some("2026-12-31"), None)
    assertEquals(r.toRrule, "FREQ=WEEKLY;INTERVAL=2;BYDAY=MO,WE;UNTIL=20261231")
    assertEquals(Recurrence.parse("RRULE:" + r.toRrule), Some(r))
    assertEquals(Recurrence.parse("FREQ=MONTHLY;BYDAY=-1FR;COUNT=3").map(_.describe), Some("毎月 最終金曜日、3 回"))
    assertEquals(Recurrence(Recurrence.Weekly, byDay = Recurrence.Weekdays).describe, "毎週 平日（月〜金）")
    assert(Recurrence("HOURLY").valid.isLeft)
  }

  test("繰り返しのない予定は期間に重なるときだけ") {
    val sp = Span.Timed(at("2026-11-15", "10:00", tokyo), at("2026-11-15", "11:00", tokyo))
    val (f, t) = range("2026-11-15", "2026-11-16")
    assertEquals(Expand.occurrences(sp, tokyo, None, Set.empty, f, t, tokyo), List(sp))
    val (f2, t2) = range("2026-11-16", "2026-11-17")
    assertEquals(Expand.occurrences(sp, tokyo, None, Set.empty, f2, t2, tokyo), Nil)
  }

  test("毎週 月・水（2週間ごと）と除外の日") {
    val sp = Span.Timed(at("2026-11-02", "09:00", tokyo), at("2026-11-02", "09:30", tokyo)) // 月曜日
    val r  = Recurrence(Recurrence.Weekly, 2, List("MO", "WE"))
    val (f, t) = range("2026-11-01", "2026-12-01")
    val ex = Set(at("2026-11-16", "09:00", tokyo).toString)
    assertEquals(days(Expand.occurrences(sp, tokyo, Some(r), ex, f, t, tokyo)), List("2026-11-02", "2026-11-04", "2026-11-18", "2026-11-30"))
  }

  test("毎月 第2火曜日・最終金曜日、31日の毎月は短い月を飛ばす") {
    val sp = Span.AllDay(LocalDate.parse("2026-01-13"), LocalDate.parse("2026-01-13"))
    val (f, t) = range("2026-01-01", "2026-05-01")
    assertEquals(days(Expand.occurrences(sp, tokyo, Some(Recurrence(Recurrence.Monthly, byDay = List("2TU"))), Set.empty, f, t, tokyo)),
      List("2026-01-13", "2026-02-10", "2026-03-10", "2026-04-14"))
    assertEquals(days(Expand.occurrences(sp, tokyo, Some(Recurrence(Recurrence.Monthly, byDay = List("-1FR"))), Set.empty, f, t, tokyo)),
      List("2026-01-30", "2026-02-27", "2026-03-27", "2026-04-24"))
    val sp31 = Span.AllDay(LocalDate.parse("2026-01-31"), LocalDate.parse("2026-01-31"))
    assertEquals(days(Expand.occurrences(sp31, tokyo, Some(Recurrence(Recurrence.Monthly)), Set.empty, f, t, tokyo)),
      List("2026-01-31", "2026-03-31"))
  }

  test("回数と終わりの日、毎年のうるう日") {
    val sp = Span.AllDay(LocalDate.parse("2026-11-15"), LocalDate.parse("2026-11-16"))
    val (f, t) = range("2026-01-01", "2027-12-31")
    assertEquals(Expand.occurrences(sp, tokyo, Some(Recurrence(Recurrence.Daily, count = Some(3))), Set.empty, f, t, tokyo).size, 3)
    assertEquals(days(Expand.occurrences(sp, tokyo, Some(Recurrence(Recurrence.Daily, 3, until = Some("2026-11-24"))), Set.empty, f, t, tokyo)),
      List("2026-11-15", "2026-11-18", "2026-11-21", "2026-11-24"))
    val leap = Span.AllDay(LocalDate.parse("2028-02-29"), LocalDate.parse("2028-02-29"))
    assertEquals(Expand.all(leap, tokyo, Recurrence(Recurrence.Yearly)).take(2).map(_.key).toList, List("2028-02-29", "2032-02-29"))
  }

  test("夏時間をまたいでも現地の時刻を保つ") {
    val sp = Span.Timed(at("2026-10-26", "09:00", ny), at("2026-10-26", "10:00", ny))
    val xs = Expand.all(sp, ny, Recurrence(Recurrence.Weekly)).take(2).toList
    assertEquals(xs.map { case Span.Timed(s, _) => s.atZone(ny).toLocalTime.toString; case _ => "" }, List("09:00", "09:00"))
    assertEquals(xs.map { case Span.Timed(s, e) => Duration.between(s, e).toMinutes; case _ => 0L }, List(60L, 60L))
  }

  test("長い予定は期間の前から始まっていても出る、これ以降の分割") {
    val sp = Span.AllDay(LocalDate.parse("2026-11-01"), LocalDate.parse("2026-11-03"))
    val (f, t) = range("2026-11-03", "2026-11-04")
    assertEquals(Expand.occurrences(sp, tokyo, Some(Recurrence(Recurrence.Weekly)), Set.empty, f, t, tokyo).map(_.key), List("2026-11-01"))
    assertEquals(Expand.endBefore(sp, tokyo, Recurrence(Recurrence.Daily, count = Some(10)), "2026-11-05").until, Some("2026-11-04"))
  }

  test("iCalendar の書き出し") {
    val e = IcsEvent("u1@skuaka", "会議; 定例, 週次", "1行目\n2行目", "", Span.Timed(at("2026-11-16", "10:00", tokyo), at("2026-11-16", "11:00", tokyo)),
      tokyo, Some(Recurrence(Recurrence.Weekly)), List(at("2026-11-23", "10:00", tokyo).toString), Instant.parse("2026-11-15T00:00:00Z"))
    val s = Ics.calendar("仕事", List(e), Instant.parse("2026-11-15T00:00:00Z"))
    assert(s.contains("DTSTART;TZID=Asia/Tokyo:20261116T100000\r\n"))
    assert(s.contains("RRULE:FREQ=WEEKLY\r\n"))
    assert(s.contains("EXDATE;TZID=Asia/Tokyo:20261123T100000\r\n"))
    assert(s.contains("SUMMARY:会議\\; 定例\\, 週次"))
    assert(s.contains("DESCRIPTION:1行目\\n2行目"))
    assert(s.split("\r\n").forall(_.getBytes("UTF-8").length <= 75))
  }

class ExpandSkipSpec extends munit.FunSuite:
  import skuaka.shared.Recurrence
  import java.time.*

  private val tokyo = ZoneId.of("Asia/Tokyo")

  test("昔から続く繰り返しでも、期間の回だけを（飛ばしても同じ結果で）返す") {
    val sp = Span.Timed(Instant.parse("2016-01-04T01:00:00Z"), Instant.parse("2016-01-04T02:00:00Z"))
    val f  = Instant.parse("2026-11-01T00:00:00Z"); val t = Instant.parse("2026-12-01T00:00:00Z")
    for r <- List(Recurrence(Recurrence.Daily, 3), Recurrence(Recurrence.Weekly, 2, List("MO", "TH")), Recurrence(Recurrence.Monthly, byDay = List("-1FR")),
                  Recurrence(Recurrence.Yearly)) do
      val slow = Expand.all(sp, tokyo, r).takeWhile(_.bounds(tokyo)._1.isBefore(t)).filter(_.bounds(tokyo)._2.isAfter(f)).toList
      assertEquals(Expand.occurrences(sp, tokyo, Some(r), Set.empty, f, t, tokyo), slow, r.toRrule)
  }

  test("iCalendar の折り返しは絵文字（サロゲートペア）を割らない") {
    val line = "SUMMARY:" + "🎉" * 30
    assert(Ics.fold(line).split("\r\n ").forall(p => !Character.isLowSurrogate(p.head) && !Character.isHighSurrogate(p.last)))
  }

  // ---------------------------------------------------------------- BYMONTHDAY・BYMONTH・BYSETPOS

  private def dates(start: String, rrule: String, n: Int = 6): List[String] =
    val r = Recurrence.parse(rrule).flatMap(_.valid.toOption).getOrElse(fail(s"読めない: $rrule"))
    val d = LocalDate.parse(start)
    Expand.all(Span.AllDay(d, d), tokyo, r).take(n).toList.map { case Span.AllDay(s, _) => s.toString; case x => x.toString }

  test("毎月15日と末日（2月は28日）") {
    assertEquals(dates("2027-01-15", "FREQ=MONTHLY;BYMONTHDAY=15,-1"),
      List("2027-01-15", "2027-01-31", "2027-02-15", "2027-02-28", "2027-03-15", "2027-03-31"))
  }

  test("毎月31日（31日のない月は飛ばす）") {
    assertEquals(dates("2027-01-31", "FREQ=MONTHLY;BYMONTHDAY=31", 4), List("2027-01-31", "2027-03-31", "2027-05-31", "2027-07-31"))
  }

  test("毎月の最終平日（BYSETPOS）・第1と第3月曜日") {
    assertEquals(dates("2027-01-01", "FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-1", 4), List("2027-01-29", "2027-02-26", "2027-03-31", "2027-04-30"))
    assertEquals(dates("2027-01-01", "FREQ=MONTHLY;BYDAY=1MO,3MO", 4), List("2027-01-04", "2027-01-18", "2027-02-01", "2027-02-15"))
  }

  test("13日の金曜日（日にちを曜日で絞る）") {
    assertEquals(dates("2026-01-01", "FREQ=MONTHLY;BYMONTHDAY=13;BYDAY=FR", 3), List("2026-02-13", "2026-03-13", "2026-11-13"))
  }

  test("毎年: 3月と9月の第2日曜日・11月の第4木曜日・年の第1月曜日・1月と7月の1日") {
    assertEquals(dates("2027-01-01", "FREQ=YEARLY;BYMONTH=3,9;BYDAY=2SU", 4), List("2027-03-14", "2027-09-12", "2028-03-12", "2028-09-10"))
    assertEquals(dates("2026-01-01", "FREQ=YEARLY;BYMONTH=11;BYDAY=4TH", 2), List("2026-11-26", "2027-11-25"))
    assertEquals(dates("2026-01-01", "FREQ=YEARLY;BYDAY=1MO", 2), List("2026-01-05", "2027-01-04"))
    assertEquals(dates("2026-01-01", "FREQ=YEARLY;BYMONTH=1,7;BYMONTHDAY=1", 3), List("2026-01-01", "2026-07-01", "2027-01-01"))
  }

  test("毎日・毎週を月で絞る（BYMONTH）") {
    assertEquals(dates("2026-11-29", "FREQ=DAILY;BYMONTH=12", 3), List("2026-12-01", "2026-12-02", "2026-12-03"))
    assertEquals(dates("2026-11-01", "FREQ=WEEKLY;BYDAY=MO;BYMONTH=1", 2), List("2027-01-04", "2027-01-11"))
  }

  test("回数の指定は BYSETPOS で選んだあとの回を数える. RRULE の書き戻しと説明") {
    assertEquals(dates("2027-01-01", "FREQ=MONTHLY;BYMONTHDAY=1,15;COUNT=3", 10), List("2027-01-01", "2027-01-15", "2027-02-01"))
    val r = Recurrence.parse("FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-1").get
    assertEquals(Recurrence.parse(r.toRrule), Some(r))
    assertEquals(r.describe, "毎月 平日（月〜金）のうち最後")
    assertEquals(Recurrence(Recurrence.Monthly, byMonthDay = List(-1, 15)).describe, "毎月 15日・末日")
    assertEquals(Recurrence(Recurrence.Yearly, byMonth = List(3), byDay = List("2SU")).describe, "毎年 3月 第2日曜日")
    assertEquals(Recurrence(Recurrence.Monthly, byMonthDay = List(13), byDay = List("FR")).describe, "毎月 13日で金曜日のとき")
    assert(Recurrence(Recurrence.Weekly, byDay = List("1MO")).valid.isLeft)
    assert(Recurrence(Recurrence.Monthly, byMonthDay = List(32)).valid.isLeft)
  }

  test("古い形の JSON（新しい項目のないもの）も読める") {
    import io.circe.parser.decode
    assertEquals(decode[Recurrence]("""{"freq":"WEEKLY","interval":1,"byDay":["MO"]}"""), Right(Recurrence(Recurrence.Weekly, byDay = List("MO"))))
  }
