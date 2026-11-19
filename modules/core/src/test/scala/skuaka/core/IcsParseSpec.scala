package skuaka.core

import skuaka.shared.Recurrence

import java.time.*

class IcsParseSpec extends munit.FunSuite:

  private val tokyo = ZoneId.of("Asia/Tokyo")
  private def at(s: String, z: ZoneId = tokyo) = LocalDateTime.parse(s).atZone(z).toInstant

  /** Google カレンダーが書き出すものに近い iCalendar. */
  private val google = List(
    "BEGIN:VCALENDAR",
    "PRODID:-//Google Inc//Google Calendar 70.9054//EN",
    "VERSION:2.0",
    "X-WR-CALNAME:仕事",
    "X-WR-TIMEZONE:Asia/Tokyo",
    "BEGIN:VTIMEZONE", "TZID:Asia/Tokyo", "BEGIN:STANDARD", "DTSTART:19700101T000000", "TZOFFSETFROM:+0900", "TZOFFSETTO:+0900", "END:STANDARD", "END:VTIMEZONE",
    // 時刻のある予定（TZID）. 説明は折り返しとエスケープ
    "BEGIN:VEVENT",
    "DTSTART;TZID=Asia/Tokyo:20261201T100000",
    "DTEND;TZID=Asia/Tokyo:20261201T113000",
    "UID:a1@google.com",
    "SUMMARY:定例会議\\, 第1回",
    "DESCRIPTION:議題:\\n1. 進み具合\\n2. 次の作業。とても長い説明の行はここで折り返されま",
    " す",
    "LOCATION:会議室A",
    "BEGIN:VALARM", "ACTION:DISPLAY", "TRIGGER:-PT10M", "DESCRIPTION:アラーム", "END:VALARM",
    "END:VEVENT",
    // 終日の予定（DTEND は次の日）
    "BEGIN:VEVENT",
    "DTSTART;VALUE=DATE:20261224",
    "DTEND;VALUE=DATE:20261226",
    "UID:a2@google.com",
    "SUMMARY:冬休み",
    "END:VEVENT",
    // 毎週の繰り返し（UNTIL は UTC. 1回を除き、1回を変更）
    "BEGIN:VEVENT",
    "DTSTART;TZID=Asia/Tokyo:20261202T090000",
    "DTEND;TZID=Asia/Tokyo:20261202T093000",
    "RRULE:FREQ=WEEKLY;WKST=MO;UNTIL=20261230T145959Z;BYDAY=WE",
    "EXDATE;TZID=Asia/Tokyo:20261209T090000",
    "UID:a3@google.com",
    "SUMMARY:朝会",
    "END:VEVENT",
    "BEGIN:VEVENT",
    "DTSTART;TZID=Asia/Tokyo:20261216T100000",
    "DTEND;TZID=Asia/Tokyo:20261216T103000",
    "RECURRENCE-ID;TZID=Asia/Tokyo:20261216T090000",
    "UID:a3@google.com",
    "SUMMARY:朝会（遅らせた）",
    "END:VEVENT",
    // UTC の時刻、DURATION
    "BEGIN:VEVENT",
    "DTSTART:20261203T030000Z",
    "DURATION:PT45M",
    "UID:a4@google.com",
    "SUMMARY:電話",
    "END:VEVENT",
    // 取り消された予定・UID のない予定
    "BEGIN:VEVENT", "DTSTART:20261204T030000Z", "UID:a5@google.com", "STATUS:CANCELLED", "SUMMARY:中止", "END:VEVENT",
    "BEGIN:VEVENT", "DTSTART:20261204T030000Z", "SUMMARY:UIDなし", "END:VEVENT",
    // 毎月15日と末日（BYMONTHDAY）・skuaka で表せない繰り返し（年の何日目）
    "BEGIN:VEVENT", "DTSTART;VALUE=DATE:20261215", "UID:a6@google.com", "RRULE:FREQ=MONTHLY;BYMONTHDAY=15,-1", "SUMMARY:支払い", "END:VEVENT",
    "BEGIN:VEVENT", "DTSTART;VALUE=DATE:20260101", "UID:a7@google.com", "RRULE:FREQ=YEARLY;BYYEARDAY=100", "SUMMARY:100日目", "END:VEVENT",
    "END:VCALENDAR").mkString("\r\n")

  private lazy val parsed = IcsParse.parse(google, ZoneOffset.UTC).fold(fail(_), identity)
  private def ev(uid: String) = parsed.events.find(_.uid == uid).getOrElse(fail(s"$uid がない: ${parsed.events.map(_.uid)}"))

  test("カレンダーの名前と、読み取れなかった予定の数") {
    assertEquals(parsed.name, Some("仕事"))
    assertEquals(parsed.skipped, 1)
    assert(!parsed.events.exists(_.uid == "a5@google.com"), "取り消された予定は読まない")
  }

  test("時刻のある予定: TZID・折り返し・エスケープ. VALARM は読み飛ばす") {
    val e = ev("a1@google.com")
    assertEquals(e.title, "定例会議, 第1回")
    assertEquals(e.description, "議題:\n1. 進み具合\n2. 次の作業。とても長い説明の行はここで折り返されます")
    assertEquals(e.location, "会議室A")
    assertEquals(e.span, Span.Timed(at("2026-12-01T10:00"), at("2026-12-01T11:30")))
    assertEquals(e.zone, tokyo)
  }

  test("終日の予定: DTEND は含まないので、終わりの日は前の日") {
    assertEquals(ev("a2@google.com").span, Span.AllDay(LocalDate.parse("2026-12-24"), LocalDate.parse("2026-12-25")))
  }

  test("繰り返し: UNTIL は予定のタイムゾーンの日付. EXDATE と、変更した回は元の予定から除く") {
    val e = ev("a3@google.com")
    assertEquals(e.rule, Some(Recurrence(Recurrence.Weekly, 1, List("WE"), Some("2026-12-30"), None)))
    assertEquals(e.exdates.toSet, Set(at("2026-12-09T09:00").toString, at("2026-12-16T09:00").toString))
    assert(!e.approximated)
    val moved = ev(s"a3@google.com#${at("2026-12-16T09:00")}")
    assertEquals(moved.title, "朝会（遅らせた）")
    assertEquals(moved.rule, None)
    assertEquals(moved.span, Span.Timed(at("2026-12-16T10:00"), at("2026-12-16T10:30")))
    // 展開すると 12/2・12/23・12/30（9日と16日は除く）
    val occ = Expand.occurrences(e.span, e.zone, e.rule, e.exdates.toSet, at("2026-12-01T00:00"), at("2027-01-31T00:00"), tokyo)
    assertEquals(occ.map(_.key), List("2026-12-02T09:00", "2026-12-23T09:00", "2026-12-30T09:00").map(s => at(s).toString))
  }

  test("UTC の時刻と DURATION. 浮動の時刻でなければ X-WR-TIMEZONE を基準にする") {
    val e = ev("a4@google.com")
    assertEquals(e.span, Span.Timed(Instant.parse("2026-12-03T03:00:00Z"), Instant.parse("2026-12-03T03:45:00Z")))
    assertEquals(e.zone, tokyo)
  }

  test("毎月15日と末日はそのまま読む. 表せない繰り返しの指定（BYYEARDAY）は省いて近似する") {
    val e = ev("a6@google.com")
    assertEquals(e.rule, Some(Recurrence(Recurrence.Monthly, byMonthDay = List(15, -1))))
    assert(!e.approximated)
    assert(ev("a7@google.com").approximated)
  }

  test("Outlook の Windows のタイムゾーン名・浮動の時刻・iCalendar でないもの") {
    val ics = List("BEGIN:VCALENDAR", "BEGIN:VEVENT", "UID:o1", "DTSTART;TZID=\"Tokyo Standard Time\":20261201T100000",
      "DTEND;TZID=\"Tokyo Standard Time\":20261201T110000", "SUMMARY:Outlook", "END:VEVENT",
      "BEGIN:VEVENT", "UID:o2", "DTSTART:20261201T100000", "DTEND:20261201T110000", "END:VEVENT", "END:VCALENDAR").mkString("\n")
    val p = IcsParse.parse(ics, tokyo).fold(fail(_), identity)
    assertEquals(p.events.map(_.span), List(Span.Timed(at("2026-12-01T10:00"), at("2026-12-01T11:00")), Span.Timed(at("2026-12-01T10:00"), at("2026-12-01T11:00"))))
    assertEquals(p.events.map(_.title), List("Outlook", "（タイトルなし）"))
    assert(IcsParse.parse("<html></html>", tokyo).isLeft)
  }

  test("DURATION の読み取り") {
    assertEquals(IcsParse.duration("P1D"), Some(Duration.ofDays(1)))
    assertEquals(IcsParse.duration("P1DT2H"), Some(Duration.ofHours(26)))
    assertEquals(IcsParse.duration("P2W"), Some(Duration.ofDays(14)))
    assertEquals(IcsParse.duration("PT1H30M"), Some(Duration.ofMinutes(90)))
    assertEquals(IcsParse.duration("-PT15M"), Some(Duration.ofMinutes(-15)))
  }

  test("skuaka が書き出した iCalendar を読み戻せる") {
    val src = IcsEvent("x1@skuaka", "読み戻し; テスト", "説明\n2行目", "", Span.Timed(at("2026-12-01T10:00"), at("2026-12-01T11:00")), tokyo,
      Some(Recurrence(Recurrence.Daily, 1, Nil, None, Some(5))), List(at("2026-12-03T10:00").toString), Instant.now())
    val p = IcsParse.parse(Ics.calendar("名前", List(src), Instant.now()), ZoneOffset.UTC).fold(fail(_), identity)
    val e = p.events.head
    assertEquals((e.uid, e.title, e.description, e.span, e.zone, e.rule, e.exdates), (src.uid, src.title, src.description, src.span, tokyo, src.rule, src.exdates))
  }
