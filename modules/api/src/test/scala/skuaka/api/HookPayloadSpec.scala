package skuaka.api

import skuaka.core.Span
import skuaka.db.EventRow
import skuaka.shared.*

import java.time.Instant
import java.util.UUID

/** 通知先に送る本文と、stui に送る文. */
class HookPayloadSpec extends munit.FunSuite {

  private val id = UUID.fromString("00000000-0000-0000-0000-000000000001")
  private val cal = UUID.fromString("00000000-0000-0000-0000-000000000002")
  private val row = EventRow(
    id,
    cal,
    None,
    "定例",
    "説明",
    "会議室A",
    false,
    Some(Instant.parse("2026-12-03T23:30:00Z")),
    Some(Instant.parse("2026-12-04T00:30:00Z")),
    None,
    None,
    "Asia/Tokyo",
    None,
    Some("FREQ=WEEKLY"),
    Nil,
    List(10),
    1,
    Instant.EPOCH
  )

  test("リマインダー: その回の日時（予定のタイムゾーン）・何分後・場所. リンクはその回の日を開く") {
    val o = Span.Timed(Instant.parse("2026-12-10T23:30:00Z"), Instant.parse("2026-12-11T00:30:00Z"))
    val p = HookPayload.of(HookEvent.Reminder, cal -> "チーム", row, o, "https://cal.example.com", minutes = Some(10))
    assertEquals(p.hcursor.downField("event").get[String]("start"), Right("2026-12-10T23:30:00Z"))
    assertEquals(p.hcursor.get[Int]("minutes"), Right(10))
    assert(p.hcursor.downField("actor").focus.isEmpty)
    val m = HookPayload.message(p)
    assertEquals(m.title, "🔔 定例")
    assertEquals(m.text, "12月11日（金） 8:30〜9:30（10分後）\n📍 会議室A")
    assertEquals(m.url, s"https://cal.example.com/?event=$id&date=2026-12-11")
  }

  test("繰り返しの予定の削除は範囲を添え、リンクはその日のカレンダー. 終日の予定は日付だけ") {
    val u = skuaka.db.User(UUID.randomUUID(), "s", "山田", None, None, "Asia/Tokyo", false, false)
    val p = HookPayload.of(
      HookEvent.Deleted,
      cal -> "チーム",
      row,
      row.span,
      "https://c",
      Some(u),
      scope = Some(EditScope.Following)
    )
    val m = HookPayload.message(p)
    assertEquals(m.text, "山田 さんが「チーム」の予定を削除しました（この回以降）\n12月4日（金） 8:30〜9:30\n📍 会議室A")
    assertEquals(m.url, "https://c/?date=2026-12-04")
    val allDay = row.copy(
      allDay = true,
      startAt = None,
      endAt = None,
      startDate = Some(java.time.LocalDate.parse("2026-12-24")),
      endDate = Some(java.time.LocalDate.parse("2026-12-25")),
      rrule = None,
      location = ""
    )
    val m2 = HookPayload.message(HookPayload.of(
      HookEvent.Created,
      cal -> "チーム",
      allDay,
      allDay.span,
      "https://c",
      Some(u),
      scope = Some(EditScope.All)
    ))
    assertEquals(m2.text, "山田 さんが「チーム」の予定を追加しました\n12月24日（木）〜12月25日（金） 終日")
  }
}
