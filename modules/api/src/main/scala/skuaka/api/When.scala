package skuaka.api

import skuaka.core.*

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, ZoneId}
import java.util.Locale

/** 通知の文言の日時（受け取る人のタイムゾーンで）. */
object When {
  private val dateFmt = DateTimeFormatter.ofPattern("M月d日（E）", Locale.JAPANESE)
  private val timeFmt = DateTimeFormatter.ofPattern("H:mm")

  def zone(tz: String): ZoneId = Time.zone(tz).getOrElse(ZoneId.of("Asia/Tokyo"))

  /** 「11月17日（火） 10:00」. today と同じ日なら日付を省く. 終日の予定は日付だけ. */
  def text(span: Span, zone: ZoneId, today: Option[LocalDate] = None): String = span match {
    case Span.AllDay(s, e) => dateFmt.format(s) + (if e != s then s"〜${dateFmt.format(e)}" else "") + " 終日"
    case Span.Timed(s, _) =>
      val at = s.atZone(zone)
      (if today.contains(at.toLocalDate) then "" else dateFmt.format(at) + " ") + timeFmt.format(at)
  }
}
