package skuaka.api

import cats.effect.*
import cats.syntax.all.*
import skuaka.core.*
import skuaka.db.*
import skuaka.observe.{Log, Metrics, Telemetry}
import skuaka.push.{PushMessage, PushService}
import skuaka.shared.Reminders

import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDate, ZoneId}
import java.util.{Locale, UUID}
import scala.concurrent.duration.*

/** 通知の文言の日時（受け取る人のタイムゾーンで）. */
object When:
  private val dateFmt = DateTimeFormatter.ofPattern("M月d日（E）", Locale.JAPANESE)
  private val timeFmt = DateTimeFormatter.ofPattern("H:mm")

  def zone(tz: String): ZoneId = Time.zone(tz).getOrElse(ZoneId.of("Asia/Tokyo"))

  /** 「11月17日（火） 10:00」. today と同じ日なら日付を省く. 終日の予定は日付だけ. */
  def text(span: Span, zone: ZoneId, today: Option[LocalDate] = None): String = span match
    case Span.AllDay(s, e) => dateFmt.format(s) + (if e != s then s"〜${dateFmt.format(e)}" else "") + " 終日"
    case Span.Timed(s, _) =>
      val at = s.atZone(zone)
      (if today.contains(at.toLocalDate) then "" else dateFmt.format(at) + " ") + timeFmt.format(at)

/**
 * 予定のプッシュ通知（PWA）:
 *  - 招待: 予定のゲストに加えられた人へ
 *  - リマインダー: 予定に付けた通知（何分前）の時刻に、カレンダーの所有者・共有された人・ゲストへ（欠席の人を除く）.
 *    複数ノードで動いても、reminder_sent に先に印を付けたノードだけが送る
 */
final class EventNotifier(events: EventStore, users: UserStore, push: PushService):

  /** 招待した人へ知らせる（失敗しても予定の保存は止めない）. */
  def invited(by: User, id: UUID, guests: List[UUID]): IO[Unit] =
    IO.whenA(guests.nonEmpty) {
      Telemetry.span("event.invite", "event.id" -> id.toString)(events.get(id).flatMap(_.traverse_ { r =>
        guests.traverse_(g => users.find(g).flatMap(_.filterNot(_.disabled).traverse_ { t =>
          val body = s"${r.title}\n${When.text(r.span, When.zone(t.timezone))}" + (if r.location.nonEmpty then s"\n${r.location}" else "")
          push.send(g, PushMessage(s"${by.name} さんから予定に招待されました", body, "/", Some(s"invite-$id")))
        }))
      })).handleErrorWith(e => Log.println(s"[skuaka] invite push failed: $e"))
    }

  /** リマインダーを送る裏の処理（30秒ごと. 起動より前の時刻の分は送らない）. */
  def reminderLoop: IO[Nothing] =
    def go(last: Instant): IO[Nothing] =
      IO.sleep(30.seconds) *> IO.realTimeInstant.flatMap { now =>
        Telemetry.span("reminder.scan")(scan(last, now)).handleErrorWith(e => Log.println(s"[skuaka] reminder scan failed: $e")) *> go(now)
      }
    IO.realTimeInstant.flatMap(go)

  /** (last, now] に通知の時刻が来た回を送る. */
  private def scan(last: Instant, now: Instant): IO[Unit] =
    events.overlapping(last, now.plusSeconds(Reminders.MaxMinutes * 60L + 60), timedWithReminders = true).flatMap { rows =>
      val due = for
        r <- rows
        o <- Expand.occurrences(r.span, r.zone, r.recurrence, r.exdates.toSet, last, now.plusSeconds(r.reminders.max * 60L + 1), r.zone)
        m <- r.reminders
        at = o.bounds(r.zone)._1.minusSeconds(m * 60L)
        if at.isAfter(last) && !at.isAfter(now)
      yield (r, o, m)
      due.traverse_((r, o, m) => events.markReminder(r.id, o.key, m).ifM(remind(r, o, m), IO.unit))
    }

  private def remind(r: EventRow, o: Span, minutes: Int): IO[Unit] =
    val when = if minutes == 0 then "" else s"（${Reminders.label(minutes).stripSuffix("前")}後）"
    // 画面を開いているときの通知（Store.reminders）と同じ tag にして、2つ出ないようにする
    val tag = s"${r.id}${o.key}-$minutes"
    events.reminderTargets(r.id, r.calendarId).flatMap(_.traverse_ { (uid, tz) =>
      val zone = When.zone(tz)
      val body = s"${When.text(o, zone, Some(LocalDate.now(zone)))}〜$when" + (if r.location.nonEmpty then s"\n${r.location}" else "")
      push.send(uid, PushMessage(r.title, body, "/", Some(tag)))
    }) *> Metrics.count("skuaka.reminders.sent")
