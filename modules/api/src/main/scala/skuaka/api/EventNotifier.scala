package skuaka.api

import cats.effect.*
import cats.syntax.all.*
import skuaka.core.*
import skuaka.db.*
import sbirds.observe.{Log, Metrics, Telemetry}
import sbirds.links.{StuiClient, StuiMessage}
import sbirds.push.{PushMessage, PushService}
import skuaka.shared.Reminders

import java.time.{Instant, LocalDate}
import java.util.UUID
import scala.concurrent.duration.*

/**
 * 予定の通知. プッシュ通知（PWA）と、stui と連携した人には stui のボット「skuaka」からの DM（個人の設定で止められる）:
 *  - 招待: 予定のゲストに加えられた人へ
 *  - リマインダー: 予定に付けた通知（何分前）の時刻に、カレンダーの所有者・共有された人・ゲストへ（欠席の人を除く）.
 *    カレンダーの通知先（hooks）にも積む.
 *    複数ノードで動いても、reminder_sent に先に印を付けたノードだけが送る
 */
final class EventNotifier(events: EventStore,
                          users: UserStore,
                          push: PushService,
                          stui: Option[StuiClient] = None,
                          hooks: Option[HookService] = None,
                          publicUrl: String = "") {

  /** stui の DM（裏で送る. 届かなくても、ほかの通知は止めない. 連携していない人には送らない）. */
  private def dm(uid: UUID, m: StuiMessage): IO[Unit] =
    stui.traverse_(s =>
      s.link.connected(uid).flatMap(IO.whenA(_)(s.notify(uid, m).flatMap {
        case (sbirds.db.OutboxResult.Retry(e), _) => Log.report(ErrorCode.StuiNotifyFailed, e)
        case _ => IO.unit
      })).handleErrorWith(e => Log.report(ErrorCode.StuiNotifyFailed, e.toString, e)).start.void
    )

  /** 招待した人へ知らせる（失敗しても予定の保存は止めない）. */
  def invited(by: User, id: UUID, guests: List[UUID]): IO[Unit] =
    IO.whenA(guests.nonEmpty) {
      Telemetry.span("event.invite", "event.id" -> id.toString)(events.get(id).flatMap(_.traverse_ { r =>
        guests.traverse_(g =>
          users.find(g).flatMap(_.filterNot(_.disabled).traverse_ { t =>
            val body = s"${r.title}\n${When.text(r.span, When.zone(t.timezone))}" +
              (if r.location.nonEmpty then s"\n${r.location}" else "")
            push.send(g, PushMessage(s"${by.name} さんから予定に招待されました", body, "/", Some(s"invite-$id"))) *>
              IO.whenA(t.chatInvites)(dm(
                g,
                StuiMessage(
                  s"📅 ${r.title}",
                  s"${StuiMessage.plain(by.name)} さんから予定に招待されました\n${When.range(r.span, When.zone(t.timezone))}" +
                    (if r.location.nonEmpty then s"\n📍 ${StuiMessage.plain(r.location)}" else ""),
                  HookPayload.link(publicUrl, r, r.span)
                )
              ))
          })
        )
      })).handleErrorWith(e => Log.report(ErrorCode.InvitePushFailed, e.toString, e))
    }

  /** リマインダーを送る裏の処理（30秒ごと. 起動より前の時刻の分は送らない）. */
  def reminderLoop: IO[Nothing] = {
    def go(last: Instant): IO[Nothing] =
      IO.sleep(30.seconds) *> IO.realTimeInstant.flatMap { now =>
        Telemetry.span("reminder.scan")(scan(last, now)).handleErrorWith(e =>
          Log.report(ErrorCode.ReminderScanFailed, e.toString, e)
        ) *> go(now)
      }
    IO.realTimeInstant.flatMap(go)
  }

  /** (last, now] に通知の時刻が来た回を送る. */
  private def scan(last: Instant, now: Instant): IO[Unit] =
    events.overlapping(last, now.plusSeconds(Reminders.MaxMinutes * 60L + 60), timedWithReminders = true).flatMap {
      rows =>
        val due = for {
          r <- rows
          o <- Expand.occurrences(
            r.span,
            r.zone,
            r.recurrence,
            r.exdates.toSet,
            last,
            now.plusSeconds(r.reminders.max * 60L + 1),
            r.zone
          )
          m <- r.reminders
          at = o.bounds(r.zone)._1.minusSeconds(m * 60L)
          if at.isAfter(last) && !at.isAfter(now)
        } yield (r, o, m)
        due.traverse_((r, o, m) => events.markReminder(r.id, o.key, m).ifM(remind(r, o, m), IO.unit))
    }

  private def remind(r: EventRow, o: Span, minutes: Int): IO[Unit] = {
    val when = if minutes == 0 then "" else s"（${Reminders.label(minutes).stripSuffix("前")}後）"
    // 画面を開いているときの通知（Store.reminders）と同じ tag にして、2つ出ないようにする
    val tag = s"${r.id}${o.key}-$minutes"
    events.reminderTargets(r.id, r.calendarId).flatMap(_.traverse_ {
      (uid, tz, chat) =>
        val zone = When.zone(tz)
        val body = s"${When.text(o, zone, Some(LocalDate.now(zone)))}〜$when" +
          (if r.location.nonEmpty then s"\n${r.location}" else "")
        push.send(uid, PushMessage(r.title, body, "/", Some(tag))) *>
          IO.whenA(chat)(dm(
            uid,
            StuiMessage(
              s"🔔 ${r.title}",
              When.range(o, zone) + When.until(minutes) +
                (if r.location.nonEmpty then s"\n📍 ${StuiMessage.plain(r.location)}" else ""),
              HookPayload.link(publicUrl, r, o)
            )
          ))
    }) *> hooks.traverse_(_.reminder(r, o, minutes)) *> Metrics.count("skuaka.reminders.sent")
  }
}
