package skuaka.api

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import org.http4s.*
import org.http4s.client.Client
import sbirds.auth.Crypto
import sbirds.db.{OutboxResult, Periodic}
import sbirds.links.{StuiClient, StuiMessage, Webhook}
import sbirds.net.NetGuard
import sbirds.observe.{Log, Metrics}
import skuaka.core.*
import skuaka.db.*
import skuaka.shared.*

import java.util.UUID
import scala.concurrent.duration.*

/**
 * カレンダーの通知先（docs/design.md の「通知先」）:
 *  - 予定のリマインダー・追加・変更・削除を、そのカレンダーの通知先の送信待ちに積む（enqueue・changed・reminder）
 *  - 送信待ちを送る（loop. 複数ノードでも SKIP LOCKED で重ならない. 失敗したら間を空けて送り直す）
 *  - 保存する通知先を確かめる（check）
 * http は送り先を確かめるクライアント（NetGuard.client(policy)）. policy は WEBHOOK_ALLOW_INTERNAL と stui のホストを許したもの.
 */
final class HookService(val store: HookStore,
                        stui: Option[StuiClient],
                        http: Client[IO],
                        policy: NetGuard.Policy,
                        publicUrl: String,
                        allowHttp: Boolean = false) {

  def stuiConfigured: Boolean = stui.isDefined

  // ---------------------------------------------------------------- 積む

  /** そのカレンダーの通知先の送信待ちに積む（失敗しても予定の保存は止めない）. */
  def enqueue(calendarId: UUID, kind: String, payload: Json): IO[Unit] =
    store.enqueue(calendarId, kind, payload).flatMap(n =>
      IO.whenA(n > 0)(Metrics.add("skuaka.hooks.enqueued", n.toLong, "event" -> kind))
    ).handleErrorWith(e => Log.report(ErrorCode.HookEnqueueFailed, e.toString, e))

  private def calendar(id: UUID): IO[(UUID, String)] = store.calendarName(id).map(n => id -> n.getOrElse(""))

  /** 予定の追加・変更・削除（r は変えた後の予定. 削除なら消す前の予定）. */
  def changed(actor: User, kind: String, r: EventRow, scope: Option[String] = None): IO[Unit] =
    calendar(r.calendarId).flatMap(c =>
      enqueue(r.calendarId, kind, HookPayload.of(kind, c, r, r.span, publicUrl, Some(actor), scope = scope))
    ).handleErrorWith(e => Log.report(ErrorCode.HookEnqueueFailed, e.toString, e))

  /** リマインダー（o はその回、minutes は何分前）. */
  def reminder(r: EventRow, o: Span, minutes: Int): IO[Unit] =
    calendar(r.calendarId).flatMap(c =>
      enqueue(
        r.calendarId,
        HookEvent.Reminder,
        HookPayload.of(HookEvent.Reminder, c, r, o, publicUrl, minutes = Some(minutes))
      )
    ).handleErrorWith(e => Log.report(ErrorCode.HookEnqueueFailed, e.toString, e))

  /** 「テストを送る」（その通知先だけに積む）. */
  def ping(h: HookRow, actor: User): IO[Unit] =
    calendar(h.calendarId).flatMap(c => store.enqueueOne(h.id, HookEvent.Ping, HookPayload.ping(c, actor, publicUrl)))

  // ---------------------------------------------------------------- 送る

  def send(d: HookDelivery): IO[(OutboxResult, Option[Int])] = {
    val h = d.hook
    sbirds.observe.Telemetry.span("hook.deliver", "hook.kind" -> h.kind, "hook.event" -> d.event)(h.kind match {
      case HookKind.Stui => sendStui(h, HookPayload.message(d.payload))
      case HookKind.StuiHook =>
        guarded(stuiInternal(h.url))(u => StuiClient.postHook(http, u, HookPayload.message(d.payload), "skuaka"))
      case _ =>
        Webhook.post(http, policy, "Skuaka", Webhook.Send(h.url, d.event, d.id.toString, d.payload.noSpaces, h.secret))
    }).flatTap((r, _) =>
      Metrics.count(
        "skuaka.hooks.sent",
        "kind" -> h.kind,
        "result" ->
          (r match {
            case OutboxResult.Sent => "ok"
            case _: OutboxResult.Retry => "retry"
            case _ => "skip"
          })
      )
    )
  }

  private def sendStui(h: HookRow, m: StuiMessage): IO[(OutboxResult, Option[Int])] =
    (stui, h.ownerId, h.channelId.filter(_.nonEmpty)) match {
      case (None, _, _) => IO.pure((OutboxResult.Skip("stui との連携が設定されていません（STUI_URL など）"), None))
      case (_, None, _) => IO.pure((OutboxResult.Skip("設定した人がいません（通知先を設定し直してください）"), None))
      case (_, _, None) => IO.pure((OutboxResult.Skip("チャンネルが選ばれていません"), None))
      case (Some(s), Some(owner), Some(ch)) =>
        s.notify(owner, m, Some(ch)).map {
          case (OutboxResult.Skip(r), code) if code.isEmpty || code.contains(401) =>
            (OutboxResult.Skip(s"$r（設定した人の連携が必要です）"), code)
          case other => other
        }
    }

  /** 送る前に送り先を確かめる（内部のアドレスには送らない）. */
  private def guarded(url: String)(f: String => IO[(OutboxResult, Option[Int])]): IO[(OutboxResult, Option[Int])] =
    NetGuard.check(url, policy).flatMap {
      case Left(r) => IO.pure((OutboxResult.Skip(r.message), None))
      case Right(_) => f(url).handleError {
          case r: NetGuard.Rejected => (OutboxResult.Skip(r.message), None)
          case e => (OutboxResult.Retry(s"接続できませんでした（${e.getClass.getSimpleName}）"), None)
        }
    }

  /** stui の受信 Webhook の URL が STUI_URL で始まるなら、サーバーから呼ぶ URL（STUI_INTERNAL_URL）に替える. */
  private def stuiInternal(url: String): String =
    stui.map(_.link.cfg).filter(c => url.startsWith(c.url + "/")).fold(url)(c => c.internalUrl + url.drop(c.url.length))

  def loop: IO[Nothing] = Periodic.run(10.seconds, 10.seconds, ErrorCode.HookWorkerFailed)(store.process(50)(send))

  def purgeLoop: IO[Nothing] = Periodic.run(1.hour, 24.hours, ErrorCode.HookPurgeFailed)(store.purge(30))

  // ---------------------------------------------------------------- 保存の確認

  /**
   * 保存する通知先を確かめて整える. (中身, secret).
   *  - stui: 保存した人が stui と連携していて、そのチャンネルに参加していること（その人の連携で投稿する）.
   *    チャンネルを変えずに、ほかの人が名前・送るできごとなどを変えたときは、もとの人のまま
   *  - stui_hook: stui の受信 Webhook の URL（/hooks/…）
   *  - url: https の URL（WEBHOOK_ALLOW_HTTP なら http も）. secret は署名の鍵（種類を変えたときに作る）
   * URL は、変えるときに空なら今の URL のまま. 内部のアドレスは WEBHOOK_ALLOW_INTERNAL に書いたものだけ.
   */
  def check(u: User, r: HookReq, old: Option[HookRow]): IO[Either[String, (HookSave, String)]] = {
    val events = HookEvent.all.filter(r.events.contains)
    val name = r.name.trim
    val keptUrl = old.filter(_.kind == r.kind).map(_.url).getOrElse("")
    val url = Option(r.url.trim).filter(_.nonEmpty).getOrElse(keptUrl)
    val secret = old.filter(_.kind == r.kind).map(_.secret).filter(_.nonEmpty).getOrElse(Crypto.randomToken(24))
    def save(n: String, url: String = "", owner: Option[UUID] = None, ch: Option[(String, String)] = None) =
      HookSave(r.kind, n.take(100), url, events, r.enabled, owner, ch.map(_._1), ch.map(_._2))
    if !HookKind.all.contains(r.kind) then IO.pure(Left("通知先の種類が正しくありません"))
    else if events.isEmpty then IO.pure(Left("送るできごとを1つ以上選んでください"))
    else if name.length > 100 then IO.pure(Left("名前は100文字以内にしてください"))
    else
      r.kind match {
        case HookKind.Stui => checkStui(u, r, old).map(_.map((owner, ch) =>
            (save(Option(name).filter(_.nonEmpty).getOrElse(s"stui #${ch._2}"), owner = Some(owner), ch = Some(ch)), "")
          ))
        case HookKind.StuiHook =>
          if !StuiClient.isHookUrl(url) then
            IO.pure(Left("stui の受信 Webhook の URL（…/hooks/…）を入れてください（stui のチャンネルの設定 →「受信 Webhook」で作れます）"))
          else checkUrl(url).map(_.as((save(Option(name).filter(_.nonEmpty).getOrElse("stui の受信 Webhook"), url), "")))
        case _ =>
          checkUrl(url).map(_.map(_ =>
            (
              save(Option(name).filter(_.nonEmpty).getOrElse(Uri.unsafeFromString(url).host.fold(url)(_.value)), url),
              secret
            )
          ))
      }
  }

  private def checkStui(u: User, r: HookReq, old: Option[HookRow]): IO[Either[String, (UUID, (String, String))]] = {
    val ch = r.channelId.map(_.trim).filter(_.nonEmpty)
    val kept = old.filter(o => o.kind == HookKind.Stui && o.channelId == ch && o.ownerId.isDefined)
    (stui, ch, kept) match {
      case (None, _, _) => IO.pure(Left("stui との連携が設定されていません"))
      case (_, None, _) => IO.pure(Left("チャンネルを選んでください"))
      case (_, _, Some(o)) => IO.pure(Right((o.ownerId.get, (ch.get, o.channelName.getOrElse("")))))
      case (Some(s), Some(id), None) =>
        s.channels(u.id).map {
          case Left(sbirds.links.LinkFailure.NotConnected) => Left("stui と連携してから選んでください（設定の画面で連携できます）")
          case Left(sbirds.links.LinkFailure.Expired) => Left("stui との連携が切れました。設定の画面で連携し直してください")
          case Left(_) => Left("stui のチャンネルを読めませんでした")
          case Right(cs) => cs.find(_.id == id).toRight("そのチャンネルに参加していません").map(c => (u.id, (c.id, c.name)))
        }
    }
  }

  private def checkUrl(url: String): IO[Either[String, Unit]] = {
    val lower = url.toLowerCase
    // STUI_URL で始まる URL（stui の受信 Webhook）は、STUI_URL が http でも許す
    val ownStui = stui.exists(s => url.startsWith(s.link.cfg.url + "/"))
    if url.isEmpty then IO.pure(Left("URL を入れてください"))
    else if url.length > 2000 then IO.pure(Left("URL が長すぎます"))
    else if !(lower.startsWith("https://") || ((allowHttp || ownStui) && lower.startsWith("http://"))) then
      IO.pure(Left(if allowHttp then "URL は http(s):// で始めてください" else "URL は https:// で始めてください"))
    else
      Uri.fromString(url).toOption.filter(_.host.isDefined) match {
        case None => IO.pure(Left("URL が正しくありません"))
        case Some(_) => NetGuard.check(url, policy).map(_.left.map(_.message))
      }
  }
}
