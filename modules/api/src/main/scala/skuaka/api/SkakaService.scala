package skuaka.api

import cats.effect.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import sbirds.links.{LinkClient, LinkConfig, LinkFailure}
import skuaka.core.ErrorCode

import java.util.UUID

/**
 * skaka の API を、各ユーザーが許可したトークン（暗号化して保存）で呼ぶ（docs は skaka の docs/skuaka-integration.md）.
 * 連携（トークンの保存・交換・失効）は sbirds.links.LinkClient. 401 はトークンの失効なので「連携が切れた」と扱う.
 */
final class SkakaService(val link: LinkClient) {

  def cfg: LinkConfig = link.cfg

  private def api(path: String): Uri = link.api(path)

  def connected(uid: UUID): IO[Boolean] = link.connected(uid)

  /** トークンで呼ぶ. op は観測（スパン・メトリクス）での呼び出しの名前. */
  private def call[A](op: String, uid: UUID, req: Request[IO])(ok: Response[IO] => IO[A]): IO[Either[String, A]] =
    sbirds.observe.Telemetry.span("skaka.call", "op" -> op)(link.call(uid, req)(ok).flatMap {
      case Right(a) => IO.pure(Right(a))
      case Left(LinkFailure.NotConnected) => IO.pure(Left("skaka と連携していません。設定の画面で連携してください"))
      case Left(LinkFailure.Expired) => IO.pure(Left("skaka との連携が切れました。設定の画面で連携し直してください"))
      case Left(LinkFailure.Rejected(code)) => IO.pure(Left(s"skaka が $code を返しました"))
      // 中継先の URL などの詳細はログにだけ出す
      case Left(LinkFailure.Unreachable(e)) =>
        sbirds.observe.Log.report(ErrorCode.SkakaCallFailed, e.toString, e).as(Left("skaka に接続できませんでした"))
    }).flatTap(r =>
      sbirds.observe.Metrics.count("skuaka.skaka.calls", "op" -> op, "result" -> (if r.isRight then "ok" else "error"))
    )

  /** 会議室を作り、その URL を返す（名前は予定のタイトル. 100文字まで）. */
  def createRoom(uid: UUID, ev: SkakaEvent): IO[Either[String, String]] = {
    val body = Json.obj(
      "name" -> ev.title.take(100).asJson,
      "description" -> "".asJson,
      "guestAccess" -> false.asJson,
      "lobby" -> false.asJson,
      "event" -> eventJson(ev)
    )
    call("room.create", uid, Request[IO](Method.POST, api("/api/rooms")).withEntity(body))(_.as[Json]).map(_.flatMap(
      j =>
        j.hcursor.get[String]("url").left.map(_ => "skaka の応答を読めませんでした")
    ))
  }

  /** 会議室に結び付けた予定の写しを変える（タイトル・日時を変えたとき）. */
  def updateEvent(uid: UUID, code: String, ev: SkakaEvent): IO[Either[String, Unit]] =
    call("room.event.update", uid, Request[IO](Method.PUT, api(s"/api/rooms/$code/event")).withEntity(eventJson(ev)))(
      _ => IO.unit
    )

  private def eventJson(ev: SkakaEvent): Json = Json.obj(
    "eventId" -> ev.eventId.asJson,
    "title" -> ev.title.asJson,
    "start" -> ev.start.asJson,
    "end" -> ev.end.asJson,
    "allDay" -> ev.allDay.asJson
  )
}
