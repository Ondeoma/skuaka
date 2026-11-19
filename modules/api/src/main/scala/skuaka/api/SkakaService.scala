package skuaka.api

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.client.Client
import org.http4s.headers.Authorization
import skuaka.auth.SecretBox
import skuaka.db.*
import skuaka.core.ErrorCode

import java.util.UUID
import scala.concurrent.duration.*

/**
 * skaka の API を、各ユーザーが許可したトークン（暗号化して保存）で呼ぶ（docs は skaka の docs/skuaka-integration.md）.
 * 401 はトークンの失効なので、保存したトークンを消して「連携が切れた」と扱う.
 */
final class SkakaService(
  val cfg: SkakaConfig,
  http: Client[IO],
  val store: IntegrationStore,
  box: SecretBox) {

  val Provider = "skaka"

  private def api(
    path: String
  ): Uri = Uri.unsafeFromString(cfg.internalUrl.stripSuffix("/") + path)
  private def basic = Authorization(BasicCredentials(cfg.clientId, cfg.clientSecret))
  private def bearer(
    t: String
  ) = Authorization(Credentials.Token(AuthScheme.Bearer, t))

  def token(
    uid: UUID
  ): IO[Option[String]] = store.token(uid, Provider).map(_.flatMap(box.decrypt))
  def connected(
    uid: UUID
  ): IO[Boolean] = token(uid).map(_.isDefined)

  /** 認可コードをトークンに替えて保存する. */
  def exchange(
    uid: UUID,
    code: String,
    verifier: String,
    redirectUri: String
  ): IO[Boolean] = {
    val form = UrlForm(
      "grant_type" -> "authorization_code",
      "code" -> code,
      "redirect_uri" -> redirectUri,
      "code_verifier" -> verifier
    )
    http.run(Request[IO](Method.POST, api("/oauth/token")).withEntity(form).putHeaders(basic))
      .use(r =>
        if r.status.isSuccess then r.as[Json].map(_.hcursor.get[String]("access_token").toOption) else IO.pure(None)
      )
      .timeout(15.seconds).handleError(_ => None)
      .flatMap(_.traverse(t => store.saveToken(uid, Provider, box.encrypt(t))).map(_.isDefined))
  }

  /** 連携をやめる（skaka のトークンも失効させる. skaka に届かなくても、こちらでは消す）. */
  def disconnect(
    uid: UUID
  ): IO[Unit] =
    token(uid).flatMap(_.traverse_ { t =>
      http.run(Request[IO](Method.POST, api("/oauth/revoke")).withEntity(UrlForm("token" -> t)).putHeaders(basic))
        .use(_ => IO.unit).timeout(10.seconds).handleError(_ => ())
    }) *> store.deleteToken(uid, Provider)

  /** トークンで呼ぶ. 401 なら連携を外して Left. op は観測（スパン・メトリクス）での呼び出しの名前. */
  private def call[A](
    op: String,
    uid: UUID,
    req: Request[IO]
  )(
    ok: Response[IO] => IO[A]
  ): IO[Either[String, A]] =
    sbirds.observe.Telemetry.span("skaka.call", "op" -> op)(callImpl(uid, req)(ok)).flatTap(r =>
      sbirds.observe.Metrics.count("skuaka.skaka.calls", "op" -> op, "result" -> (if r.isRight then "ok" else "error"))
    )

  private def callImpl[A](
    uid: UUID,
    req: Request[IO]
  )(
    ok: Response[IO] => IO[A]
  ): IO[Either[String, A]] =
    token(uid).flatMap {
      case None => IO.pure(Left("skaka と連携していません。設定の画面で連携してください"))
      case Some(t) =>
        http.run(req.putHeaders(bearer(t))).use { r =>
          if r.status == Status.Unauthorized then
            store.deleteToken(uid, Provider).as(Left("skaka との連携が切れました。設定の画面で連携し直してください"))
          else if !r.status.isSuccess then IO.pure(Left(s"skaka が ${r.status.code} を返しました"))
          else ok(r).map(Right(_))
        }.timeout(15.seconds).handleErrorWith(e =>
          // 中継先の URL などの詳細はログにだけ出す
          sbirds.observe.Log.report(ErrorCode.SkakaCallFailed, e.toString, e).as(Left("skaka に接続できませんでした"))
        )
    }

  /** 会議室を作り、その URL を返す（名前は予定のタイトル. 100文字まで）. */
  def createRoom(
    uid: UUID,
    ev: SkakaEvent
  ): IO[Either[String, String]] = {
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
  def updateEvent(
    uid: UUID,
    code: String,
    ev: SkakaEvent
  ): IO[Either[String, Unit]] =
    call("room.event.update", uid, Request[IO](Method.PUT, api(s"/api/rooms/$code/event")).withEntity(eventJson(ev)))(
      _ => IO.unit
    )

  private def eventJson(
    ev: SkakaEvent
  ): Json = Json.obj(
    "eventId" -> ev.eventId.asJson,
    "title" -> ev.title.asJson,
    "start" -> ev.start.asJson,
    "end" -> ev.end.asJson,
    "allDay" -> ev.allDay.asJson
  )
}
