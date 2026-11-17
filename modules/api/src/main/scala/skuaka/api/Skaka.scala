package skuaka.api

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.client.Client
import org.http4s.headers.{Authorization, Location}
import skuaka.auth.{Crypto, SecretBox}
import skuaka.db.*
import skuaka.shared.*

import java.time.Instant
import java.util.UUID
import scala.concurrent.duration.*

/**
 * Web 会議（skaka）との連携の設定. url は画面から開く skaka の URL（会議室の URL もこれで始まる）、
 * internalUrl はサーバーから呼ぶ URL（省けば url）.
 */
final case class SkakaConfig(url: String, internalUrl: String, clientId: String, clientSecret: String)

/** 会議室の結び付ける予定の写し（skaka の RoomEventDto と同じ形）. */
final case class SkakaEvent(eventId: UUID, title: String, start: String, end: String, allDay: Boolean)

/**
 * skaka の API を、各ユーザーが許可したトークン（暗号化して保存）で呼ぶ（docs は skaka の docs/skuaka-integration.md）.
 * 401 はトークンの失効なので、保存したトークンを消して「連携が切れた」と扱う.
 */
final class SkakaService(val cfg: SkakaConfig, http: Client[IO], val store: IntegrationStore, box: SecretBox):

  val Provider = "skaka"

  private def api(path: String): Uri = Uri.unsafeFromString(cfg.internalUrl.stripSuffix("/") + path)
  private def basic = Authorization(BasicCredentials(cfg.clientId, cfg.clientSecret))
  private def bearer(t: String) = Authorization(Credentials.Token(AuthScheme.Bearer, t))

  def token(uid: UUID): IO[Option[String]] = store.token(uid, Provider).map(_.flatMap(box.decrypt))
  def connected(uid: UUID): IO[Boolean] = token(uid).map(_.isDefined)

  /** 認可コードをトークンに替えて保存する. */
  def exchange(uid: UUID, code: String, verifier: String, redirectUri: String): IO[Boolean] =
    val form = UrlForm("grant_type" -> "authorization_code", "code" -> code, "redirect_uri" -> redirectUri, "code_verifier" -> verifier)
    http.run(Request[IO](Method.POST, api("/oauth/token")).withEntity(form).putHeaders(basic))
      .use(r => if r.status.isSuccess then r.as[Json].map(_.hcursor.get[String]("access_token").toOption) else IO.pure(None))
      .timeout(15.seconds).handleError(_ => None)
      .flatMap(_.traverse(t => store.saveToken(uid, Provider, box.encrypt(t))).map(_.isDefined))

  /** 連携をやめる（skaka のトークンも失効させる. skaka に届かなくても、こちらでは消す）. */
  def disconnect(uid: UUID): IO[Unit] =
    token(uid).flatMap(_.traverse_ { t =>
      http.run(Request[IO](Method.POST, api("/oauth/revoke")).withEntity(UrlForm("token" -> t)).putHeaders(basic))
        .use(_ => IO.unit).timeout(10.seconds).handleError(_ => ())
    }) *> store.deleteToken(uid, Provider)

  /** トークンで呼ぶ. 401 なら連携を外して Left. op は観測（スパン・メトリクス）での呼び出しの名前. */
  private def call[A](op: String, uid: UUID, req: Request[IO])(ok: Response[IO] => IO[A]): IO[Either[String, A]] =
    skuaka.observe.Telemetry.span("skaka.call", "op" -> op)(callImpl(uid, req)(ok)).flatTap(r =>
      skuaka.observe.Metrics.count("skuaka.skaka.calls", "op" -> op, "result" -> (if r.isRight then "ok" else "error")))

  private def callImpl[A](uid: UUID, req: Request[IO])(ok: Response[IO] => IO[A]): IO[Either[String, A]] =
    token(uid).flatMap {
      case None => IO.pure(Left("skaka と連携していません。設定の画面で連携してください"))
      case Some(t) =>
        http.run(req.putHeaders(bearer(t))).use { r =>
          if r.status == Status.Unauthorized then store.deleteToken(uid, Provider).as(Left("skaka との連携が切れました。設定の画面で連携し直してください"))
          else if !r.status.isSuccess then IO.pure(Left(s"skaka が ${r.status.code} を返しました"))
          else ok(r).map(Right(_))
        }.timeout(15.seconds).handleErrorWith(e =>
          // 中継先の URL などの詳細はログにだけ出す
          skuaka.observe.Log.println(s"[skuaka] skaka call failed: $e").as(Left("skaka に接続できませんでした")))
    }

  /** 会議室を作り、その URL を返す（名前は予定のタイトル. 100文字まで）. */
  def createRoom(uid: UUID, ev: SkakaEvent): IO[Either[String, String]] =
    val body = Json.obj("name" -> ev.title.take(100).asJson, "description" -> "".asJson, "guestAccess" -> false.asJson, "lobby" -> false.asJson,
      "event" -> eventJson(ev))
    call("room.create", uid, Request[IO](Method.POST, api("/api/rooms")).withEntity(body))(_.as[Json]).map(_.flatMap(j =>
      j.hcursor.get[String]("url").left.map(_ => "skaka の応答を読めませんでした")))

  /** 会議室に結び付けた予定の写しを変える（タイトル・日時を変えたとき）. */
  def updateEvent(uid: UUID, code: String, ev: SkakaEvent): IO[Either[String, Unit]] =
    call("room.event.update", uid, Request[IO](Method.PUT, api(s"/api/rooms/$code/event")).withEntity(eventJson(ev)))(_ => IO.unit)

  private def eventJson(ev: SkakaEvent): Json = Json.obj("eventId" -> ev.eventId.asJson, "title" -> ev.title.asJson, "start" -> ev.start.asJson,
    "end" -> ev.end.asJson, "allDay" -> ev.allDay.asJson)

/** 連携の開始・戻り・やめる・状態（/api/integrations/skaka/…）. */
final class SkakaRoutes(skaka: Option[SkakaService], val sessions: SessionAuth, cfg: ApiConfig) extends RouteSupport:

  private val callback = s"${cfg.publicUrl}/api/integrations/skaka/callback"

  private def redirect(to: String): IO[Response[IO]] =
    IO.pure(Response[IO](Status.Found).putHeaders(Location(Uri.unsafeFromString(to)), "Cache-Control" -> "no-store"))

  /** 戻り先（同じサイトのパスだけ）に結果を付けて戻る. */
  private def done(returnTo: String, result: String): IO[Response[IO]] =
    val back = SafePath.of(Option(returnTo), "/settings")
    redirect(back + (if back.contains("?") then "&" else "?") + s"integration=skaka&result=$result")

  private def enabled(f: SkakaService => IO[Response[IO]]): IO[Response[IO]] =
    skaka.fold(err(Status.NotFound, "skaka との連携は設定されていません"))(f)

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "api" / "integrations" / "skaka" =>
      authed(req)(u => skaka.fold(Ok(SkakaStatusDto(false, false, None)))(s => s.connected(u.id).flatMap(c => Ok(SkakaStatusDto(true, c, Some(s.cfg.url))))))

    // 連携を始める（skaka の許可の画面へ）. 画面のリンクから開く
    case req @ GET -> Root / "api" / "integrations" / "skaka" / "connect" =>
      interactive(req)(u => enabled { s =>
        val state    = Crypto.randomToken(24)
        val verifier = Crypto.randomToken(48)
        s.store.saveState(state, u.id, s.Provider, verifier, param(req, "returnTo").getOrElse("/settings"), Instant.now().plusSeconds(600)) *>
          redirect(Uri.unsafeFromString(s"${s.cfg.url.stripSuffix("/")}/oauth/authorize").withQueryParams(Map(
            "response_type" -> "code", "client_id" -> s.cfg.clientId, "redirect_uri" -> callback, "scope" -> "read write", "state" -> state,
            "code_challenge" -> Crypto.pkceChallenge(verifier), "code_challenge_method" -> "S256")).renderString)
      })

    // 許可の画面から戻ったとき: コードをトークンに替えて保存する
    case req @ GET -> Root / "api" / "integrations" / "skaka" / "callback" =>
      interactive(req)(u => enabled { s =>
        param(req, "state").fold(IO.pure(None))(st => s.store.takeState(st, s.Provider)).flatMap {
          case None                                  => done("/settings", "error")
          case Some((uid, _, back)) if uid != u.id   => done(back, "error")
          case Some((_, _, back)) if param(req, "error").isDefined =>
            done(back, if param(req, "error").contains("access_denied") then "denied" else "error")
          case Some((_, verifier, back)) =>
            param(req, "code").fold(done(back, "error"))(code =>
              s.exchange(u.id, code, verifier, callback).ifM(done(back, "connected"), done(back, "error")))
        }
      })

    case req @ DELETE -> Root / "api" / "integrations" / "skaka" / "connection" =>
      interactive(req)(u => enabled(s => s.disconnect(u.id) *> NoContent()))
  }
