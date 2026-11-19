package skuaka.api

import cats.data.OptionT
import cats.effect.*
import cats.syntax.all.*
import io.circe.Decoder
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl
import org.typelevel.ci.CIString
import org.http4s.headers.{`WWW-Authenticate`, Authorization}
import org.http4s.server.middleware.EntityLimiter
import skuaka.auth.{ApiTokens, Crypto}
import skuaka.db.*
import skuaka.shared.*

import java.time.Instant
import java.util.UUID
import scala.concurrent.duration.*

final case class ApiConfig(
    cookieSecure: Boolean,
    publicUrl: String,
    sessionTtl: FiniteDuration = 14.days,
    cookieName: String = "skuaka_session",
    devLogin: Boolean = false,
    rateLimit: RateLimitConfig = RateLimitConfig(),
    version: String = "0.1.0",
    oauthClients: List[OAuthClient] = Nil
)

/** 認可を求める兄弟のサービス（OAUTH_CLIENTS で登録する）. redirectUri は完全一致で照合する. scopes は許すスコープ（read・write）. */
final case class OAuthClient(id: String, secret: String, redirectUri: String, name: String, scopes: List[String] = List(TokenScope.Read))

/** サーバーで組み立てる HTML の文字の置き換え. */
object Html:
  def escape(s: String): String = s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\"", "&quot;").replace("'", "&#39;")

/** 誰の要求か. token は API トークンで呼ばれたときの (トークンID, スコープ). */
final case class Principal(user: User, token: Option[(UUID, String)]):
  def canWrite: Boolean = token.forall(_._2 == TokenScope.Write)

/** 認証: Cookie のセッション、または `Authorization: Bearer <API トークン>`（どちらも DB にはハッシュだけ保存する）. */
final class SessionAuth(users: UserStore, cfg: ApiConfig):

  /** Bearer の値（API トークンの形のものだけ）. */
  def bearer(req: Request[IO]): Option[String] =
    req.headers.get[Authorization].collect { case Authorization(Credentials.Token(AuthScheme.Bearer, t)) => t.trim }

  def fromRequest(req: Request[IO]): IO[Option[Principal]] =
    bearer(req) match
      case Some(t) if ApiTokens.looksLike(t) =>
        users.findToken(Crypto.sha256(t), Instant.now()).map(_.map((u, id, scope) => Principal(u, Some(id -> scope))))
      case Some(_) => IO.pure(None)
      case None =>
        req.cookies.find(_.name == cfg.cookieName).map(_.content).filter(_.nonEmpty) match
          case None        => IO.pure(None)
          case Some(token) => users.findSession(Crypto.sha256(token), Instant.now()).map(_.map(Principal(_, None)))

  def issue(user: User, idToken: Option[String]): IO[ResponseCookie] =
    val token = Crypto.randomToken()
    users.createSession(Crypto.sha256(token), user.id, idToken, Instant.now().plusSeconds(cfg.sessionTtl.toSeconds))
      .as(cookie(token, cfg.sessionTtl.toSeconds))

  /** セッションを消す. IdP のログアウトに渡す ID トークンと、Cookie を消す指示を返す. */
  def revoke(req: Request[IO]): IO[(Option[String], ResponseCookie)] =
    req.cookies.find(_.name == cfg.cookieName) match
      case Some(c) => users.deleteSession(Crypto.sha256(c.content)).map(_ -> cookie("", 0))
      case None    => IO.pure(None -> cookie("", 0))

  private def cookie(value: String, maxAge: Long): ResponseCookie =
    ResponseCookie(cfg.cookieName, value, maxAge = Some(maxAge), path = Some("/"), httpOnly = true, secure = cfg.cookieSecure,
      sameSite = Some(SameSite.Lax))

/** 文字列の応答（JSON 以外. circe の EntityEncoder に取られないよう明示する）. */
object TextResponse:
  def apply(status: Status, body: String, media: MediaType = MediaType.text.plain): Response[IO] =
    Response[IO](status).withEntity(body)(using EntityEncoder.stringEncoder).withContentType(headers.`Content-Type`(media, Charset.`UTF-8`))

/** *Routes に共通の補助（エラー応答・認証・リクエスト本文）. */
trait RouteSupport extends Http4sDsl[IO]:
  protected def sessions: SessionAuth

  protected def err(status: Status, msg: String): IO[Response[IO]] =
    Response[IO](status).withEntity(ErrorDto(msg)).pure[IO]

  /** 画面からの更新の要求に付けるヘッダー. 他のサイトのフォームからは付けられない（CORS の事前確認が要る）ので CSRF を防げる. */
  private val csrfHeader = CIString("X-Skuaka")

  private def readOnly(req: Request[IO]): Boolean = Set(Method.GET, Method.HEAD).contains(req.method)

  /**
   * ログインしていなければ 401.
   * Cookie のときは、GET 以外に X-Skuaka ヘッダーが必要. API トークン（Bearer）はブラウザが自動では付けないので CSRF の確認は要らないが、
   * 参照のみのトークンでは GET しかできない.
   */
  protected def authed(req: Request[IO])(f: User => IO[Response[IO]]): IO[Response[IO]] =
    principal(req)(p => if p.canWrite || readOnly(req) then f(p.user) else err(Status.Forbidden, "参照のみの API トークンでは変更できません"))

  /** 画面（Cookie のセッション）からだけ受け付ける操作（API トークンの管理など）. */
  protected def interactive(req: Request[IO])(f: User => IO[Response[IO]]): IO[Response[IO]] =
    principal(req)(p => if p.token.isEmpty then f(p.user) else err(Status.Forbidden, "この操作は API トークンではできません"))

  private def principal(req: Request[IO])(f: Principal => IO[Response[IO]]): IO[Response[IO]] =
    val bearer = sessions.bearer(req).isDefined
    if !bearer && !readOnly(req) && req.headers.get(csrfHeader).isEmpty then err(Status.Forbidden, "画面以外からの更新の要求は受け付けません")
    else
      sessions.fromRequest(req).flatMap {
        // 操作したユーザーを、トレース（スパンの属性）とこの中のログに載せる
        case Some(p) => skuaka.observe.Telemetry.withUser(p.user.id.toString, p.user.name)(f(p))
        case None if bearer =>
          err(Status.Unauthorized, "API トークンが正しくないか、期限が切れています").map(_.putHeaders(`WWW-Authenticate`(Challenge("Bearer", "skuaka"))))
        case None => err(Status.Unauthorized, "ログインしてください")
      }

  protected def body[A: Decoder](req: Request[IO])(f: A => IO[Response[IO]]): IO[Response[IO]] =
    req.as[A].attempt.flatMap {
      case Left(_: EntityLimiter.EntityTooLarge) => err(Status.PayloadTooLarge, ApiRoutes.TooLargeMessage)
      case Left(e)  => err(Status.BadRequest, s"リクエストの内容が正しくありません: ${e.getMessage.take(200)}")
      case Right(b) => f(b)
    }

  protected def param(req: Request[IO], k: String): Option[String] = req.params.get(k).map(_.trim).filter(_.nonEmpty)

  protected object UuidVar:
    def unapply(s: String): Option[UUID] = scala.util.Try(UUID.fromString(s)).toOption

  /** 結果が Right なら f、Left なら 400. */
  protected def valid[A](e: Either[String, A])(f: A => IO[Response[IO]]): IO[Response[IO]] =
    e.fold(err(Status.BadRequest, _), f)

/** 入力の確認. */
object Check:
  private val colorRe = "^#[0-9a-fA-F]{6}$".r

  def color(c: String): Either[String, String] = Either.cond(colorRe.matches(c), c.toLowerCase, "色は #rrggbb で指定してください")

  def text(v: String, what: String, max: Int): Either[String, String] =
    Either.cond(v.length <= max, v, s"$what は $max 文字以内にしてください")

/**
 * すべての応答に付けるセキュリティのヘッダー. CSP は応答が自分で付けていなければ、何も読み込ませないもの
 * （API・エラーの画面向け. 画面の配信は画面に合わせた CSP を付ける）. hsts は HTTPS で公開しているとき.
 */
object SecurityHeaders:
  val StrictCsp = "default-src 'none'; style-src 'unsafe-inline'; frame-ancestors 'none'; base-uri 'none'; form-action 'none'"

  def apply(routes: HttpRoutes[IO], hsts: Boolean): HttpRoutes[IO] =
    val fixed = List[Header.ToRaw]("X-Content-Type-Options" -> "nosniff", "X-Frame-Options" -> "DENY",
      "Referrer-Policy" -> "strict-origin-when-cross-origin") ++
      Option.when(hsts)("Strict-Transport-Security" -> "max-age=31536000").toList.map(h => h: Header.ToRaw)
    routes.map { r =>
      val withCsp = if r.headers.get(CIString("Content-Security-Policy")).isDefined then r else r.putHeaders("Content-Security-Policy" -> StrictCsp)
      withCsp.putHeaders(fixed*)
    }

/** アプリのルートをまとめる（REST API・iCalendar の配信・MCP・プッシュ通知の購読・兄弟のサービスへの認可（OAuth））. MCP は REST API をトークンで内部から呼ぶ. */
object ApiRoutes:
  def apply(oidc: Option[skuaka.auth.OidcClient], users: UserStore, calendars: CalendarStore, events: EventStore, sess: SessionAuth,
      cfg: ApiConfig, skaka: Option[SkakaService] = None, push: Option[(skuaka.push.PushStore, skuaka.push.PushService)] = None,
      notifier: Option[EventNotifier] = None, oauth: Option[OAuthStore] = None, importer: Option[CalendarImporter] = None): HttpRoutes[IO] =
    val calendarRoutes = new CalendarRoutes(calendars, events, users, sess, cfg, importer)
    val rest = new AuthRoutes(oidc, users, calendars, sess, cfg, skaka.map(_.cfg.url)).routes <+>
      calendarRoutes.routes <+>
      new EventRoutes(events, calendars, users, sess, skaka, notifier).routes <+>
      new SkakaRoutes(skaka, sess, cfg).routes <+>
      oauth.fold(HttpRoutes.empty[IO])(new OAuthRoutes(_, sess, cfg).routes) <+>
      push.fold(HttpRoutes.empty[IO])((store, service) => new PushRoutes(store, service, sess).routes)
    // 本文は MaxBody まで（大きな本文でメモリを使い切らせない. アイコンの 1MB より大きくしておく）.
    // Content-Length が超えていれば、EntityLimiter はルートに入る前に例外にするので、ここで 413 にする
    // iCalendar のファイルの取り込みだけは、本文を IcsImport.MaxBytes まで受ける
    val limited = EntityLimiter.httpRoutes(calendarRoutes.importRoutes, IcsImport.MaxBytes.toLong) <+>
      EntityLimiter.httpRoutes(rest <+> new McpRoutes(rest, sess, cfg.publicUrl, cfg.version).routes, MaxBody)
    val all = HttpRoutes[IO](req => limited(req).recoverWith { case _: EntityLimiter.EntityTooLarge => OptionT.liftF(tooLarge) })
    SecurityHeaders(RateLimit(all, cfg.rateLimit), cfg.cookieSecure)

  val MaxBody: Long = 2L * 1024 * 1024

  private def tooLarge: IO[Response[IO]] = IO.pure(Response[IO](Status.PayloadTooLarge).withEntity(ErrorDto(TooLargeMessage)))
  val TooLargeMessage = "リクエストの本文が大きすぎます"
