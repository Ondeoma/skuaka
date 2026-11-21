package skuaka.api

import sbirds.shared.SafePath
import cats.effect.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import sbirds.auth.{Crypto, LoginResult, OidcClient}
import sbirds.http.OidcLoginFlow
import skuaka.auth.ApiKeys
import skuaka.db.*
import skuaka.shared.*
import skuaka.core.ErrorCode

import java.time.Instant
import java.util.UUID

/** ログイン（OIDC・開発用）・ログアウト・自分の情報とアイコン・ユーザーの検索. */
final class AuthRoutes(oidc: Option[OidcClient],
                       users: UserStore,
                       calendars: CalendarStore,
                       val sessions: SessionAuth,
                       cfg: ApiConfig,
                       skakaUrl: Option[String] = None)
  extends RouteSupport {

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "auth" / "login" => getAuthLogin(req)
    case req @ GET -> Root / "auth" / "callback" => getAuthCallback(req)
    case req @ POST -> Root / "auth" / "logout" => postAuthLogout(req)
    case req @ POST -> Root / "auth" / "dev-login" if cfg.devLogin && oidc.isEmpty => postAuthDevLogin(req)
    case GET -> Root / "api" / "client-config" => getClientConfig()
    case req @ GET -> Root / "api" / "me" => getMe(req)
    case req @ PATCH -> Root / "api" / "me" => patchMe(req)
    case req @ PUT -> Root / "api" / "me" / "avatar" => putMeAvatar(req)
    case req @ DELETE -> Root / "api" / "me" / "avatar" => deleteMeAvatar(req)
    case req @ GET -> Root / "api" / "users" / UuidVar(id) / "avatar" => getUserAvatar(req, id)
    case req @ GET -> Root / "api" / "users" => getUsers(req)
    case req @ GET -> Root / "api" / "tokens" => getTokens(req)
    case req @ POST -> Root / "api" / "tokens" => postTokens(req)
    case req @ DELETE -> Root / "api" / "tokens" / UuidVar(id) => deleteToken(req, id)
  }

  private def getAuthLogin(req: Request[IO]): IO[Response[IO]] = OidcLoginFlow.login(flow)(req)

  private def getAuthCallback(req: Request[IO]): IO[Response[IO]] = OidcLoginFlow.callback(flow)(req)

  /** OIDC のログインの流れ（sbirds.http.OidcLoginFlow）に差し込む、skuaka の部分. */
  private val flow = new OidcLoginFlow.Host[LoginResult] {
    val idp = oidc.map(OidcLoginFlow.Idp(_))

    /**
     * ログインを始めたブラウザの印（state）. IdP から戻ったとき、同じブラウザかを確かめる
     * （攻撃者が途中で止めたコールバックの URL を踏ませて、攻撃者のアカウントでログインさせる「ログイン CSRF」を防ぐ）.
     */
    val cookie = OidcLoginFlow.StateCookie("skuaka_login", cfg.cookieSecure, maxAge = 600)
    override def idpErrorParam: Boolean = true

    def returnTo(req: Request[IO]): String = SafePath.of(param(req, "returnTo"))
    def saveState(state: String, verifier: String, nonce: String, returnTo: String): IO[Unit] =
      users.saveLoginState(state, verifier, nonce, returnTo)
    def takeState(state: String): IO[Option[(String, String, String)]] = users.takeLoginState(state)
    def redirect(to: String): IO[Response[IO]] = AuthRoutes.this.redirect(to)

    def withoutIdp(req: Request[IO], back: String): IO[Response[IO]] =
      if cfg.devLogin then AuthRoutes.this.redirect(s"/login?returnTo=${java.net.URLEncoder.encode(back, "UTF-8")}")
      else err(ErrorCode.OidcNotConfigured)

    def failed(req: Request[IO], f: OidcLoginFlow.Failure): IO[Response[IO]] = {
      import OidcLoginFlow.Failure.*
      f match {
        // IdP の error・error_description は URL で差し替えられるので画面には出さず、ログにだけ残す
        case IdpError(e, d) =>
          sbirds.observe.Log.report(ErrorCode.LoginRejectedByIdp, OidcLoginFlow.idpErrorLog(e, d)) *>
            page("ログインできませんでした", ErrorCode.LoginRejectedByIdp.message, Status.Unauthorized)
        case BrowserMismatch => page("ログインできませんでした", "ログインを始めたブラウザと違います。もう一度ログインしてください。", Status.BadRequest)
        case Expired => page("ログインできませんでした", "ログインの有効期限が切れました。もう一度ログインしてください。", Status.BadRequest)
        // 理由の詳細（検証に失敗した項目など）はログにだけ出す
        case Denied(e) => page("ログインできませんでした", e.getMessage, Status.Forbidden)
        case Failed(e) =>
          sbirds.observe.Log.report(ErrorCode.LoginFailed, e.getMessage) *>
            page("ログインできませんでした", "IdP からの応答を確かめられませんでした。時間をおいてもう一度試してください。", Status.Unauthorized)
        case NotConfigured => err(ErrorCode.OidcNotConfigured)
        case ParamsMissing => err(ErrorCode.LoginCodeStateRequired)
      }
    }

    // IdP で確かめた後（認可コードを ID トークンに交換して、署名・iss・aud・exp・nonce を確かめた後）
    def loggedIn(req: Request[IO], res: LoginResult, back: String): IO[Response[IO]] = {
      val id = res.identity
      users.upsert(
        id.sub,
        id.name,
        id.email,
        id.picture.filter(p => p.startsWith("https://") || p.startsWith("http://"))
      ).flatMap { u =>
        if u.disabled then page("ログインできませんでした", "このユーザーは無効にされています。", Status.Forbidden)
        else AuthRoutes.this.loggedIn(u, Some(res.idToken)).flatMap(c => AuthRoutes.this.redirect(back, Some(c)))
      }
    }
  }

  // ログアウト: セッションを消し、IdP にもログアウトを頼む（end_session_endpoint があれば）
  private def postAuthLogout(req: Request[IO]): IO[Response[IO]] =
    sessions.revoke(req).flatMap {
      (idToken, c) =>
        OidcLoginFlow.logoutRedirect(oidc, idToken, cfg.publicUrl + "/", "/login?loggedOut=1").flatMap { to =>
          Ok(Json.obj("redirect" -> to.asJson)).map(_.addCookie(c))
        }
    }

  // 開発用: 名前だけでログインする（DEV_LOGIN=true で OIDC を設定していないときだけ）
  private def postAuthDevLogin(req: Request[IO]): IO[Response[IO]] =
    body[Json](req) { j =>
      val c = j.hcursor
      val name = c.get[String]("name").getOrElse("").trim
      if name.isEmpty then err(ErrorCode.NameRequired)
      else
        for {
          u <- users.upsert(
            s"dev:$name",
            name,
            c.get[String]("email").toOption.map(_.trim).filter(_.contains("@")),
            None
          )
          ck <- loggedIn(u, None)
          r <- Ok(Json.obj("redirect" -> SafePath.of(c.get[String]("returnTo").toOption).asJson)).map(_.addCookie(ck))
        } yield r
    }

  private def getClientConfig(): IO[Response[IO]] =
    Ok(ClientConfigDto(cfg.devLogin && oidc.isEmpty, oidc.isDefined, cfg.version, skakaUrl))

  private def getMe(req: Request[IO]): IO[Response[IO]] =
    authed(req)(u => sessions.appNames(req).flatMap(n => Ok(u.me.copy(appNames = n))))

  private def patchMe(req: Request[IO]): IO[Response[IO]] =
    authed(req)(u =>
      body[UpdateMeReq](req) { r =>
        val tz = r.timezone.map(_.trim).filter(_.nonEmpty)
        if tz.exists(z => skuaka.core.Time.zone(z).isEmpty) then err(ErrorCode.TimeZoneInvalid)
        else if r.name.exists(_.trim.length > 100) then err(ErrorCode.NameTooLong)
        else users.updateMe(u.id, r.name, tz, r.chatReminders, r.chatInvites) *> me(u)
      }
    )

  // アイコン: 画像そのもの（PNG・JPEG・GIF・WebP、1MB まで）を本文にして送る. 消すと IdP の画像に戻る
  private def putMeAvatar(req: Request[IO]): IO[Response[IO]] =
    authed(req) { u =>
      req.body.take(AvatarImage.MaxBytes + 1L).compile.to(Array).flatMap { data =>
        if data.length > AvatarImage.MaxBytes then err(ErrorCode.AvatarTooLarge)
        else
          AvatarImage.detect(data).fold(err(ErrorCode.AvatarTypeUnsupported)) { ct =>
            users.saveAvatar(u.id, ct, data) *> me(u)
          }
      }
    }

  private def deleteMeAvatar(req: Request[IO]): IO[Response[IO]] =
    authed(req)(u => users.deleteAvatar(u.id) *> me(u))

  // URL の v（版）が変われば別の画像なので、長くキャッシュしてよい
  private def getUserAvatar(req: Request[IO], id: UUID): IO[Response[IO]] =
    authed(req) { _ =>
      users.avatar(id).flatMap {
        case None => err(ErrorCode.AvatarNotFound)
        case Some((ct, data)) =>
          // JSON の変換が優先されないよう、バイト列のまま返す
          IO.pure(Response[IO](Status.Ok).withBodyStream(fs2.Stream.emits(data)).putHeaders(
            headers.`Content-Type`(MediaType.unsafeParse(ct)),
            "Cache-Control" -> "private, max-age=31536000, immutable",
            "X-Content-Type-Options" -> "nosniff",
            "Content-Security-Policy" -> "default-src 'none'"
          ))
      }
    }

  private def getUsers(req: Request[IO]): IO[Response[IO]] =
    authed(req)(_ => users.search(param(req, "q").getOrElse(""), 20).flatMap(Ok(_)))

  // ------------------------------------------------------------ API トークン（画面からだけ管理できる）
  private def getTokens(req: Request[IO]): IO[Response[IO]] =
    interactive(req)(u => users.tokens(u.id).flatMap(Ok(_)))

  private def postTokens(req: Request[IO]): IO[Response[IO]] =
    interactive(req)(u =>
      body[ApiTokenReq](req) { r =>
        val name = r.name.trim
        if name.isEmpty || name.length > 100 then err(ErrorCode.ApiTokenNameInvalid)
        else if !TokenScope.all.contains(r.scope) then err(ErrorCode.ApiTokenScopeInvalid)
        else if r.expiresInDays.exists(d => d < 1 || d > 3650) then err(ErrorCode.ApiTokenExpiryInvalid)
        else {
          val (token, prefix) = ApiKeys.generate()
          users.createToken(
            u.id,
            name,
            prefix,
            Crypto.sha256(token),
            r.scope,
            r.expiresInDays.map(d => Instant.now().plusSeconds(d * 86400L))
          )
            .flatMap(t => Created(ApiTokenCreated(t, token)))
        }
      }
    )

  private def deleteToken(req: Request[IO], id: UUID): IO[Response[IO]] =
    interactive(req)(u => users.revokeToken(u.id, id).ifM(NoContent(), err(ErrorCode.ApiTokenNotFound)))

  /** 変更した後の自分の情報. */
  private def me(u: User): IO[Response[IO]] = users.find(u.id).flatMap(x => Ok(x.getOrElse(u).me))

  /** ログインした人のカレンダーを用意してから、セッションを発行する. */
  private def loggedIn(u: User, idToken: Option[String]): IO[ResponseCookie] =
    calendars.ensurePrimary(u) *> sessions.issue(u, idToken) <*
      sbirds.observe.Metrics.count("skuaka.logins", "via" -> idToken.fold("dev")(_ => "oidc"))

  private def page(title: String, msg: String, status: Status): IO[Response[IO]] = {
    val esc = Html.escape
    IO.pure(TextResponse(
      status,
      s"""<!doctype html><html lang="ja"><meta charset="utf-8"><meta name="viewport" content="width=device-width"><title>${esc(
          title
        )}</title>
         |<body style="font-family:system-ui,sans-serif;max-width:480px;margin:12vh auto;padding:0 1rem">
         |<h1 style="font-size:1.3rem">${esc(title)}</h1><p>${esc(
          msg
        )}</p><p><a href="/auth/login">もう一度ログインする</a></p></body></html>""".stripMargin,
      MediaType.text.html
    ))
  }

}
