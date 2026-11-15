package skuaka.api

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.headers.Location
import skuaka.auth.{ApiTokens, Crypto, OidcClient}
import skuaka.db.*
import skuaka.shared.*

import java.time.Instant

/** ログイン（OIDC・開発用）・ログアウト・自分の情報・ユーザーの検索. */
final class AuthRoutes(oidc: Option[OidcClient], users: UserStore, calendars: CalendarStore, val sessions: SessionAuth, cfg: ApiConfig)
    extends RouteSupport:

  /** ログイン後に戻る先（同じサイトのパスだけ）. */
  private def safeReturn(s: Option[String]): String =
    s.filter(p => p.startsWith("/") && !p.startsWith("//") && !p.startsWith("/\\")).getOrElse("/")

  private def redirect(to: String, cookie: Option[ResponseCookie] = None): IO[Response[IO]] =
    val r = Response[IO](Status.Found).putHeaders(Location(Uri.unsafeFromString(to)), "Cache-Control" -> "no-store")
    IO.pure(cookie.fold(r)(r.addCookie))

  /** ログインした人のカレンダーを用意してから、セッションを発行する. */
  private def loggedIn(u: User, idToken: Option[String]): IO[ResponseCookie] =
    calendars.ensurePrimary(u) *> sessions.issue(u, idToken)

  private def page(title: String, msg: String, status: Status): IO[Response[IO]] =
    val esc = (s: String) => s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
    IO.pure(TextResponse(status,
      s"""<!doctype html><html lang="ja"><meta charset="utf-8"><meta name="viewport" content="width=device-width"><title>${esc(title)}</title>
         |<body style="font-family:system-ui,sans-serif;max-width:480px;margin:12vh auto;padding:0 1rem">
         |<h1 style="font-size:1.3rem">${esc(title)}</h1><p>${esc(msg)}</p><p><a href="/auth/login">もう一度ログインする</a></p></body></html>""".stripMargin,
      MediaType.text.html))

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "auth" / "login" =>
      val back = safeReturn(param(req, "returnTo"))
      oidc match
        case Some(o) =>
          val state = Crypto.randomToken(); val verifier = Crypto.randomToken(48); val nonce = Crypto.randomToken()
          users.saveLoginState(state, verifier, nonce, back) *> o.authorizationUri(state, verifier, nonce).flatMap(u => redirect(u.renderString))
        case None if cfg.devLogin => redirect(s"/login?returnTo=${java.net.URLEncoder.encode(back, "UTF-8")}")
        case None                 => err(Status.ServiceUnavailable, "OIDC が設定されていません")

    // IdP から戻ってきたところ（認可コードを ID トークンに交換して、署名・iss・aud・exp・nonce を確かめる）
    case req @ GET -> Root / "auth" / "callback" =>
      (oidc, param(req, "code"), param(req, "state"), param(req, "error")) match
        case (_, _, _, Some(e)) =>
          page("ログインできませんでした", s"IdP がログインを断りました（$e ${param(req, "error_description").getOrElse("")}）", Status.Unauthorized)
        case (Some(o), Some(code), Some(state), None) =>
          users.takeLoginState(state).flatMap {
            case None => page("ログインできませんでした", "ログインの有効期限が切れました。もう一度ログインしてください。", Status.BadRequest)
            case Some((verifier, nonce, back)) =>
              o.authenticate(code, verifier, nonce, Instant.now()).attempt.flatMap {
                case Left(e) =>
                  IO.println(s"[skuaka] login failed: ${e.getMessage}") *>
                    page("ログインできませんでした", e.getMessage, Status.Unauthorized)
                case Right(res) =>
                  val id = res.identity
                  users.upsert(id.sub, id.name, id.email, id.picture).flatMap { u =>
                    if u.disabled then page("ログインできませんでした", "このユーザーは無効にされています。", Status.Forbidden)
                    else loggedIn(u, Some(res.idToken)).flatMap(c => redirect(back, Some(c)))
                  }
              }
          }
        case (None, _, _, _) => err(Status.ServiceUnavailable, "OIDC が設定されていません")
        case _               => err(Status.BadRequest, "code と state が必要です")

    // ログアウト: セッションを消し、IdP にもログアウトを頼む（end_session_endpoint があれば）
    case req @ POST -> Root / "auth" / "logout" =>
      sessions.revoke(req).flatMap { (idToken, c) =>
        oidc.traverse(_.logoutUri(idToken, cfg.publicUrl + "/")).map(_.flatten).flatMap { idp =>
          Ok(Json.obj("redirect" -> idp.fold("/login?loggedOut=1")(_.renderString).asJson)).map(_.addCookie(c))
        }
      }

    // 開発用: 名前だけでログインする（DEV_LOGIN=true で OIDC を設定していないときだけ）
    case req @ POST -> Root / "auth" / "dev-login" if cfg.devLogin && oidc.isEmpty =>
      body[Json](req) { j =>
        val c    = j.hcursor
        val name = c.get[String]("name").getOrElse("").trim
        if name.isEmpty then err(Status.BadRequest, "名前を入力してください")
        else
          for
            u  <- users.upsert(s"dev:$name", name, c.get[String]("email").toOption.map(_.trim).filter(_.contains("@")), None)
            ck <- loggedIn(u, None)
            r  <- Ok(Json.obj("redirect" -> safeReturn(c.get[String]("returnTo").toOption).asJson)).map(_.addCookie(ck))
          yield r
      }

    case GET -> Root / "api" / "client-config" =>
      Ok(ClientConfigDto(cfg.devLogin && oidc.isEmpty, oidc.isDefined, cfg.version))

    case req @ GET -> Root / "api" / "me" => authed(req)(u => Ok(u.me))

    case req @ PATCH -> Root / "api" / "me" =>
      authed(req)(u => body[UpdateMeReq](req) { r =>
        val tz = r.timezone.map(_.trim).filter(_.nonEmpty)
        if tz.exists(z => skuaka.core.Time.zone(z).isEmpty) then err(Status.BadRequest, "タイムゾーンが正しくありません")
        else if r.name.exists(_.trim.length > 100) then err(Status.BadRequest, "名前は100文字以内にしてください")
        else users.updateMe(u.id, r.name, tz) *> users.find(u.id).flatMap(x => Ok(x.getOrElse(u).me))
      })

    case req @ GET -> Root / "api" / "users" =>
      authed(req)(_ => users.search(param(req, "q").getOrElse(""), 20).flatMap(Ok(_)))

    // ------------------------------------------------------------ API トークン（画面からだけ管理できる）

    case req @ GET -> Root / "api" / "tokens" => interactive(req)(u => users.tokens(u.id).flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "tokens" =>
      interactive(req)(u => body[ApiTokenReq](req) { r =>
        val name = r.name.trim
        if name.isEmpty || name.length > 100 then err(Status.BadRequest, "名前を100文字以内で入れてください")
        else if !TokenScope.all.contains(r.scope) then err(Status.BadRequest, "スコープは read か write です")
        else if r.expiresInDays.exists(d => d < 1 || d > 3650) then err(Status.BadRequest, "有効期限は1〜3650日です")
        else
          val (token, prefix) = ApiTokens.generate()
          users.createToken(u.id, name, prefix, Crypto.sha256(token), r.scope, r.expiresInDays.map(d => Instant.now().plusSeconds(d * 86400L)))
            .flatMap(t => Created(ApiTokenCreated(t, token)))
      })

    case req @ DELETE -> Root / "api" / "tokens" / UuidVar(id) =>
      interactive(req)(u => users.revokeToken(u.id, id).ifM(NoContent(), err(Status.NotFound, "API トークンが見つかりません")))
  }
