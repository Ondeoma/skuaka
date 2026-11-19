package skuaka.api

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.headers.{`Content-Type`, Authorization, Location}
import sbirds.auth.Crypto
import skuaka.auth.ApiKeys
import skuaka.db.*
import skuaka.shared.*

import java.time.Instant

/**
 * 兄弟のサービス（skakapo など）への認可（OAuth 2.0 の認可コード + PKCE. RFC 6749・7636・7009）.
 * 発行するトークンは、そのユーザーの API トークン（「<クライアント名> 連携」. スコープは求められたもの. 設定の画面から取り消せる）.
 * トークンは1時間で切れ、refresh token（使うたびに新しくなる）で更新する. REST API と /mcp を、その利用者の権限とスコープで使える.
 */
final class OAuthRoutes(oauth: OAuthStore, val sessions: SessionAuth, cfg: ApiConfig) extends RouteSupport {

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "oauth" / "authorize" => getOauthAuthorize(req)
    case req @ POST -> Root / "oauth" / "authorize" => postOauthAuthorize(req)
    case req @ POST -> Root / "oauth" / "token" => postOauthToken(req)
    case req @ POST -> Root / "oauth" / "revoke" => postOauthRevoke(req)
  }

  private def getOauthAuthorize(req: Request[IO]): IO[Response[IO]] =
    validate(param(req, _)) match {
      case Left(r) => r
      case Right((c, _, _, scope)) =>
        sessions.fromRequest(req).flatMap {
          // 画面のセッションだけ（API トークンでは許可しない）. なければログインしてからこの画面に戻る
          case Some(p) if p.token.isEmpty =>
            val write = scope == TokenScope.Write
            val csrf = Crypto.randomToken(24)
            val hidden = List(
              "client_id",
              "redirect_uri",
              "state",
              "code_challenge",
              "code_challenge_method",
              "response_type",
              "scope"
            )
              .flatMap(k =>
                param(req, k).map(v => s"""<input type="hidden" name="$k" value="${Html.escape(v)}">""")
              ).mkString
            val name = Html.escape(c.name)
            page(
              s"$name との連携",
              s"""<h1>$name との連携</h1>
                 |<p><b>$name</b> が、あなた（${Html.escape(p.user.name)}）の権限で skuaka の予定を${
                  if write then "読み書き" else "読み取り"
                }します。</p>
                 |<p class="muted">求められた権限: ${
                  if write then "<code>read</code>・<code>write</code>" else "<code>read</code>"
                }</p>
                 |<ul class="muted"><li>予定・カレンダーの参照（あなたが見られるものだけ）</li>
                 |${
                  if write then "<li>予定の作成・変更・削除と、招待への返事を、あなたの名前で行います</li>" else "<li>読み取りだけです（作成・変更・削除はしません）</li>"
                }
                 |<li>設定の「連携しているサービス」から、いつでも連携をやめられます</li></ul>
                 |<form method="post" action="/oauth/authorize">$hidden<input type="hidden" name="csrf" value="$csrf">
                 |<div class="actions"><button type="submit" name="decision" value="deny">拒否する</button>
                 |<button type="submit" name="decision" value="allow" class="primary">許可する</button></div></form>""".stripMargin
            )
              .map(_.addCookie(ResponseCookie(
                CsrfCookie,
                csrf,
                path = Some("/oauth"),
                httpOnly = true,
                secure = cfg.cookieSecure,
                sameSite = Some(SameSite.Strict),
                maxAge = Some(600)
              )))
          case _ =>
            IO.pure(Response[IO](Status.Found).putHeaders(
              Location(Uri.unsafeFromString(s"/auth/login?returnTo=${enc(req.uri.renderString)}")),
              "Cache-Control" -> "no-store"
            ))
        }
    }

  private def postOauthAuthorize(req: Request[IO]): IO[Response[IO]] =
    req.as[UrlForm].flatMap { u =>
      val p = (k: String) => u.getFirst(k).map(_.trim).filter(_.nonEmpty)
      validate(p) match {
        case Left(r) => r
        case Right((c, state, challenge, scope)) =>
          val csrfOk = req.cookies.find(_.name == CsrfCookie).exists(ck =>
            p("csrf").exists(v => Crypto.constantTimeEquals(ck.content, v))
          )
          sessions.fromRequest(req).flatMap {
            case Some(s) if s.token.isEmpty =>
              if !csrfOk then errorPage("画面の有効期限が切れました。連携元の画面からやり直してください")
              else if p("decision").contains("allow") then {
                val code = Crypto.randomToken(32)
                oauth.createCode(
                  Crypto.sha256(code),
                  OAuthCode(c.id, c.redirectUri, s.user.id, challenge, scope),
                  Instant.now().plusSeconds(CodeTtlSeconds)
                ) *>
                  sbirds.observe.Log.println(s"[skuaka] oauth authorize: ${c.id} user=${s.user.id} scope=$scope") *>
                  back(c.redirectUri, "code" -> Some(code), "state" -> state)
              } else back(c.redirectUri, "error" -> Some("access_denied"), "state" -> state)
            case _ => errorPage("ログインが切れました。連携元の画面からやり直してください")
          }.map(_.removeCookie(ResponseCookie(CsrfCookie, "", path = Some("/oauth"))))
      }
    }

  private def postOauthToken(req: Request[IO]): IO[Response[IO]] =
    basicClient(req) match {
      case None => oauthError(Status.Unauthorized, "invalid_client", "クライアントの認証に失敗しました")
      case Some(c) =>
        form(req) { f =>
          f.get("grant_type") match {
            case Some("authorization_code") =>
              (f.get("code"), f.get("redirect_uri"), f.get("code_verifier")) match {
                case (Some(code), Some(uri), Some(verifier)) if verifier.length >= 43 && verifier.length <= 128 =>
                  oauth.consumeCode(Crypto.sha256(code)).flatMap {
                    case Some(oc)
                        if oc.clientId == c.id && oc.redirectUri == uri &&
                          Crypto.constantTimeEquals(oc.codeChallenge, Crypto.pkceChallenge(verifier)) =>
                      val (token, refresh, g) = newGrant
                      oauth.replaceToken(oc.userId, c.id, s"${c.name} 連携", oc.scope, g) *>
                        tokenResponse(token, refresh, oc.scope)
                    case _ => oauthError(Status.BadRequest, "invalid_grant", "認可コードが無効です（期限切れ・使用済み・不一致）")
                  }
                case _ => oauthError(
                    Status.BadRequest,
                    "invalid_request",
                    "code・redirect_uri・code_verifier（43〜128文字）を指定してください"
                  )
              }
            case Some("refresh_token") =>
              f.get("refresh_token").filter(_.nonEmpty) match {
                case None => oauthError(Status.BadRequest, "invalid_request", "refresh_token を指定してください")
                case Some(rt) =>
                  val (token, refresh, g) = newGrant
                  oauth.rotate(Crypto.sha256(rt), c.id, g).flatMap {
                    // 範囲を狭める指定（scope）は受け付けない. 許可したときと同じ範囲を返す
                    case Some(scope) => tokenResponse(token, refresh, scope)
                    case None => oauthError(Status.BadRequest, "invalid_grant", "refresh_token が無効です（取り消し済み・使用済み）")
                  }
              }
            case g => oauthError(
                Status.BadRequest,
                if g.isEmpty then "invalid_request" else "unsupported_grant_type",
                "grant_type は authorization_code か refresh_token です"
              )
          }
        }
    }

  private def postOauthRevoke(req: Request[IO]): IO[Response[IO]] =
    basicClient(req) match {
      case None => oauthError(Status.Unauthorized, "invalid_client", "クライアントの認証に失敗しました")
      case Some(c) => form(req)(f =>
          f.get("token").filter(_.nonEmpty).traverse_(t => oauth.revoke(Crypto.sha256(t), c.id)) *> Ok(Json.obj())
        )
    }

  private val CodeTtlSeconds = 60L
  private val TokenTtlSeconds = 3600L
  private val CsrfCookie = "skuaka_oauth_csrf"

  private def client(id: String): Option[OAuthClient] = cfg.oauthClients.find(_.id == id)

  private def enc(s: String): String = java.net.URLEncoder.encode(s, "UTF-8")

  /** redirect_uri に結果を付けて戻す. */
  private def back(uri: String, params: (String, Option[String])*): IO[Response[IO]] = {
    val q = params.collect { case (k, Some(v)) => s"$k=${enc(v)}" }.mkString("&")
    IO.pure(Response[IO](Status.Found).putHeaders(Location(Uri.unsafeFromString(uri +
      (if uri.contains("?") then "&" else "?") + q))))
  }

  // ---------------------------------------------------------------- 許可の画面（サーバーで HTML を組み立てる）

  /**
   * 許可の画面. CSP の form-action は付けない（フォームの送信後のリダイレクトの連鎖にも効くため、
   * 連携元が callback の後に別の origin へ戻すと止まる）. CSRF は Cookie と hidden の値の照合で防ぐ.
   */
  private def page(title: String, body: String): IO[Response[IO]] = {
    val html =
      s"""<!DOCTYPE html><html lang="ja"><head><meta charset="UTF-8"><meta name="viewport" content="width=device-width, initial-scale=1">
         |<title>${Html.escape(title)} | skuaka</title><style>
         |:root{color-scheme:light dark}body{margin:0;font-family:system-ui,-apple-system,"Hiragino Sans","Noto Sans JP",sans-serif;background:#f1f5f9;color:#1e293b;
         |display:flex;min-height:100vh;align-items:center;justify-content:center;padding:1rem}
         |@media(prefers-color-scheme:dark){body{background:#0f172a;color:#e2e8f0}.card{background:#1c273d!important;border-color:#283548!important}}
         |.card{background:#fff;border:1px solid #e2e8f0;border-radius:14px;padding:1.6rem 1.8rem;max-width:440px;width:100%;
         |box-shadow:0 11px 15px -7px rgba(0,0,0,.2),0 24px 38px 3px rgba(0,0,0,.14)}
         |h1{font-size:1.15rem;margin:0 0 .8rem}p{line-height:1.7;margin:.4rem 0}.muted{color:#64748b;font-size:.85rem}.logo{color:#2563eb;font-weight:800}
         |.actions{display:flex;gap:.6rem;justify-content:flex-end;margin-top:1.2rem}button{font:inherit;border-radius:8px;padding:.5rem 1.1rem;cursor:pointer;
         |border:1px solid #cbd5e1;background:transparent;color:inherit}button.primary{background:#2563eb;border-color:#2563eb;color:#fff}
         |ul{padding-left:1.2rem;margin:.4rem 0}code{font-size:.85em}</style></head><body><div class="card"><div class="logo">skuaka</div>$body</div></body></html>""".stripMargin
    IO.pure(Response[IO](Status.Ok).withEntity(html)(using EntityEncoder.stringEncoder).putHeaders(
      `Content-Type`(MediaType.text.html, Charset.`UTF-8`),
      "Cache-Control" -> "no-store",
      "X-Frame-Options" -> "DENY",
      "Content-Security-Policy" -> "default-src 'none'; style-src 'unsafe-inline'; frame-ancestors 'none'"
    ))
  }

  private def errorPage(msg: String): IO[Response[IO]] =
    page(
      "認可できません",
      s"<h1>認可できません</h1><p>${Html.escape(msg)}</p><p class=\"muted\">連携元のサービスの設定を確認してください。</p>"
    ).map(_.withStatus(Status.BadRequest))

  /** 求められたスコープ（なければ read）. クライアントに許していないものが含まれていれば None. */
  private def scopes(c: OAuthClient, requested: Option[String]): Option[List[String]] = {
    val rs = requested.map(
      _.split(' ').map(_.trim).filter(_.nonEmpty).toList.distinct
    ).filter(_.nonEmpty).getOrElse(List(TokenScope.Read))
    Option.when(rs.forall(c.scopes.contains))(rs)
  }

  /** API トークンのスコープ（write があれば write. write は読み取りも含む）. */
  private def tokenScope(rs: List[String]): String =
    if rs.contains(TokenScope.Write) then TokenScope.Write else TokenScope.Read

  /** 応答に載せるスコープ. */
  private def granted(scope: String): String = if scope == TokenScope.Write then "read write" else TokenScope.Read

  /** 認可の要求の確認（client_id・redirect_uri が不正ならリダイレクトしない）. */
  private def validate(p: String => Option[String])
    : Either[IO[Response[IO]], (OAuthClient, Option[String], String, String)] =
    (p("client_id").flatMap(client), p("redirect_uri")) match {
      case (None, _) => Left(errorPage("登録されていないクライアントです（client_id）"))
      case (Some(c), r) if !r.contains(c.redirectUri) => Left(errorPage("戻り先（redirect_uri）が登録されたものと一致しません"))
      case (Some(c), _) =>
        val state = p("state")
        if !p("response_type").contains("code") then
          Left(back(c.redirectUri, "error" -> Some("unsupported_response_type"), "state" -> state))
        else if !p("code_challenge_method").contains("S256") ||
          !p("code_challenge").exists(_.matches("^[A-Za-z0-9_-]{43}$"))
        then
          Left(back(
            c.redirectUri,
            "error" -> Some("invalid_request"),
            "error_description" -> Some("PKCE（S256）が必要です"),
            "state" -> state
          ))
        else
          scopes(c, p("scope")) match {
            case None => Left(back(c.redirectUri, "error" -> Some("invalid_scope"), "state" -> state))
            case Some(rs) => Right((c, state, p("code_challenge").get, tokenScope(rs)))
          }
    }

  // ---------------------------------------------------------------- トークンの発行・更新・失効（クライアント認証は Basic）

  private def oauthError(status: Status, code: String, desc: String): IO[Response[IO]] =
    IO.pure(Response[IO](status).withEntity(Json.obj("error" -> code.asJson, "error_description" -> desc.asJson))
      .putHeaders("Cache-Control" -> "no-store", "Pragma" -> "no-cache").putHeaders(
        (if status == Status.Unauthorized then List[Header.ToRaw]("WWW-Authenticate" -> "Basic realm=\"skuaka\"")
         else Nil)*
      ))

  private def basicClient(req: Request[IO]): Option[OAuthClient] =
    req.headers.get[Authorization].collect { case Authorization(BasicCredentials(id, secret)) => (id, secret) }
      .flatMap((id, secret) => client(id).filter(c => Crypto.constantTimeEquals(c.secret, secret)))

  private def form(req: Request[IO])(f: Map[String, String] => IO[Response[IO]]): IO[Response[IO]] =
    req.as[UrlForm].attempt.flatMap {
      case Left(_) => oauthError(Status.BadRequest, "invalid_request", "application/x-www-form-urlencoded で送ってください")
      case Right(u) => f(u.values.view.mapValues(_.headOption.getOrElse("")).toMap)
    }

  /** 新しいトークンと refresh token（値と、保存するハッシュ）. */
  private def newGrant: (String, String, OAuthGrant) = {
    val (token, prefix) = ApiKeys.generate()
    val refresh = Crypto.randomToken(32)
    (
      token,
      refresh,
      OAuthGrant(prefix, Crypto.sha256(token), Crypto.sha256(refresh), Instant.now().plusSeconds(TokenTtlSeconds))
    )
  }

  private def tokenResponse(token: String, refresh: String, scope: String): IO[Response[IO]] =
    Ok(Json.obj(
      "access_token" -> token.asJson,
      "token_type" -> "Bearer".asJson,
      "expires_in" -> TokenTtlSeconds.asJson,
      "refresh_token" -> refresh.asJson,
      "scope" -> granted(scope).asJson
    )).map(_.putHeaders("Cache-Control" -> "no-store", "Pragma" -> "no-cache"))

}
