package skuaka.api

import cats.effect.*
import org.http4s.*
import sbirds.auth.Crypto
import sbirds.oauth.{OAuthCode, OAuthProvider}
import skuaka.auth.ApiKeys
import skuaka.db.*
import skuaka.shared.*

import java.time.Instant
import java.util.UUID

/**
 * 兄弟のサービス（skakapo など）への認可（OAuth 2.0 の認可コード + PKCE. 流れは sbirds.oauth.OAuthProvider）.
 * 発行するトークンは、そのユーザーの API トークン（「<クライアント名> 連携」. スコープは求められたもの. 設定の画面から取り消せる）.
 * トークンは1時間で切れ、refresh token（使うたびに新しくなる）で更新する. REST API と /mcp を、その利用者の権限とスコープで使える.
 */
final class OAuthRoutes(oauth: OAuthStore, val sessions: SessionAuth, cfg: ApiConfig) extends RouteSupport {

  private val TokenTtlSeconds = 3600L

  private val host = new OAuthProvider.Host[Principal] {
    val settings = OAuthProvider.Settings(
      "skuaka",
      cfg.cookieSecure,
      extraCss = "code{font-size:.85em}",
      loginRedirectNoStore = true,
      unreadableForm = "フォームの内容を読めませんでした。連携元の画面からやり直してください"
    )
    val codes = oauth

    def client(id: String): IO[Option[OAuthClient]] = IO.pure(cfg.oauthClients.find(_.id == id))

    // 画面のセッションだけ（API トークンでは許可しない）
    def browserSession(req: Request[IO]): IO[Option[Principal]] =
      sessions.fromRequest(req).map(_.filter(_.token.isEmpty))

    def userId(s: Principal): UUID = s.user.id

    /** API トークンのスコープ（write があれば write. write は読み取りも含む）. */
    override def codeScope(c: OAuthClient, rs: List[String]): String =
      if rs.contains(TokenScope.Write) then TokenScope.Write else TokenScope.Read

    def consent(req: Request[IO], c: OAuthClient, s: Principal, rs: List[String]): IO[OAuthProvider.Consent] = {
      val write = rs.contains(TokenScope.Write)
      val name = Html.escape(c.name)
      IO.pure(OAuthProvider.Consent(
        s"$name との連携",
        s"""<h1>$name との連携</h1>
           |<p><b>$name</b> が、あなた（${Html.escape(s.user.name)}）の権限で skuaka の予定を${
            if write then "読み書き" else "読み取り"
          }します。</p>
           |<p class="muted">求められた権限: ${
            if write then "<code>read</code>・<code>write</code>" else "<code>read</code>"
          }</p>
           |<ul class="muted"><li>予定・カレンダーの参照（あなたが見られるものだけ）</li>
           |${
            if write then "<li>予定の作成・変更・削除と、招待への返事を、あなたの名前で行います</li>" else "<li>読み取りだけです（作成・変更・削除はしません）</li>"
          }
           |<li>設定の「連携しているサービス」から、いつでも連携をやめられます</li></ul>""".stripMargin
      ))
    }

    override def authorized(req: Request[IO], s: Principal, c: OAuthClient, rs: List[String]): IO[Unit] =
      sbirds.observe.Log.println(s"[skuaka] oauth authorize: ${c.id} user=${s.user.id} scope=${codeScope(c, rs)}")

    def issue(req: Request[IO], c: OAuthClient, oc: OAuthCode): IO[OAuthProvider.Token] = {
      val (access, refresh, g) = newGrant
      oauth.replaceToken(oc.userId, c.id, s"${c.name} 連携", oc.scope, g).as(token(access, refresh, oc.scope))
    }

    // 範囲を狭める指定（scope）は受け付けない. 許可したときと同じ範囲を返す
    override def refresh: Option[OAuthProvider.Refresh] = Some { (_, c, rt, _) =>
      val (access, refresh, g) = newGrant
      oauth.rotate(Crypto.sha256(rt), c.id, g).map {
        case Some(scope) => Right(token(access, refresh, scope))
        case None => Left(OAuthProvider.Failure.invalidGrant("refresh_token が無効です（取り消し済み・使用済み）"))
      }
    }

    def revoke(req: Request[IO], c: OAuthClient, token: String): IO[Unit] = oauth.revoke(Crypto.sha256(token), c.id)
  }

  val routes: HttpRoutes[IO] = new OAuthProvider(host).routes

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

  /** トークンの応答（応答に載せるスコープは、write なら read write）. */
  private def token(access: String, refresh: String, scope: String): OAuthProvider.Token =
    OAuthProvider.Token(
      access,
      if scope == TokenScope.Write then "read write" else TokenScope.Read,
      Some(TokenTtlSeconds),
      Some(refresh)
    )
}
