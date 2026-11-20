package skuaka.api

import cats.effect.*
import org.http4s.*
import sbirds.auth.Crypto
import sbirds.http.{HttpParts, SessionCookie}
import skuaka.auth.ApiKeys
import skuaka.db.*

import java.time.Instant

/** 認証: Cookie のセッション、または `Authorization: Bearer <API トークン>`（どちらも DB にはハッシュだけ保存する）. */
final class SessionAuth(users: UserStore, cfg: ApiConfig) {

  private val cookie = new SessionCookie(cfg.cookieName, cfg.cookieSecure, cfg.sessionTtl)

  /** Bearer の値（API トークンの形のものだけ）. */
  def bearer(req: Request[IO]): Option[String] = HttpParts.bearer(req).map(_.trim)

  def fromRequest(req: Request[IO]): IO[Option[Principal]] =
    bearer(req) match {
      case Some(t) if ApiKeys.looksLike(t) =>
        users.findToken(Crypto.sha256(t), Instant.now()).map(_.map((u, id, scope) => Principal(u, Some(id -> scope))))
      case Some(_) => IO.pure(None)
      case None =>
        cookie.token(req).filter(_.nonEmpty) match {
          case None => IO.pure(None)
          case Some(token) => users.findSession(Crypto.sha256(token), Instant.now()).map(_.map(Principal(_, None)))
        }
    }

  def issue(user: User, idToken: Option[String]): IO[ResponseCookie] =
    cookie.issue((hash, expires) => users.createSession(hash, user.id, idToken, expires))

  /** sbirds のアプリの表示名（Cookie のセッションの id_token から. API キーやログインしていないときは空）. */
  def appNames(req: Request[IO]): IO[Map[String, String]] = cookie.appNames(req)(users.sessionIdToken)

  /** セッションを消す. IdP のログアウトに渡す ID トークンと、Cookie を消す指示を返す. */
  def revoke(req: Request[IO]): IO[(Option[String], ResponseCookie)] = cookie.revoke(req)(users.deleteSession)
}
