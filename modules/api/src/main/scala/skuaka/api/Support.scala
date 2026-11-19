package skuaka.api

import cats.effect.*
import org.http4s.*
import skuaka.db.*
import skuaka.shared.*

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
  oauthClients: List[OAuthClient] = Nil)

/** 認可を求める兄弟のサービス（OAUTH_CLIENTS で登録する）. redirectUri は完全一致で照合する. scopes は許すスコープ（read・write）. */
final case class OAuthClient(
  id: String,
  secret: String,
  redirectUri: String,
  name: String,
  scopes: List[String] = List(TokenScope.Read))

/** サーバーで組み立てる HTML の文字の置き換え. */
object Html {
  def escape(
    s: String
  ): String =
    s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\"", "&quot;").replace("'", "&#39;")
}

/** 誰の要求か. token は API トークンで呼ばれたときの (トークンID, スコープ). */
final case class Principal(
  user: User,
  token: Option[(UUID, String)]) {
  def canWrite: Boolean = token.forall(_._2 == TokenScope.Write)
}

/** 文字列の応答（JSON 以外. circe の EntityEncoder に取られないよう明示する）. */
object TextResponse {
  def apply(
    status: Status,
    body: String,
    media: MediaType = MediaType.text.plain
  ): Response[IO] =
    Response[IO](status).withEntity(body)(using EntityEncoder.stringEncoder).withContentType(headers.`Content-Type`(
      media,
      Charset.`UTF-8`
    ))
}

/** 入力の確認. */
object Check {
  private val colorRe = "^#[0-9a-fA-F]{6}$".r

  def color(
    c: String
  ): Either[String, String] = Either.cond(colorRe.matches(c), c.toLowerCase, "色は #rrggbb で指定してください")

  def text(
    v: String,
    what: String,
    max: Int
  ): Either[String, String] =
    Either.cond(v.length <= max, v, s"$what は $max 文字以内にしてください")
}
