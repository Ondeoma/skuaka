package skuaka.app

import sbirds.http.Env.*
import sbirds.db.*
import cats.effect.*
import skuaka.api.*
import sbirds.auth.OidcConfig

import scala.concurrent.duration.*

/** 設定は環境変数から読む. */
object Config {
  private def local(url: String): Boolean =
    scala.util.Try(java.net.URI.create(url).getHost).toOption.flatMap(Option(_)).map(_.toLowerCase)
      .exists(h => Set("localhost", "127.0.0.1", "[::1]").contains(h) || h.endsWith(".localhost"))

  def load: IO[AppConfig] = IO.fromEither(
    (for {
      dbUrl <- required("DATABASE_URL")
      dbUser <- required("DATABASE_USER")
      dbPass <- required("DATABASE_PASSWORD")
      pub <- required("PUBLIC_URL").map(_.stripSuffix("/"))
      devLogin = enabled("DEV_LOGIN")
      oidc <- (opt("OIDC_ISSUER"), opt("OIDC_CLIENT_ID"), opt("OIDC_CLIENT_SECRET")) match {
        case (Some(i), Some(c), Some(s)) =>
          Right(Some(OidcConfig(
            i,
            c,
            s,
            s"$pub/auth/callback",
            opt("OIDC_GROUPS_CLAIM").getOrElse("groups"),
            Some(list("OIDC_SCOPES")).filter(_.nonEmpty).getOrElse(List("openid", "profile", "email")),
            list("OIDC_ALLOWED_GROUPS").toSet
          )))
        // 開発用のログインは名前だけでだれにでもなれるので、手元（localhost）で開くときだけ
        case (None, None, None) if devLogin && !local(pub) => Left("DEV_LOGIN は PUBLIC_URL が localhost のときだけ使えます")
        case (None, None, None) if devLogin => Right(None)
        case _ => Left("OIDC_ISSUER・OIDC_CLIENT_ID・OIDC_CLIENT_SECRET を設定してください（開発中は DEV_LOGIN=true で省略できます）")
      }
      // Web 会議（skaka）: 3つ揃ったときだけ. 連携のトークンを暗号化するので APP_SECRET も要る
      skaka <- (opt("SKAKA_URL"), opt("SKAKA_CLIENT_ID"), opt("SKAKA_CLIENT_SECRET")) match {
        case (Some(u), Some(i), Some(s)) =>
          val url = u.stripSuffix("/")
          Right(Some(SkakaConfig(url, opt("SKAKA_INTERNAL_URL").map(_.stripSuffix("/")).getOrElse(url), i, s)))
        case (None, None, None) => Right(None)
        case _ => Left("SKAKA_URL・SKAKA_CLIENT_ID・SKAKA_CLIENT_SECRET は3つとも設定してください")
      }
      rate <- sbirds.http.RateLimitConfig.fromEnv()
      secret <- opt("APP_SECRET") match {
        case Some(s) if s.length < 16 => Left("APP_SECRET は16文字以上にしてください")
        case None if skaka.isDefined => Left("skaka と連携するには APP_SECRET（16文字以上の秘密の値）を設定してください")
        case s => Right(s)
      }
      // OAUTH_CLIENTS: 認可を求める兄弟のサービス（scopes は省くと read だけ. 形は sbirds.oauth.OAuthClient.parse）
      clients <- sbirds.oauth.OAuthClient.fromEnv()
    } yield AppConfig(
      port = int("HTTP_PORT").getOrElse(8080),
      db = DbConfig(dbUrl, dbUser, dbPass, int("DATABASE_POOL_SIZE").getOrElse(10)),
      oidc = oidc,
      api = ApiConfig(
        cookieSecure = pub.startsWith("https://"),
        publicUrl = pub,
        sessionTtl = int("SESSION_DAYS").getOrElse(14).days,
        devLogin = devLogin,
        rateLimit =
          rate,
        oauthClients = clients
      ),
      staticDir = opt("STATIC_DIR"),
      skaka = skaka,
      appSecret = secret,
      icsSyncMinutes = int("ICS_SYNC_MINUTES").filter(_ >= 5).getOrElse(60),
      icsAllowPrivate = enabled("ICS_ALLOW_PRIVATE_URLS")
    )).left.map(new IllegalArgumentException(_))
  )
}
