package skuaka.api

import cats.effect.*
import com.comcast.ip4s.{Cidr, IpAddress}
import org.http4s.*

import scala.concurrent.duration.FiniteDuration

/**
 * IP ごとの回数の制限（1分あたり. 0 なら制限しない）. ログインしなくても使える入口だけに掛ける.
 * auth はログイン（/auth/…）、public は公開リンク（/api/public/…・/ics/…）.
 * trustedProxies はリバースプロキシの IP か CIDR. そこから来た要求だけ X-Forwarded-For を信じる.
 */
final case class RateLimitConfig(
  authPerMinute: Int = 20,
  publicPerMinute: Int = 120,
  trustedProxies: List[Cidr[IpAddress]] = Nil)

object RateLimitConfig {

  /** IP（10.0.0.1）か CIDR（10.0.0.0/8）. */
  def proxy(
    s: String
  ): Option[Cidr[IpAddress]] = sbirds.http.RateLimit.proxy(s)
}

/** skuaka の数え方（どの入口をどのカウンタで数えるか）. 数える仕組みは sbirds-lib の RateLimit. */
object RateLimit {
  def apply(
    routes: HttpRoutes[IO],
    cfg: RateLimitConfig,
    clock: IO[FiniteDuration] = IO.realTime
  ): HttpRoutes[IO] = {
    val auth = sbirds.http.RateLimit.counter(cfg.authPerMinute, clock)
    val pub = sbirds.http.RateLimit.counter(cfg.publicPerMinute, clock)
    sbirds.http.RateLimit(routes, cfg.trustedProxies)(req =>
      req.pathInfo.segments.map(_.decoded()).toList match {
        case "auth" :: _ => auth.toList
        case "api" :: "public" :: _ | "ics" :: _ => pub.toList
        case _ => Nil
      }
    )
  }
}
