package skuaka.api

import cats.effect.*
import org.http4s.*
import sbirds.http.RateLimit.Path
import sbirds.http.RateLimitConfig
import skuaka.core.ErrorCode

import scala.concurrent.duration.FiniteDuration

/**
 * skuaka の数え方（どの入口をどのカウンタで数えるか）.
 * 共通の数え方（/auth/…・/oauth/authorize は auth、/oauth/token・/oauth/revoke・/api/public/… は public の枠）は
 * sbirds-lib の RateLimit.standard. ここでは、skuaka だけの違いを決める.
 * 公開のカレンダーの購読（/ics/…）を、public の枠で数える.
 */
object RateLimit {

  def apply(routes: HttpRoutes[IO],
            cfg: RateLimitConfig,
            clock: IO[FiniteDuration] = IO.realTime): HttpRoutes[IO] =
    sbirds.http.RateLimit.standard(routes, cfg, ErrorCode.TooManyRequests, clock) { b =>
      { case Path("ics" :: _) => b.public }
    }
}
