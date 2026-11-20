package skuaka.api

import cats.effect.*
import io.circe.Decoder
import org.http4s.*
import skuaka.core.ErrorCode

import java.util.UUID

/** プッシュ通知（PWA）の購読（sbirds.push.PushRoutes）. 端末の登録は画面のログイン（Cookie のセッション）だけ. */
final class PushRoutes(store: sbirds.push.PushStore, push: sbirds.push.PushService, val sessions: SessionAuth)
  extends RouteSupport { self =>

  private val host = new sbirds.push.PushRoutes.Host {
    def authed(req: Request[IO])(f: UUID => IO[Response[IO]]): IO[Response[IO]] = self.authed(req)(u => f(u.id))
    def interactive(req: Request[IO])(f: UUID => IO[Response[IO]]): IO[Response[IO]] =
      self.interactive(req)(u => f(u.id))
    def body[A: Decoder](req: Request[IO])(f: A => IO[Response[IO]]): IO[Response[IO]] = self.body[A](req)(f)
  }

  val routes: HttpRoutes[IO] = new sbirds.push.PushRoutes(
    store,
    push,
    host,
    notAllowed = ErrorCode.PushServiceNotAllowed,
    invalid = ErrorCode.PushSubscriptionInvalid
  ).routes
}
