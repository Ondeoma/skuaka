package skuaka.api

import cats.effect.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.headers.Location
import sbirds.auth.Crypto
import skuaka.shared.*
import skuaka.core.ErrorCode

import java.time.Instant

/** 連携の開始・戻り・やめる・状態（/api/integrations/skaka/…）. */
final class SkakaRoutes(
  skaka: Option[SkakaService],
  val sessions: SessionAuth,
  cfg: ApiConfig) extends RouteSupport {

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "api" / "integrations" / "skaka" => getIntegrationsSkaka(req)
    case req @ GET -> Root / "api" / "integrations" / "skaka" / "connect" => getIntegrationsSkakaConnect(req)
    case req @ GET -> Root / "api" / "integrations" / "skaka" / "callback" => getIntegrationsSkakaCallback(req)
    case req @ DELETE -> Root / "api" / "integrations" / "skaka" / "connection" =>
      deleteIntegrationsSkakaConnection(req)
  }

  private def getIntegrationsSkaka(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req)(u =>
      skaka.fold(Ok(SkakaStatusDto(false, false, None)))(s =>
        s.connected(u.id).flatMap(c => Ok(SkakaStatusDto(true, c, Some(s.cfg.url))))
      )
    )

  // 連携を始める（skaka の許可の画面へ）. 画面のリンクから開く
  private def getIntegrationsSkakaConnect(
    req: Request[IO]
  ): IO[Response[IO]] =
    interactive(req)(u =>
      enabled { s =>
        val state = Crypto.randomToken(24)
        val verifier = Crypto.randomToken(48)
        s.store.saveState(
          state,
          u.id,
          s.Provider,
          verifier,
          param(req, "returnTo").getOrElse("/settings"),
          Instant.now().plusSeconds(600)
        ) *>
          redirect(Uri.unsafeFromString(s"${s.cfg.url.stripSuffix("/")}/oauth/authorize").withQueryParams(Map(
            "response_type" -> "code",
            "client_id" -> s.cfg.clientId,
            "redirect_uri" -> callback,
            "scope" -> "read write",
            "state" -> state,
            "code_challenge" -> Crypto.pkceChallenge(verifier),
            "code_challenge_method" -> "S256"
          )).renderString)
      }
    )

  // 許可の画面から戻ったとき: コードをトークンに替えて保存する
  private def getIntegrationsSkakaCallback(
    req: Request[IO]
  ): IO[Response[IO]] =
    interactive(req)(u =>
      enabled { s =>
        param(req, "state").fold(IO.pure(None))(st => s.store.takeState(st, s.Provider)).flatMap {
          case None => done("/settings", "error")
          case Some((uid, _, back)) if uid != u.id => done(back, "error")
          case Some((_, _, back)) if param(req, "error").isDefined =>
            done(back, if param(req, "error").contains("access_denied") then "denied" else "error")
          case Some((_, verifier, back)) =>
            param(req, "code").fold(done(back, "error"))(code =>
              s.exchange(u.id, code, verifier, callback).ifM(done(back, "connected"), done(back, "error"))
            )
        }
      }
    )

  private def deleteIntegrationsSkakaConnection(
    req: Request[IO]
  ): IO[Response[IO]] =
    interactive(req)(u => enabled(s => s.disconnect(u.id) *> NoContent()))

  private val callback = s"${cfg.publicUrl}/api/integrations/skaka/callback"

  private def redirect(
    to: String
  ): IO[Response[IO]] =
    IO.pure(Response[IO](Status.Found).putHeaders(Location(Uri.unsafeFromString(to)), "Cache-Control" -> "no-store"))

  /** 戻り先（同じサイトのパスだけ）に結果を付けて戻る. */
  private def done(
    returnTo: String,
    result: String
  ): IO[Response[IO]] = {
    val back = SafePath.of(Option(returnTo), "/settings")
    redirect(back + (if back.contains("?") then "&" else "?") + s"integration=skaka&result=$result")
  }

  private def enabled(
    f: SkakaService => IO[Response[IO]]
  ): IO[Response[IO]] =
    skaka.fold(err(ErrorCode.SkakaNotConfigured))(f)

}
