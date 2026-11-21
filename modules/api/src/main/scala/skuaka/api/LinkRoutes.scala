package skuaka.api

import sbirds.links.LinkClient
import sbirds.shared.SafePath
import cats.effect.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import skuaka.shared.*
import skuaka.core.ErrorCode

/**
 * 兄弟のサービス（skaka: Web 会議・stui: チャット）との連携の開始・戻り・やめる・状態（/api/integrations/{サービス}/…）.
 * links は設定されている連携先. known は設定されていなくても状態を返す連携先（画面が「設定されていない」と分かるように）.
 */
final class LinkRoutes(links: List[LinkClient],
                       val sessions: SessionAuth,
                       cfg: ApiConfig,
                       known: Set[String] = LinkRoutes.Known) extends RouteSupport {

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "api" / "integrations" / p if known(p) => getIntegration(req, p)
    case req @ GET -> Root / "api" / "integrations" / p / "connect" if known(p) => getIntegrationConnect(req, p)
    case req @ GET -> Root / "api" / "integrations" / p / "callback" if known(p) => getIntegrationCallback(req, p)
    case req @ DELETE -> Root / "api" / "integrations" / p / "connection" if known(p) =>
      deleteIntegrationConnection(req, p)
  }

  private def getIntegration(req: Request[IO], p: String): IO[Response[IO]] =
    authed(req)(u =>
      find(p).fold(Ok(LinkStatusDto(false, false, None)))(l =>
        l.connected(u.id).flatMap(c => Ok(LinkStatusDto(true, c, Some(l.cfg.url))))
      )
    )

  // 連携を始める（相手の許可の画面へ）. 画面のリンクから開く
  private def getIntegrationConnect(req: Request[IO], p: String): IO[Response[IO]] =
    interactive(req)(u =>
      enabled(p)(l =>
        l.authorizeUrl(u.id, callback(p), param(req, "returnTo").getOrElse("/settings")).flatMap(redirect(_))
      )
    )

  // 許可の画面から戻ったとき: コードをトークンに替えて保存する
  private def getIntegrationCallback(req: Request[IO], p: String): IO[Response[IO]] =
    interactive(req)(u =>
      enabled(p) { l =>
        param(req, "state").fold(IO.pure(None))(l.takeState).flatMap {
          case None => done(p, "/settings", "error")
          case Some((uid, _, back)) if uid != u.id => done(p, back, "error")
          case Some((_, _, back)) if param(req, "error").isDefined =>
            done(p, back, if param(req, "error").contains("access_denied") then "denied" else "error")
          case Some((_, verifier, back)) =>
            param(req, "code").fold(done(p, back, "error"))(code =>
              l.exchange(u.id, code, verifier, callback(p)).ifM(done(p, back, "connected"), done(p, back, "error"))
            )
        }
      }
    )

  private def deleteIntegrationConnection(req: Request[IO], p: String): IO[Response[IO]] =
    interactive(req)(u => enabled(p)(l => l.disconnect(u.id) *> NoContent()))

  private def find(p: String): Option[LinkClient] = links.find(_.provider == p)

  private def callback(p: String) = s"${cfg.publicUrl}/api/integrations/$p/callback"

  /** 戻り先（同じサイトのパスだけ）に結果を付けて戻る. */
  private def done(p: String, returnTo: String, result: String): IO[Response[IO]] = {
    val back = SafePath.of(Option(returnTo), "/settings")
    redirect(back + (if back.contains("?") then "&" else "?") + s"integration=$p&result=$result")
  }

  private def enabled(p: String)(f: LinkClient => IO[Response[IO]]): IO[Response[IO]] =
    find(p).fold(err(p match {
      case "skaka" => ErrorCode.SkakaNotConfigured
      case "stui" => ErrorCode.StuiNotConfigured
      case _ => ErrorCode.LinkUnknown
    }))(f)
}

object LinkRoutes {

  /** 連携できるサービス. */
  val Known: Set[String] = Set("skaka", "stui")
}
