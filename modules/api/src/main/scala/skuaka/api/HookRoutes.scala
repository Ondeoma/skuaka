package skuaka.api

import cats.effect.*
import cats.syntax.all.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import sbirds.links.{LinkFailure, StuiClient}
import skuaka.core.ErrorCode
import skuaka.db.*
import skuaka.shared.*

import java.util.UUID

/**
 * カレンダーの通知先（stui のチャンネル・stui の受信 Webhook・Webhook）の設定と、投稿先に選べる stui のチャンネル.
 * 通知先を見る・変えるのは、カレンダーの所有者と変更の権限がある人だけ.
 */
final class HookRoutes(hooks: HookService,
                       stui: Option[StuiClient],
                       calendars: CalendarStore,
                       users: UserStore,
                       val sessions: SessionAuth) extends RouteSupport {

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "api" / "calendars" / UuidVar(id) / "hooks" => getCalendarHooks(req, id)
    case req @ POST -> Root / "api" / "calendars" / UuidVar(id) / "hooks" => postCalendarHooks(req, id)
    case req @ PUT -> Root / "api" / "hooks" / UuidVar(id) => putHook(req, id)
    case req @ DELETE -> Root / "api" / "hooks" / UuidVar(id) => deleteHook(req, id)
    case req @ POST -> Root / "api" / "hooks" / UuidVar(id) / "test" => postHookTest(req, id)
    case req @ GET -> Root / "api" / "hooks" / UuidVar(id) / "deliveries" => getHookDeliveries(req, id)
    case req @ GET -> Root / "api" / "integrations" / "stui" / "channels" => getStuiChannels(req)
  }

  private def getCalendarHooks(req: Request[IO], id: UUID): IO[Response[IO]] =
    authed(req)(u => editor(u, id)(list(id).flatMap(Ok(_))))

  private def postCalendarHooks(req: Request[IO], id: UUID): IO[Response[IO]] =
    authed(req)(u =>
      editor(u, id)(body[HookReq](req)(r =>
        hooks.check(u, r, None).flatMap {
          case Left(m) => err(ErrorCode.HookInvalid, m)
          case Right((s, secret)) => hooks.store.create(id, s, secret, u.id) *> list(id).flatMap(Created(_))
        }
      ))
    )

  private def putHook(req: Request[IO], id: UUID): IO[Response[IO]] =
    authed(req)(u =>
      withHook(u, id)(h =>
        body[HookReq](req)(r =>
          hooks.check(u, r, Some(h)).flatMap {
            case Left(m) => err(ErrorCode.HookInvalid, m)
            case Right((s, secret)) => hooks.store.update(id, s, secret) *> list(h.calendarId).flatMap(Ok(_))
          }
        )
      )
    )

  private def deleteHook(req: Request[IO], id: UUID): IO[Response[IO]] =
    authed(req)(u => withHook(u, id)(h => hooks.store.delete(h.id) *> NoContent()))

  // テストの通知を積む（10秒ほどで送る. 結果は deliveries で見る）
  private def postHookTest(req: Request[IO], id: UUID): IO[Response[IO]] =
    authed(req)(u => withHook(u, id)(h => hooks.ping(h, u) *> Accepted()))

  private def getHookDeliveries(req: Request[IO], id: UUID): IO[Response[IO]] =
    authed(req)(u => withHook(u, id)(h => hooks.store.deliveries(h.id, 20).flatMap(Ok(_))))

  // 投稿先に選べる stui のチャンネル（自分が参加している公開・非公開のもの）
  private def getStuiChannels(req: Request[IO]): IO[Response[IO]] =
    authed(req)(u =>
      stui.fold(err(ErrorCode.StuiNotConfigured))(_.channels(u.id).flatMap {
        case Right(cs) => Ok(cs.map(c => StuiChannelDto(c.id, c.name, c.kind)))
        case Left(LinkFailure.NotConnected) => err(ErrorCode.StuiNotConnected)
        case Left(LinkFailure.Expired) => err(ErrorCode.StuiConnectionExpired)
        case Left(_) => err(ErrorCode.StuiChannelsUnreadable)
      })
    )

  /** 通知先の一覧（最後の送信と、stui の連携を使う人を付ける）. */
  private def list(calendarId: UUID): IO[List[HookDto]] =
    hooks.store.list(calendarId).flatMap { hs =>
      (hooks.store.lastDeliveries(hs.map(_.id)), hs.flatMap(_.ownerId).distinct.traverse(users.find)).mapN {
        (last, owners) =>
          val byId = owners.flatten.map(o => o.id -> o.brief).toMap
          hs.map(h =>
            HookDto(
              h.id,
              h.kind,
              h.name,
              if h.kind == HookKind.Url then h.url else "",
              if h.kind == HookKind.Url then h.secret else "",
              h.events,
              h.enabled,
              h.channelId,
              h.channelName,
              h.ownerId.flatMap(byId.get),
              last.get(h.id)
            )
          )
      }
    }

  /** カレンダーの所有者か変更の権限がある人だけ（見えないカレンダーは 404）. */
  private def editor(u: User, calendarId: UUID)(f: => IO[Response[IO]]): IO[Response[IO]] =
    calendars.get(u.id, calendarId).flatMap {
      case None => err(ErrorCode.CalendarNotFound)
      case Some(c) if CalRole.rank(c.role) >= CalRole.rank(CalRole.Edit) => f
      case Some(_) => err(ErrorCode.HookNotEditable)
    }

  private def withHook(u: User, id: UUID)(f: HookRow => IO[Response[IO]]): IO[Response[IO]] =
    hooks.store.get(id).flatMap {
      case None => err(ErrorCode.HookNotFound)
      case Some(h) =>
        calendars.get(u.id, h.calendarId).flatMap {
          case None => err(ErrorCode.HookNotFound)
          case Some(c) if CalRole.rank(c.role) >= CalRole.rank(CalRole.Edit) => f(h)
          case Some(_) => err(ErrorCode.HookNotEditable)
        }
    }
}
