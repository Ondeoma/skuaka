package skuaka.api

import cats.effect.*
import cats.syntax.all.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import sbirds.auth.Crypto
import skuaka.core.{ErrorCode, Ics, IcsEvent, Time}
import skuaka.db.*
import skuaka.shared.*

import java.time.Instant
import java.util.UUID

/** カレンダー・共有・表示の設定・iCalendar の配信. */
final class CalendarRoutes(
  calendars: CalendarStore,
  events: EventStore,
  users: UserStore,
  val sessions: SessionAuth,
  cfg: ApiConfig,
  importer: Option[CalendarImporter] = None) extends RouteSupport {

  /**
   * iCalendar のファイルの取り込み（本文は iCalendar の文字列. 大きなファイルを受けるため、ほかの API と本文の上限を分ける）.
   * 予定を入れられるカレンダーにだけ取り込める. 同じファイルを取り込み直すと、変わった予定だけを上書きする.
   */
  val importRoutes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ POST -> Root / "api" / "calendars" / UuidVar(id) / "import" => postCalendarImport(req, id)
  }

  private def postCalendarImport(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    withCalendar(req, id, CalRole.Edit) {
      (
        u,
        c
      ) =>
        if c.subscribed then err(ErrorCode.ImportIntoSubscribedCalendar)
        else
          withImporter { im =>
            // 本文はそのままの文字列（circe の EntityDecoder に取られないよう bodyText で読む. 文字コードの指定がなければ UTF-8）
            req.bodyText.compile.string.attempt.flatMap {
              case Left(_: org.http4s.server.middleware.EntityLimiter.EntityTooLarge) =>
                err(ErrorCode.ImportFileTooLarge, s"ファイルが大きすぎます（${IcsImport.MaxBytes / 1024 / 1024}MB まで）")
              case Left(_) => err(ErrorCode.ImportFileUnreadable)
              case Right(text) =>
                val zone = Time.zone(u.timezone).getOrElse(java.time.ZoneOffset.UTC)
                im.importText(id, u.id, text, zone, removeMissing = false).flatMap {
                  case Left(e) => err(ErrorCode.ImportIcsInvalid, e)
                  case Right((r, _)) => Ok(r)
                }
            }
          }
    }

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "api" / "calendars" => getCalendars(req)
    case req @ POST -> Root / "api" / "calendars" => postCalendars(req)
    case req @ POST -> Root / "api" / "calendars" / "subscribe" => postCalendarsSubscribe(req)
    case req @ POST -> Root / "api" / "calendars" / UuidVar(id) / "sync" => postCalendarSync(req, id)
    case req @ PUT -> Root / "api" / "calendars" / UuidVar(id) => putCalendar(req, id)
    case req @ PATCH -> Root / "api" / "calendars" / UuidVar(id) / "prefs" => patchCalendarPrefs(req, id)
    case req @ DELETE -> Root / "api" / "calendars" / UuidVar(id) => deleteCalendar(req, id)
    case req @ GET -> Root / "api" / "calendars" / UuidVar(id) / "members" => getCalendarMembers(req, id)
    case req @ POST -> Root / "api" / "calendars" / UuidVar(id) / "members" => postCalendarMembers(req, id)
    case req @ DELETE -> Root / "api" / "calendars" / UuidVar(id) / "members" / UuidVar(uid) =>
      deleteCalendarMember(req, id, uid)
    case req @ GET -> Root / "api" / "team" / "calendars" => getTeamCalendars(req)
    case req @ POST -> Root / "api" / "team" / "calendars" / UuidVar(id) => postTeamCalendar(req, id)
    case req @ POST -> Root / "api" / "calendars" / UuidVar(id) / "public" => postCalendarPublic(req, id)
    case req @ DELETE -> Root / "api" / "calendars" / UuidVar(id) / "public" => deleteCalendarPublic(req, id)
    case GET -> Root / "ics" / file if file.endsWith(".ics") => getIcsFeed(file)
  }

  private def getCalendars(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req)(u => calendars.list(u.id).flatMap(dtos).flatMap(Ok(_)))

  private def postCalendars(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req)(u =>
      body[CalendarReq](req)(r => valid(checkReq(r))(ok => calendars.create(u.id, ok).flatMap(one(u, _))))
    )

  // iCalendar の URL を購読する（読めたらカレンダーを作る. 予定は読み取りだけで、定期的に読み直す）
  private def postCalendarsSubscribe(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req)(u =>
      body[SubscribeReq](req)(r =>
        withImporter(_.subscribe(u, r).flatMap(_.fold(err(ErrorCode.SubscribeFailed, _), one(u, _))))
      )
    )

  // 購読を今すぐ読み直す（所有者だけ）
  private def postCalendarSync(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    withCalendar(req, id, CalRole.Owner) {
      (
        u,
        c
      ) =>
        (c.sourceUrl, importer) match {
          case (Some(url), Some(im)) =>
            im.sync(Subscription(id, c.ownerId, url)).flatMap(_.fold(
              e => err(ErrorCode.SubscriptionRefreshFailed, s"読み直せませんでした: $e"),
              _ => one(u, id)
            ))
          case _ => err(ErrorCode.CalendarNotSubscribed)
        }
    }

  private def putCalendar(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    withCalendar(req, id, CalRole.Owner)(
      (
        u,
        _
      ) => body[CalendarReq](req)(r => valid(checkReq(r))(ok => calendars.update(id, ok) *> one(u, id)))
    )

  // 自分だけの表示の設定（色・表示するか）
  private def patchCalendarPrefs(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    withCalendar(req, id, CalRole.Busy) {
      (
        u,
        _
      ) =>
        body[CalendarPrefReq](req) { p =>
          valid(p.color.filter(_.nonEmpty).traverse(Check.color))(_ => calendars.setPrefs(u.id, id, p) *> one(u, id))
        }
    }

  // 所有者は削除（予定もすべて消える）、ほかの人は共有から抜ける（チームに公開されたものは一覧から外す）
  private def deleteCalendar(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    withCalendar(req, id, CalRole.Busy) {
      (
        u,
        c
      ) =>
        if c.role != CalRole.Owner then calendars.removeMember(id, u.id) *> NoContent()
        else if c.primary then err(ErrorCode.PrimaryCalendarNotDeletable)
        else calendars.delete(id) *> NoContent()
    }

  private def getCalendarMembers(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    withCalendar(req, id, CalRole.View)(
      (
        _,
        _
      ) => calendars.members(id).flatMap(Ok(_))
    )

  private def postCalendarMembers(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    withCalendar(req, id, CalRole.Owner) {
      (
        u,
        _
      ) =>
        body[MemberReq](req) { m =>
          if !Set(CalRole.View, CalRole.Edit).contains(m.role) then err(ErrorCode.ShareRoleInvalid)
          else
            users.resolve(m.user).flatMap {
              case None => err(ErrorCode.ShareUserNotFound)
              case Some(t) if t.id == u.id => err(ErrorCode.ShareWithSelf)
              case Some(t) => calendars.setMember(id, t.id, m.role) *> calendars.members(id).flatMap(Ok(_))
            }
        }
    }

  private def deleteCalendarMember(
    req: Request[IO],
    id: UUID,
    uid: UUID
  ): IO[Response[IO]] =
    withCalendar(req, id, CalRole.Owner)(
      (
        _,
        _
      ) => calendars.removeMember(id, uid) *> calendars.members(id).flatMap(Ok(_))
    )

  // チームの人が公開しているカレンダーを探す・自分の一覧に加える（外すのは DELETE /api/calendars/{id}）
  private def getTeamCalendars(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req)(u => calendars.team(u.id, param(req, "q").getOrElse(""), 200).flatMap(teamDtos).flatMap(Ok(_)))

  private def postTeamCalendar(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    authed(req)(u => calendars.addTeam(u.id, id).ifM(one(u, id), err(ErrorCode.TeamCalendarNotFound)))

  // ログインしない人への公開リンク: 作る・範囲を変える（regenerate で作り直す. 前のリンクは使えなくなる）・やめる
  private def postCalendarPublic(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    withCalendar(req, id, CalRole.Owner) {
      (
        u,
        c
      ) =>
        body[PublicLinkReq](req) { p =>
          if !Set(CalRole.Busy, CalRole.View).contains(p.access) then
            err(ErrorCode.PublishScopeInvalid)
          else {
            val token = Option.when(p.regenerate || c.publicToken.isEmpty)(Crypto.randomToken(24))
            calendars.setPublic(id, p.access, token) *> one(u, id)
          }
        }
    }

  private def deleteCalendarPublic(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    withCalendar(req, id, CalRole.Owner)(
      (
        u,
        _
      ) => calendars.stopPublic(id) *> one(u, id)
    )

  // iCalendar の配信（公開リンクを知っている人はだれでも読める. カレンダーアプリから URL で購読する）
  private def getIcsFeed(
    file: String
  ): IO[Response[IO]] =
    sbirds.observe.Telemetry.span("ics.feed")(calendars.byPublicToken(file.stripSuffix(".ics")).flatMap {
      case None => sbirds.observe.Metrics.count("skuaka.ics.requests", "result" -> "not_found") *>
          err(ErrorCode.IcsFeedNotFound)
      case Some(c) =>
        events.forCalendar(c.id).flatMap { rows =>
          val evs = rows.map(VisibleEvent.of(_, Some(c.access), None, 0).row)
            .map(r =>
              IcsEvent(
                s"${r.id}@skuaka",
                r.title,
                r.description,
                r.location,
                r.span,
                r.zone,
                r.recurrence,
                r.exdates,
                r.updatedAt
              )
            )
          sbirds.observe.Metrics.count("skuaka.ics.requests", "result" -> "ok") *>
            IO.pure(TextResponse(
              Status.Ok,
              Ics.calendar(c.name, evs, Instant.now()),
              MediaType.unsafeParse("text/calendar")
            )
              .putHeaders("Cache-Control" -> "no-cache"))
        }
    })

  private def link(
    r: CalendarRow
  ): Option[PublicLinkDto] =
    r.publicToken.map(t => PublicLinkDto(s"${cfg.publicUrl}/p/$t", s"${cfg.publicUrl}/ics/$t.ics", r.publicAccess))

  private def dtos(
    rows: List[CalendarRow]
  ): IO[List[CalendarDto]] =
    calendars.owners(rows.map(_.ownerId)).map { owners =>
      rows.map { r =>
        CalendarDto(
          r.id,
          r.name,
          r.description,
          r.color,
          r.myColor,
          r.visible,
          r.role,
          owner(owners, r.ownerId),
          r.primary,
          if r.role == CalRole.Owner then link(r) else None,
          r.teamAccess,
          r.viaTeam,
          r.sourceUrl.map(url =>
            SubscriptionDto(Option.when(r.role == CalRole.Owner)(url), r.syncedAt.map(_.toString), r.syncError)
          )
        )
      }
    }

  private def owner(
    owners: Map[UUID, UserBrief],
    id: UUID
  ): UserBrief = owners.getOrElse(id, UserBrief(id, "?", None, None))

  private def teamDtos(
    rows: List[TeamCalendarRow]
  ): IO[List[TeamCalendarDto]] =
    calendars.owners(rows.map(_.ownerId)).map(owners =>
      rows.map(r =>
        TeamCalendarDto(r.id, r.name, r.description, r.color, owner(owners, r.ownerId), r.primary, r.access, r.added)
      )
    )

  /** カレンダーの権限を確かめてから処理する（見られなければ 404、足りなければ 403）. */
  private def withCalendar(
    req: Request[IO],
    id: UUID,
    need: String
  )(
    f: (User, CalendarRow) => IO[Response[IO]]
  ): IO[Response[IO]] =
    authed(req) { u =>
      calendars.get(u.id, id).flatMap {
        case None => err(ErrorCode.CalendarNotFound)
        case Some(c) if CalRole.rank(c.role) < CalRole.rank(need) =>
          err(if need == CalRole.Owner then ErrorCode.CalendarOwnerOnly else ErrorCode.CalendarNotEditable)
        case Some(c) => f(u, c)
      }
    }

  private def checkReq(
    r: CalendarReq
  ): Either[String, CalendarReq] =
    for {
      name <- Either.cond(r.name.trim.nonEmpty, r.name.trim, "名前を入力してください").flatMap(Check.text(_, "名前", 100))
      desc <- Check.text(r.description.trim, "説明", 2000)
      col <- Check.color(r.color)
      acc <- r.teamAccess.traverse(a => Either.cond(TeamAccess.all.contains(a), a, "チームへの公開の範囲が正しくありません"))
    } yield CalendarReq(name, col, desc, acc)

  private def one(
    u: User,
    id: UUID
  ): IO[Response[IO]] =
    calendars.get(u.id, id).flatMap(_.traverse(r => dtos(List(r)).map(_.head))).flatMap {
      case Some(c) => Ok(c)
      case None => err(ErrorCode.CalendarNotFound)
    }

  /** 取り込み・購読ができないとき. */
  private def withImporter(
    f: CalendarImporter => IO[Response[IO]]
  ): IO[Response[IO]] =
    importer.fold(err(ErrorCode.ImportNotAvailable))(f)

}
