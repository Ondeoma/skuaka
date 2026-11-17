package skuaka.api

import cats.effect.*
import cats.syntax.all.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import skuaka.auth.Crypto
import skuaka.core.{Ics, IcsEvent}
import skuaka.db.*
import skuaka.shared.*

import java.time.Instant
import java.util.UUID

/** カレンダー・共有・表示の設定・iCalendar の配信. */
final class CalendarRoutes(calendars: CalendarStore, events: EventStore, users: UserStore, val sessions: SessionAuth, cfg: ApiConfig)
    extends RouteSupport:

  private def link(r: CalendarRow): Option[PublicLinkDto] =
    r.publicToken.map(t => PublicLinkDto(s"${cfg.publicUrl}/p/$t", s"${cfg.publicUrl}/ics/$t.ics", r.publicAccess))

  private def dtos(rows: List[CalendarRow]): IO[List[CalendarDto]] =
    calendars.owners(rows.map(_.ownerId)).map { owners =>
      rows.map { r =>
        CalendarDto(r.id, r.name, r.description, r.color, r.myColor, r.visible, r.role,
          owner(owners, r.ownerId), r.primary, if r.role == CalRole.Owner then link(r) else None, r.teamAccess, r.viaTeam)
      }
    }

  private def owner(owners: Map[UUID, UserBrief], id: UUID): UserBrief = owners.getOrElse(id, UserBrief(id, "?", None, None))

  private def teamDtos(rows: List[TeamCalendarRow]): IO[List[TeamCalendarDto]] =
    calendars.owners(rows.map(_.ownerId)).map(owners =>
      rows.map(r => TeamCalendarDto(r.id, r.name, r.description, r.color, owner(owners, r.ownerId), r.primary, r.access, r.added)))

  /** カレンダーの権限を確かめてから処理する（見られなければ 404、足りなければ 403）. */
  private def withCalendar(req: Request[IO], id: UUID, need: String)(f: (User, CalendarRow) => IO[Response[IO]]): IO[Response[IO]] =
    authed(req) { u =>
      calendars.get(u.id, id).flatMap {
        case None                                              => err(Status.NotFound, "カレンダーが見つかりません")
        case Some(c) if CalRole.rank(c.role) < CalRole.rank(need) =>
          err(Status.Forbidden, if need == CalRole.Owner then "カレンダーの所有者だけができます" else "このカレンダーの予定は変更できません")
        case Some(c) => f(u, c)
      }
    }

  private def checkReq(r: CalendarReq): Either[String, CalendarReq] =
    for
      name <- Either.cond(r.name.trim.nonEmpty, r.name.trim, "名前を入力してください").flatMap(Check.text(_, "名前", 100))
      desc <- Check.text(r.description.trim, "説明", 2000)
      col  <- Check.color(r.color)
      acc  <- r.teamAccess.traverse(a => Either.cond(TeamAccess.all.contains(a), a, "チームへの公開の範囲が正しくありません"))
    yield CalendarReq(name, col, desc, acc)

  private def one(u: User, id: UUID): IO[Response[IO]] =
    calendars.get(u.id, id).flatMap(_.traverse(r => dtos(List(r)).map(_.head))).flatMap {
      case Some(c) => Ok(c)
      case None    => err(Status.NotFound, "カレンダーが見つかりません")
    }

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "api" / "calendars" =>
      authed(req)(u => calendars.list(u.id).flatMap(dtos).flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "calendars" =>
      authed(req)(u => body[CalendarReq](req)(r => valid(checkReq(r))(ok => calendars.create(u.id, ok).flatMap(one(u, _)))))

    case req @ PUT -> Root / "api" / "calendars" / UuidVar(id) =>
      withCalendar(req, id, CalRole.Owner)((u, _) => body[CalendarReq](req)(r => valid(checkReq(r))(ok => calendars.update(id, ok) *> one(u, id))))

    // 自分だけの表示の設定（色・表示するか）
    case req @ PATCH -> Root / "api" / "calendars" / UuidVar(id) / "prefs" =>
      withCalendar(req, id, CalRole.Busy) { (u, _) =>
        body[CalendarPrefReq](req) { p =>
          valid(p.color.filter(_.nonEmpty).traverse(Check.color))(_ => calendars.setPrefs(u.id, id, p) *> one(u, id))
        }
      }

    // 所有者は削除（予定もすべて消える）、ほかの人は共有から抜ける（チームに公開されたものは一覧から外す）
    case req @ DELETE -> Root / "api" / "calendars" / UuidVar(id) =>
      withCalendar(req, id, CalRole.Busy) { (u, c) =>
        if c.role != CalRole.Owner then calendars.removeMember(id, u.id) *> NoContent()
        else if c.primary then err(Status.BadRequest, "自分のメインのカレンダーは削除できません")
        else calendars.delete(id) *> NoContent()
      }

    case req @ GET -> Root / "api" / "calendars" / UuidVar(id) / "members" =>
      withCalendar(req, id, CalRole.View)((_, _) => calendars.members(id).flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "calendars" / UuidVar(id) / "members" =>
      withCalendar(req, id, CalRole.Owner) { (u, _) =>
        body[MemberReq](req) { m =>
          if !Set(CalRole.View, CalRole.Edit).contains(m.role) then err(Status.BadRequest, "権限は view か edit で指定してください")
          else
            users.resolve(m.user).flatMap {
              case None                    => err(Status.NotFound, "そのユーザーが見つかりません（一度ログインした人だけと共有できます）")
              case Some(t) if t.id == u.id => err(Status.BadRequest, "自分とは共有できません")
              case Some(t)                 => calendars.setMember(id, t.id, m.role) *> calendars.members(id).flatMap(Ok(_))
            }
        }
      }

    case req @ DELETE -> Root / "api" / "calendars" / UuidVar(id) / "members" / UuidVar(uid) =>
      withCalendar(req, id, CalRole.Owner)((_, _) => calendars.removeMember(id, uid) *> calendars.members(id).flatMap(Ok(_)))

    // チームの人が公開しているカレンダーを探す・自分の一覧に加える（外すのは DELETE /api/calendars/{id}）
    case req @ GET -> Root / "api" / "team" / "calendars" =>
      authed(req)(u => calendars.team(u.id, param(req, "q").getOrElse(""), 200).flatMap(teamDtos).flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "team" / "calendars" / UuidVar(id) =>
      authed(req)(u => calendars.addTeam(u.id, id).ifM(one(u, id), err(Status.NotFound, "チームに公開されたカレンダーが見つかりません")))

    // ログインしない人への公開リンク: 作る・範囲を変える（regenerate で作り直す. 前のリンクは使えなくなる）・やめる
    case req @ POST -> Root / "api" / "calendars" / UuidVar(id) / "public" =>
      withCalendar(req, id, CalRole.Owner) { (u, c) =>
        body[PublicLinkReq](req) { p =>
          if !Set(CalRole.Busy, CalRole.View).contains(p.access) then err(Status.BadRequest, "公開の範囲は busy か view で指定してください")
          else
            val token = Option.when(p.regenerate || c.publicToken.isEmpty)(Crypto.randomToken(24))
            calendars.setPublic(id, p.access, token) *> one(u, id)
        }
      }

    case req @ DELETE -> Root / "api" / "calendars" / UuidVar(id) / "public" =>
      withCalendar(req, id, CalRole.Owner)((u, _) => calendars.stopPublic(id) *> one(u, id))

    // iCalendar の配信（公開リンクを知っている人はだれでも読める. カレンダーアプリから URL で購読する）
    case GET -> Root / "ics" / file if file.endsWith(".ics") =>
      skuaka.observe.Telemetry.span("ics.feed")(calendars.byPublicToken(file.stripSuffix(".ics")).flatMap {
        case None => skuaka.observe.Metrics.count("skuaka.ics.requests", "result" -> "not_found") *> NotFound()
        case Some(c) =>
          events.forCalendar(c.id).flatMap { rows =>
            val evs = rows.map(VisibleEvent.of(_, Some(c.access), None, 0).row)
              .map(r => IcsEvent(s"${r.id}@skuaka", r.title, r.description, r.location, r.span, r.zone, r.recurrence, r.exdates, r.updatedAt))
            skuaka.observe.Metrics.count("skuaka.ics.requests", "result" -> "ok") *>
              IO.pure(TextResponse(Status.Ok, Ics.calendar(c.name, evs, Instant.now()), MediaType.unsafeParse("text/calendar"))
                .putHeaders("Cache-Control" -> "no-cache"))
          }
      })
  }
