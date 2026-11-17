package skuaka.api

import cats.effect.*
import cats.syntax.all.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import skuaka.core.*
import skuaka.observe.{Metrics, Telemetry}
import skuaka.db.*
import skuaka.shared.*

import java.time.temporal.ChronoUnit
import java.time.{Instant, LocalDate, ZoneId}
import java.util.UUID
import scala.util.Try

/** 確認済みの予定の入力. */
final case class EventInput(calendarId: UUID, title: String, description: String, location: String, span: Span, zone: ZoneId,
    color: Option[String], recurrence: Option[Recurrence], attendees: List[UUID], reminders: List[Int], version: Option[Int])

/** 予定の一覧（繰り返しの展開）・作成・変更・削除・招待への返事・検索. */
final class EventRoutes(events: EventStore, calendars: CalendarStore, users: UserStore, val sessions: SessionAuth, skaka: Option[SkakaService] = None,
    notifier: Option[EventNotifier] = None)
    extends RouteSupport:

  private def canEdit(role: Option[String]): Boolean = role.exists(r => CalRole.rank(r) >= CalRole.rank(CalRole.Edit))

  private def occurrence(v: VisibleEvent, o: Span): OccurrenceDto =
    val r = v.row
    OccurrenceDto(r.id, r.calendarId, r.title, r.location, r.allDay, o.startText, o.endText, r.color, r.rrule.isDefined, o.key,
      canEdit(v.role), v.role.isEmpty, v.response, r.reminders, v.guests, v.busy)

  private def eventDto(v: VisibleEvent): IO[EventDto] =
    val r = v.row
    (if v.busy then IO.pure(Nil) else events.attendees(r.id), r.organizerId.flatTraverse(users.find)).mapN { (as, org) =>
      eventDtoOf(v, as, org.map(_.brief).getOrElse(UserBrief(r.organizerId.getOrElse(r.id), "（削除されたユーザー）", None, None)))
    }

  /** 予定あり・なしのみの予定は、繰り返しの決まりも伏せる. */
  private def eventDtoOf(v: VisibleEvent, attendees: List[AttendeeDto], organizer: UserBrief): EventDto =
    val r = v.row
    EventDto(r.id, r.calendarId, r.title, r.description, r.location, r.allDay, r.span.startText, r.span.endText, r.timezone, r.color,
      if v.busy then None else r.recurrence, attendees, r.reminders, organizer, canEdit(v.role), v.response, r.version, v.busy)

  /** 予定を [from, to) の回に展開して、日時の順に. */
  private def expand(vs: List[VisibleEvent], from: Instant, to: Instant, view: ZoneId): List[OccurrenceDto] =
    vs.flatMap(v => Expand.occurrences(v.row.span, v.row.zone, v.row.recurrence, v.row.exdates.toSet, from, to, view).map(occurrence(v, _)))
      .sortBy(o => (o.start, o.end))

  /** 表示の期間（from・to は表示のタイムゾーンでの日付. to は含まない. 400日まで）. */
  private def period(req: Request[IO], view: ZoneId)(f: (Instant, Instant) => IO[Response[IO]]): IO[Response[IO]] =
    (date(param(req, "from")), date(param(req, "to"))) match
      case (Some(a), Some(b)) if b.isAfter(a) && ChronoUnit.DAYS.between(a, b) <= 400 => f(a.atStartOfDay(view).toInstant, b.atStartOfDay(view).toInstant)
      case _ => err(Status.BadRequest, "from と to を yyyy-MM-dd で指定してください（400日まで）")

  /** 公開リンクのカレンダー（なければ 404）. */
  private def publicCal(token: String)(f: PublicCalendarDto => IO[Response[IO]]): IO[Response[IO]] =
    calendars.byPublicToken(token).flatMap(_.fold(err(Status.NotFound, "公開されたカレンダーが見つかりません"))(f))

  /** 公開リンクで見る予定（権限は公開の範囲. ゲストは返さない）. */
  private def asPublic(c: PublicCalendarDto, r: EventRow): VisibleEvent = VisibleEvent.of(r, Some(c.access), None, 0)

  private def respondEvent(u: User, id: UUID, meetingError: Option[String] = None): IO[Response[IO]] =
    events.visibleOne(u.id, id).flatMap {
      case Some(v) => eventDto(v).flatMap(e => Ok(e.copy(meetingError = meetingError)))
      case None    => NoContent()
    }

  // ---------------------------------------------------------------- Web 会議（skaka）

  /** 予定の場所にある会議室のコード. */
  private def meetingCode(r: EventRow): Option[String] = skaka.flatMap(s => Meeting.find(r.location, s.cfg.url).map(_._2))

  private def skakaEvent(r: EventRow): SkakaEvent = SkakaEvent(r.id, r.title, r.span.startText, r.span.endText, r.allDay)

  /** 頼まれていれば、会議室を作って場所に URL を入れる（付け済みなら何もしない）. 失敗したら理由（予定は保存したまま）. */
  private def attachMeeting(u: User, id: UUID, wanted: Option[Boolean]): IO[Option[String]] =
    if !wanted.contains(true) then IO.pure(None)
    else skaka match
      case None => IO.pure(Some("Web 会議（skaka）との連携は設定されていません"))
      case Some(s) =>
        events.get(id).flatMap {
          case Some(r) if meetingCode(r).isEmpty =>
            s.createRoom(u.id, skakaEvent(r)).flatMap {
              case Left(e)    => IO.pure(Some(s"会議室を作れませんでした（$e）"))
              case Right(url) => events.update(r.copy(location = Meeting.append(r.location, url)), None, None).as(None)
            }
          case _ => IO.pure(None)
        }

  /** タイトル・日時が変わったら、会議室に結び付けた予定の写しを変える（失敗しても保存は止めない）. */
  private def syncMeeting(u: User, before: EventRow): IO[Unit] =
    skaka.traverse_ { s =>
      events.get(before.id).flatMap(_.traverse_ { r =>
        meetingCode(r).traverse_(code => IO.whenA(r.title != before.title || r.span != before.span)(s.updateEvent(u.id, code, skakaEvent(r)).void))
      })
    }.handleError(_ => ())

  /** 新しく招待した人を数え、プッシュ通知で知らせる（裏で. 保存の応答は待たせない）. */
  private def invite(u: User, id: UUID, guests: List[UUID]): IO[Unit] =
    IO.whenA(guests.nonEmpty)(Metrics.add("skuaka.invitations.sent", guests.size.toLong) *> notifier.traverse_(_.invited(u, id, guests).start.void))

  /** 変更・削除の範囲（メトリクスの属性）. */
  private def scopeOf(req: Request[IO], m: EventRow): String =
    if m.recurrence.isEmpty then "single" else param(req, "scope").getOrElse(EditScope.All)

  private def check(r: EventReq): Either[String, EventInput] =
    for
      title <- Check.text(Option(r.title.trim).filter(_.nonEmpty).getOrElse("（タイトルなし）"), "タイトル", 300)
      desc  <- Check.text(r.description, "説明", 10000)
      loc   <- Check.text(r.location.trim, "場所", 500)
      span  <- Span.parse(r.allDay, r.start, r.end)
      zone  <- Time.zone(r.timezone).toRight("タイムゾーンが正しくありません")
      color <- r.color.filter(_.nonEmpty).traverse(Check.color)
      rec   <- r.recurrence.traverse(_.valid)
      rem   <- Either.cond(r.reminders.forall(m => m >= 0 && m <= Reminders.MaxMinutes), Reminders.normalize(r.reminders), "通知は4週間前までにしてください")
      _     <- Either.cond(rem.size <= Reminders.MaxCount, (), s"通知は${Reminders.MaxCount}つまでにしてください")
      _     <- Either.cond(r.attendees.size <= 200, (), "ゲストは200人までにしてください")
    yield EventInput(r.calendarId, title, desc, loc, span, zone, color, rec, r.attendees.distinct, rem, r.version)

  private def row(id: UUID, organizer: Option[UUID], in: EventInput, exdates: List[String], version: Int): EventRow =
    val (sa, ea, sd, ed) = in.span match
      case Span.Timed(s, e)  => (Some(s), Some(e), None, None)
      case Span.AllDay(s, e) => (None, None, Some(s), Some(e))
    EventRow(id, in.calendarId, organizer, in.title, in.description, in.location, in.span.isInstanceOf[Span.AllDay], sa, ea, sd, ed,
      in.zone.getId, in.color, in.recurrence.map(_.toRrule), exdates, in.reminders, version, Instant.now())

  /** 予定を書き込めるカレンダーか. */
  private def writable(u: User, calendarId: UUID)(f: => IO[Response[IO]]): IO[Response[IO]] =
    calendars.get(u.id, calendarId).flatMap {
      case Some(c) if CalRole.rank(c.role) >= CalRole.rank(CalRole.Edit) => f
      case _ => err(Status.Forbidden, "このカレンダーには予定を入れられません")
    }

  /** 変更・削除できる予定か. */
  private def editable(u: User, id: UUID)(f: VisibleEvent => IO[Response[IO]]): IO[Response[IO]] =
    events.visibleOne(u.id, id).flatMap {
      case None                         => err(Status.NotFound, "予定が見つかりません")
      case Some(v) if !canEdit(v.role) => err(Status.Forbidden, "この予定は変更できません")
      case Some(v)                      => f(v)
    }

  /**
   * 「すべての予定」の変更で、ある回を動かしたとき: 繰り返しの最初の回を同じ日数だけずらし、時刻・長さは入力に合わせる.
   * （3回目を1日後ろへドラッグしたら、繰り返し全体が1日後ろへずれる）
   */
  private def rebase(m: EventRow, key: String, in: EventInput): Span =
    Expand.keyDate(m.span, m.zone, key).fold(in.span) { occ =>
      val target = Expand.startDate(m.span, m.zone).plusDays(ChronoUnit.DAYS.between(occ, Expand.startDate(in.span, in.zone)))
      Expand.at(in.span, in.zone, target)
    }

  /**
   * 繰り返しの予定を、指定した回のところで切る（「この予定」はその回を除き、「これ以降」はその前の日で繰り返しを終える）.
   * 切った後の元の予定を返す（「すべて」・繰り返しでない予定・最初の回からの「これ以降」は None. 予定そのものを扱う）.
   */
  private def cut(req: Request[IO], m: EventRow): Option[EventRow] =
    (m.recurrence, param(req, "scope").getOrElse(EditScope.All), param(req, "occurrence")) match
      case (Some(_), EditScope.This, Some(k))                            => Some(m.copy(exdates = (m.exdates :+ k).distinct))
      case (Some(rule), EditScope.Following, Some(k)) if k != m.span.key => Some(m.copy(rrule = Some(Expand.endBefore(m.span, m.zone, rule, k).toRrule)))
      case _                                                             => None

  private def zoneOf(req: Request[IO], u: User): ZoneId =
    param(req, "tz").flatMap(Time.zone).orElse(Time.zone(u.timezone)).getOrElse(ZoneId.of("Asia/Tokyo"))

  private def date(s: Option[String]): Option[LocalDate] = s.flatMap(x => Try(LocalDate.parse(x)).toOption)

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    // 期間の予定（繰り返しは展開して1回ずつ）. from・to は表示のタイムゾーンでの日付（to は含まない）
    case req @ GET -> Root / "api" / "events" =>
      authed(req) { u =>
        val view = zoneOf(req, u)
        period(req, view)((from, to) => events.window(u.id, from, to).flatMap(vs => Ok(expand(vs, from, to, view))))
      }

    // 公開リンク（ログインしない人）: カレンダー・期間の予定・予定の詳細. tz は表示のタイムゾーン（既定 Asia/Tokyo）
    case GET -> Root / "api" / "public" / token =>
      publicCal(token)(Ok(_))

    case req @ GET -> Root / "api" / "public" / token / "events" =>
      publicCal(token) { c =>
        val view = param(req, "tz").flatMap(Time.zone).getOrElse(ZoneId.of("Asia/Tokyo"))
        period(req, view)((from, to) => events.calendarWindow(c.id, from, to).flatMap(rs => Ok(expand(rs.map(asPublic(c, _)), from, to, view))))
      }

    case GET -> Root / "api" / "public" / token / "events" / UuidVar(id) =>
      publicCal(token) { c =>
        events.get(id).flatMap {
          case Some(r) if r.calendarId == c.id => Ok(eventDtoOf(asPublic(c, r), Nil, c.owner))
          case _                               => err(Status.NotFound, "予定が見つかりません")
        }
      }

    // 探す（繰り返しの予定は、これから先の最初の回. なければ最初の回）
    case req @ GET -> Root / "api" / "events" / "search" =>
      authed(req) { u =>
        val view = zoneOf(req, u)
        val now  = Instant.now()
        param(req, "q").fold(Ok(List.empty[OccurrenceDto])) { q =>
          events.search(u.id, q, 100).flatMap { vs =>
            Ok(vs.map { v =>
              val r = v.row
              val next = r.recurrence.flatMap(rule =>
                Expand.occurrences(r.span, r.zone, Some(rule), r.exdates.toSet, now, now.plus(800, ChronoUnit.DAYS), view, limit = 1).headOption)
              occurrence(v, next.getOrElse(r.span))
            })
          }
        }
      }

    case req @ GET -> Root / "api" / "events" / UuidVar(id) =>
      authed(req)(u => events.visibleOne(u.id, id).flatMap {
        case Some(v) => eventDto(v).flatMap(Ok(_))
        case None    => err(Status.NotFound, "予定が見つかりません")
      })

    case req @ POST -> Root / "api" / "events" =>
      authed(req) { u =>
        body[EventReq](req)(r => valid(check(r)) { in =>
          writable(u, in.calendarId) {
            val id     = UUID.randomUUID()
            val guests = in.attendees.filterNot(_ == u.id)
            Telemetry.span("event.create", "calendar.id" -> in.calendarId.toString)(
              events.create(row(id, Some(u.id), in, Nil, 1), guests) *>
                Metrics.count("skuaka.events.created", "recurring" -> in.recurrence.isDefined.toString, "meeting" -> r.withMeeting.contains(true).toString) *>
                invite(u, id, guests) *>
                attachMeeting(u, id, r.withMeeting).flatMap(me => respondEvent(u, id, me).map(_.withStatus(Status.Created))))
          }
        })
      }

    // 変更. 繰り返しの予定は scope（this: この予定だけ / following: これ以降 / all: すべて）と occurrence（その回の元の開始）を付ける
    case req @ PUT -> Root / "api" / "events" / UuidVar(id) =>
      authed(req) { u =>
        body[EventReq](req)(r => valid(check(r)) { in =>
          editable(u, id) { v =>
            writable(u, in.calendarId) {
              val m      = v.row
              val guests = in.attendees.filterNot(g => m.organizerId.contains(g))
              Telemetry.span("event.update", "event.id" -> m.id.toString, "scope" -> scopeOf(req, m))(events.attendees(m.id).flatMap { before =>
                // 前からのゲストには知らせない（新しく加えた人だけ）
                val added = guests.filterNot(before.map(_.user.id).toSet)
                def done(target: UUID) = Metrics.count("skuaka.events.updated", "scope" -> scopeOf(req, m)) *> invite(u, target, added)
                cut(req, m) match
                  case Some(rest) =>
                    // 切った回から新しい予定にする（「この予定」は1回だけ）
                    val newId = UUID.randomUUID()
                    val next  = if param(req, "scope").contains(EditScope.This) then in.copy(recurrence = None) else in
                    events.split(rest, row(newId, m.organizerId, next, Nil, 1), guests) *>
                      done(newId) *>
                      attachMeeting(u, newId, r.withMeeting).flatMap(respondEvent(u, newId, _))
                  case None =>
                    val span = (m.recurrence, param(req, "occurrence"), in.recurrence) match
                      case (Some(_), Some(k), Some(_)) => rebase(m, k, in)
                      case _                           => in.span
                    val next = in.copy(span = span)
                    // 開始や繰り返しが変わったら、除いた回は意味がなくなるので捨てる
                    val keep = if span.key == m.span.key && in.recurrence.map(_.toRrule) == m.rrule then m.exdates else Nil
                    events.update(row(m.id, m.organizerId, next, keep, m.version), Some(guests), in.version).ifM(
                      done(m.id) *> syncMeeting(u, m) *> attachMeeting(u, m.id, r.withMeeting).flatMap(respondEvent(u, m.id, _)),
                      err(Status.Conflict, "ほかの人がこの予定を変更しました。開き直してください"))
              })
            }
          }
        })
      }

    case req @ DELETE -> Root / "api" / "events" / UuidVar(id) =>
      authed(req) { u =>
        editable(u, id)(v => Telemetry.span("event.delete", "event.id" -> id.toString, "scope" -> scopeOf(req, v.row))(
          cut(req, v.row).fold(events.delete(id))(events.update(_, None, None).void) *>
            Metrics.count("skuaka.events.deleted", "scope" -> scopeOf(req, v.row)) *> NoContent()))
      }

    // 招待への返事
    case req @ POST -> Root / "api" / "events" / UuidVar(id) / "rsvp" =>
      authed(req) { u =>
        body[RsvpReq](req) { r =>
          if !Rsvp.all.contains(r.response) then err(Status.BadRequest, "返事が正しくありません")
          else Telemetry.span("event.rsvp", "event.id" -> id.toString, "response" -> r.response)(events.rsvp(id, u.id, r.response).ifM(
            Metrics.count("skuaka.rsvp.responses", "response" -> r.response) *> respondEvent(u, id), err(Status.NotFound, "招待されていない予定です")))
        }
      }

    // 招待された予定を自分のカレンダーから外す
    case req @ DELETE -> Root / "api" / "events" / UuidVar(id) / "rsvp" =>
      authed(req)(u => events.leave(id, u.id) *> NoContent())
  }
