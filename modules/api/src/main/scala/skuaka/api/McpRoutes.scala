package skuaka.api

import cats.effect.*
import cats.syntax.all.*
import io.circe.{ACursor, Decoder, Json}
import io.circe.parser.parse
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.{`WWW-Authenticate`, Allow, Authorization}
import skuaka.core.{ErrorCode, Expand, Span, Time}
import skuaka.shared.*

import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.time.{DayOfWeek, Instant, LocalDate, LocalDateTime, LocalTime, ZoneId, ZonedDateTime}
import java.util.{Locale, UUID}
import scala.util.Try

/**
 * MCP（Model Context Protocol）のサーバー. POST /mcp に JSON-RPC を送る（Streamable HTTP. 応答は JSON で、SSE は使わない）.
 * 認証は API トークン（Bearer）だけ. 各ツールは REST API をそのトークンで内部から呼ぶので、権限は REST API と同じになる.
 * 日時はトークンの持ち主のタイムゾーンで読み書きする（オフセット付きの ISO 8601 も受け付ける）.
 */
final class McpRoutes(api: HttpRoutes[IO], sessions: SessionAuth, publicUrl: String, version: String)
  extends Http4sDsl[IO] {

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ POST -> Root / "mcp" => postMcp(req)
    case (GET | DELETE) -> Root / "mcp" => mcpMethodNotAllowed()
    case GET -> Root / "api" / "endpoints" => getEndpoints()
  }

  private def postMcp(req: Request[IO]): IO[Response[IO]] =
    req.headers.get[Authorization] match {
      case Some(auth @ Authorization(Credentials.Token(AuthScheme.Bearer, _))) =>
        sessions.fromRequest(Request[IO](Method.POST, Uri.unsafeFromString("/mcp")).putHeaders(auth)).flatMap {
          case None => unauthorized
          case Some(p) =>
            val call = Caller(auth)
            req.as[Json].attempt.flatMap {
              case Left(_) => BadRequest(rpcFail(Json.Null, -32700, "parse error"))
              case Right(j) =>
                j.asArray match {
                  case Some(batch) if batch.size > MaxBatch =>
                    BadRequest(rpcFail(Json.Null, -32600, s"一度に送れる要求は ${MaxBatch} 件までです"))
                  case Some(batch) =>
                    batch.toList.traverse(handle(call, p, _)).map(_.flatten).flatMap(rs =>
                      if rs.isEmpty then Accepted() else Ok(rs.asJson)
                    )
                  case None => handle(call, p, j).flatMap(_.fold(Accepted())(Ok(_)))
                }
            }
        }
      case _ => unauthorized
    }

  private def mcpMethodNotAllowed(): IO[Response[IO]] =
    MethodNotAllowed(Allow(Method.POST))

  private def getEndpoints(): IO[Response[IO]] =
    Ok(ApiCatalog.all.map(e =>
      Json.obj("method" -> e.method.asJson, "path" -> e.path.asJson, "description" -> e.description.asJson)
    ))

  private val base = publicUrl.stripSuffix("/")
  private val Versions = List("2025-06-18", "2025-03-26", "2024-11-05")

  // ------------------------------------------------------------------ REST API の内部呼び出し

  private final class Caller(auth: Authorization) {
    def apply(method: Method, uri: Uri, body: Option[Json] = None): IO[Either[String, Json]] = {
      val req0 = Request[IO](method, uri).putHeaders(auth)
      val req = body.fold(req0)(req0.withEntity(_))
      api.run(req).value.flatMap {
        case None => IO.pure(Left("404 見つかりません"))
        case Some(r) =>
          r.bodyText.compile.string.map { s =>
            val j = if s.isEmpty then Json.Null else parse(s).getOrElse(Json.fromString(s))
            if r.status.isSuccess then Right(j)
            else Left(s"${r.status.code} ${j.hcursor.get[String]("error").getOrElse(r.status.reason)}")
          }
      }
    }

    /** 呼んで、結果を型に読む. */
    def as[A: Decoder](method: Method, uri: Uri, body: Option[Json] = None): IO[Either[String, A]] =
      apply(method, uri, body).map(_.flatMap(_.as[A].left.map(e => s"応答を読めません: ${e.getMessage}")))
  }

  private def path(segments: String*): Uri =
    Uri(path = Uri.Path(segments.toVector.map(Uri.Path.Segment(_)), absolute = true))

  // ------------------------------------------------------------------ 日時

  /** トークンの持ち主と、そのタイムゾーン. */
  private final case class Ctx(call: Caller, me: MeDto) {
    val zone: ZoneId = Time.zone(me.timezone).getOrElse(ZoneId.of("Asia/Tokyo"))
    def today: LocalDate = LocalDate.now(zone)
  }

  private val dayNames = Vector("月", "火", "水", "木", "金", "土", "日")
  private def dayText(d: LocalDate): String =
    f"${d.getYear}%d/${d.getMonthValue}%02d/${d.getDayOfMonth}%02d(${dayNames(d.getDayOfWeek.getValue - 1)})"
  private val hm = DateTimeFormatter.ofPattern("HH:mm", Locale.ROOT)

  private def date(s: String): Either[String, LocalDate] =
    Try(LocalDate.parse(s.trim.take(10))).toEither.left.map(_ => s"日付は yyyy-MM-dd で指定してください: $s")

  /** 日時（2026-11-16T10:00、2026-11-16 10:00、オフセット付き）を UTC にする. 時刻だけ省いたら 0:00. */
  private def instant(s: String, zone: ZoneId): Either[String, Instant] = {
    val t = s.trim.replace(' ', 'T')
    Time.instant(t)
      .orElse(Try(LocalDateTime.parse(t).atZone(zone).toInstant).toOption)
      .orElse(Try(LocalDate.parse(t).atStartOfDay(zone).toInstant).toOption)
      .toRight(s"日時は yyyy-MM-ddTHH:mm で指定してください: $s")
  }

  /** 1回分の予定の日時の表示（トークンの持ち主のタイムゾーンで）. */
  private def whenText(allDay: Boolean, start: String, end: String, zone: ZoneId): String =
    if allDay then
      (Try(LocalDate.parse(start)).toOption, Try(LocalDate.parse(end)).toOption) match {
        case (Some(s), Some(e)) if s == e => s"${dayText(s)} 終日"
        case (Some(s), Some(e)) => s"${dayText(s)}〜${dayText(e)} 終日"
        case _ => s"$start〜$end"
      }
    else
      (Time.instant(start), Time.instant(end)) match {
        case (Some(s), Some(e)) =>
          val (a, b) = (s.atZone(zone), e.atZone(zone))
          if a.toLocalDate == b.toLocalDate then s"${dayText(a.toLocalDate)} ${a.format(hm)}–${b.format(hm)}"
          else s"${dayText(a.toLocalDate)} ${a.format(hm)}〜${dayText(b.toLocalDate)} ${b.format(hm)}"
        case _ => s"$start〜$end"
      }

  // ------------------------------------------------------------------ 引数

  private type Result = IO[Either[String, String]]

  private final case class Tool(name: String,
                                description: String,
                                schema: Json,
                                readOnly: Boolean,
                                destructive: Boolean = false)(val run: (Ctx, ACursor) => Result) {
    def json: Json = Json.obj(
      "name" -> name.asJson,
      "description" -> description.asJson,
      "inputSchema" -> schema,
      "annotations" -> Json.obj(
        "readOnlyHint" -> readOnly.asJson,
        "destructiveHint" -> destructive.asJson,
        "openWorldHint" -> false.asJson
      )
    )
  }

  private def schema(required: List[String], props: (String, Json)*): Json =
    Json.obj("type" -> "object".asJson, "properties" -> Json.obj(props*), "required" -> required.asJson)
  private def str(d: String): Json = Json.obj("type" -> "string".asJson, "description" -> d.asJson)
  private def int(d: String): Json = Json.obj("type" -> "integer".asJson, "description" -> d.asJson)
  private def bool(d: String): Json = Json.obj("type" -> "boolean".asJson, "description" -> d.asJson)
  private def strs(d: String): Json =
    Json.obj("type" -> "array".asJson, "items" -> Json.obj("type" -> "string".asJson), "description" -> d.asJson)
  private def obj(d: String): Json = Json.obj("type" -> "object".asJson, "description" -> d.asJson)
  private def oneOf(d: String, vs: String*): Json =
    Json.obj("type" -> "string".asJson, "enum" -> vs.toList.asJson, "description" -> d.asJson)

  private def need(a: ACursor, k: String): Either[String, String] =
    opt(a, k).filter(_.nonEmpty).toRight(s"$k を指定してください")
  private def opt(a: ACursor, k: String): Option[String] = a.get[String](k).toOption.map(_.trim)
  private def uuid(s: String): Either[String, UUID] =
    Try(UUID.fromString(s.trim)).toEither.left.map(_ => s"ID が正しくありません: $s")

  /** Either を IO の中でつなぐための小道具. */
  private def lift[A](e: Either[String, A]): IO[Either[String, A]] = IO.pure(e)
  extension [A](io: IO[Either[String, A]]) {
    private def andThen[B](f: A => IO[Either[String, B]]): IO[Either[String, B]] =
      io.flatMap(_.fold(e => IO.pure(Left(e)), f))
    private def mapR[B](f: A => B): IO[Either[String, B]] = io.map(_.map(f))
  }

  // ------------------------------------------------------------------ 名前から ID を引く

  private def calendars(c: Ctx): IO[Either[String, List[CalendarDto]]] =
    c.call.as[List[CalendarDto]](Method.GET, path("api", "calendars"))

  /** カレンダーの指定（名前・ID. 省けば自分のメインのカレンダー）. */
  private def calendar(c: Ctx, v: Option[String]): IO[Either[String, CalendarDto]] =
    calendars(c).mapR { cs =>
      v.filter(_.nonEmpty) match {
        case None => cs.find(x => x.primary && x.owner.id == c.me.id).orElse(cs.headOption).toRight("カレンダーがありません")
        case Some(n) =>
          cs.find(x => x.id.toString == n).orElse(cs.find(_.name == n)).orElse(cs.find(_.name.contains(n)))
            .toRight(s"カレンダー「$n」が見つかりません（list_calendars で確かめてください）")
      }
    }.map(_.flatten)

  /** ユーザーの指定（メール・名前・ID）を ID にする. */
  private def user(c: Ctx, v: String): IO[Either[String, UUID]] =
    uuid(v) match {
      case Right(id) => IO.pure(Right(id))
      case Left(_) =>
        c.call.as[List[UserBrief]](Method.GET, path("api", "users").withQueryParam("q", v)).mapR { us =>
          us.find(_.email.exists(_.equalsIgnoreCase(v))).orElse(us.find(_.name == v)).orElse(us.headOption)
            .map(_.id).toRight(s"ユーザー「$v」が見つかりません")
        }.map(_.flatten)
    }

  // ------------------------------------------------------------------ 予定の入力

  private val eventProps = List(
    "title" -> str("タイトル"),
    "start" -> str("開始. 日時は yyyy-MM-ddTHH:mm（自分のタイムゾーン）、終日の予定は yyyy-MM-dd"),
    "end" -> str("終わり. 日時、終日なら最後の日（その日を含む）. 省くと1時間（終日なら1日）"),
    "allDay" -> bool("終日の予定か（省くと start の形で決める）"),
    "calendar" -> str("カレンダーの名前か ID（省くと自分のメインのカレンダー）"),
    "description" -> str("説明"),
    "location" -> str("場所"),
    "attendees" -> strs("ゲスト（メール・名前・ユーザーID）. 置き換え"),
    "recurrence" -> str(
      "繰り返し（RRULE. 例 FREQ=WEEKLY;BYDAY=MO,WE、FREQ=MONTHLY;BYDAY=-1FR;COUNT=6、毎月15日と末日は FREQ=MONTHLY;BYMONTHDAY=15,-1、毎月の最終平日は FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-1。使える項目は FREQ・INTERVAL・BYDAY・BYMONTHDAY・BYMONTH・BYSETPOS・UNTIL・COUNT）. none で繰り返しをやめる"
    ),
    "reminders" -> Json.obj(
      "type" -> "array".asJson,
      "items" -> Json.obj("type" -> "integer".asJson),
      "description" -> "通知（何分前の一覧. 例 [10, 1440]. [] で通知しない. 5つまで）. 置き換え".asJson
    ),
    "color" -> str("予定の色 #rrggbb（空文字で既定の色）"),
    "withMeeting" -> bool("Web 会議（skaka）の会議室を作り、場所に URL を入れる（skaka と連携しているときだけ. 付け済みなら何もしない）")
  )

  /** 予定の入力を、元（変更なら今の予定）に重ねて EventReq にする. */
  private def eventReq(c: Ctx, a: ACursor, base: Option[(EventDto, Option[String])]): IO[Either[String, EventReq]] = {
    val cur = base.map(_._1)
    val occ = base.flatMap(_._2)
    val start = opt(a, "start").filter(_.nonEmpty)
    val end = opt(a, "end").filter(_.nonEmpty)
    val allDay =
      a.get[Boolean]("allDay").toOption.orElse(start.map(s => !s.contains('T') && !s.contains(' ') && s.length <= 10))
        .orElse(cur.map(_.allDay)).getOrElse(false)

    // 元の日時（繰り返しの1回を指定したらその回の日時）. 開始だけ変えたら長さを保つ
    val origin: Option[Span] = cur.flatMap { e =>
      val zone = Time.zone(e.timezone).getOrElse(c.zone)
      Span.parse(e.allDay, e.start, e.end).toOption.map(sp => occ.flatMap(Expand.occurrence(sp, zone, _)).getOrElse(sp))
    }
    val need = Left("start を指定してください")

    val span: Either[String, Span] =
      if allDay then
        for {
          s <- start.map(
            date
          ).orElse(origin.map(o => Right(o.bounds(c.zone)._1.atZone(c.zone).toLocalDate))).getOrElse(need)
          e <- end.map(date).getOrElse(Right(origin match {
            case Some(Span.AllDay(os, oe)) => s.plusDays(ChronoUnit.DAYS.between(os, oe))
            case _ => s
          }))
        } yield Span.AllDay(s, e)
      else
        for {
          s <- start.map(instant(_, c.zone)).orElse(origin.map(o => Right(o.bounds(c.zone)._1))).getOrElse(need)
          e <- end.map(instant(_, c.zone)).getOrElse(Right(origin match {
            case Some(Span.Timed(os, oe)) => s.plus(java.time.Duration.between(os, oe))
            case _ => s.plus(1, ChronoUnit.HOURS)
          }))
        } yield Span.Timed(s, e)

    val recurrence: Either[String, Option[Recurrence]] = opt(a, "recurrence") match {
      case None => Right(cur.flatMap(_.recurrence))
      case Some(r) if r.isEmpty || r.equalsIgnoreCase("none") => Right(None)
      case Some(r) => Recurrence.parse(r.toUpperCase).toRight(s"繰り返しを読めません: $r").flatMap(_.valid).map(Some(_))
    }

    // 省けば、作成は10分前、変更は今のまま
    val reminders = a.get[List[Int]]("reminders").toOption.getOrElse(cur.fold(List(10))(_.reminders))

    val color = opt(a, "color") match {
      case Some("") => None
      case Some(x) => Some(x)
      case None => cur.flatMap(_.color)
    }

    val attendees: IO[Either[String, List[UUID]]] = a.get[List[String]]("attendees").toOption match {
      case Some(vs) => vs.map(_.trim).filter(_.nonEmpty).traverse(user(c, _)).map(_.sequence)
      case None =>
        IO.pure(Right(cur.toList.flatMap(_.attendees.map(_.user.id)).filterNot(id => cur.exists(_.organizer.id == id))))
    }

    val cal: IO[Either[String, UUID]] = opt(a, "calendar").filter(_.nonEmpty) match {
      case None if cur.isDefined => IO.pure(Right(cur.get.calendarId))
      case v => calendar(c, v).mapR(_.id)
    }

    lift(span).andThen(sp =>
      lift(recurrence).andThen(rec =>
        cal.andThen(cid =>
          attendees.mapR { as =>
            EventReq(
              cid,
              opt(a, "title").orElse(cur.map(_.title)).getOrElse(""),
              opt(a, "description").orElse(cur.map(_.description)).getOrElse(""),
              opt(a, "location").orElse(cur.map(_.location)).getOrElse(""),
              allDay,
              sp.startText,
              sp.endText,
              cur.filter(_ => start.isEmpty).map(_.timezone).getOrElse(c.zone.getId),
              color,
              rec,
              as,
              reminders,
              cur.map(_.version),
              a.get[Boolean]("withMeeting").toOption
            )
          }
        )
      )
    )
  }

  private def scopeQuery(u: Uri, a: ACursor): Uri =
    List("scope" -> opt(a, "scope"), "occurrence" -> opt(a, "occurrence")).collect {
      case (k, Some(v)) if v.nonEmpty => k -> v
    }
      .foldLeft(u)((x, kv) => x.withQueryParam(kv._1, kv._2))

  private def occLine(c: Ctx, cals: Map[UUID, String], o: OccurrenceDto): String = {
    val tags = List(
      Option.when(o.recurring)("繰り返し"),
      o.myResponse.filter(_ => o.invited || o.guests > 0).map(r => s"返事: ${Rsvp.label(r)}"),
      Option.when(o.guests > 0)(s"ゲスト${o.guests}人")
    ).flatten
    s"- ${whenText(o.allDay, o.start, o.end, c.zone)} ${o.title}${
        if o.location.nonEmpty then s" @${o.location}" else ""
      }" +
      s" ［${cals.getOrElse(o.calendarId, "招待")}］${if tags.nonEmpty then tags.mkString("（", "・", "）") else ""}" +
      s" eventId=${o.eventId}${if o.recurring then s" occurrence=${o.occurrence}" else ""}"
  }

  private def occurrences(c: Ctx, from: LocalDate, to: LocalDate): IO[Either[String, List[OccurrenceDto]]] =
    c.call.as[List[OccurrenceDto]](
      Method.GET,
      path("api", "events").withQueryParam("from", from.toString)
        .withQueryParam("to", to.plusDays(1).toString).withQueryParam("tz", c.zone.getId)
    )

  private def range(c: Ctx, a: ACursor, days: Int): Either[String, (LocalDate, LocalDate)] =
    for {
      f <- opt(a, "from").filter(_.nonEmpty).fold(Right(c.today))(date)
      t <- opt(a, "to").filter(_.nonEmpty).fold(Right(f.plusDays(days - 1)))(date)
      _ <- Either.cond(!t.isBefore(f) && ChronoUnit.DAYS.between(f, t) < 400, (), "to は from 以降で、400日以内にしてください")
    } yield (f, t)

  private def eventText(c: Ctx, e: EventDto, cals: Map[UUID, String]): String =
    List(
      Some(s"# ${e.title}"),
      Some(s"日時: ${whenText(e.allDay, e.start, e.end, c.zone)}${if e.recurrence.isDefined then "（最初の回）" else ""}"),
      e.recurrence.map(r => s"繰り返し: ${r.describe}（${r.toRrule}）"),
      Option.when(e.location.nonEmpty)(s"場所: ${e.location}"),
      Some(s"カレンダー: ${cals.getOrElse(e.calendarId, e.calendarId.toString)}"),
      Some(s"主催: ${e.organizer.name}"),
      Option.when(e.attendees.nonEmpty)("ゲスト: " + e.attendees.map(x =>
        s"${x.user.name}${x.user.email.fold("")(m => s" <$m>")}（${Rsvp.label(x.response)}）"
      ).mkString("、")),
      e.myResponse.map(r => s"自分の返事: ${Rsvp.label(r)}"),
      Some(s"通知: ${Reminders.text(e.reminders)}"),
      Some(s"変更できる: ${if e.canEdit then "はい" else "いいえ"}"),
      Option.when(e.description.nonEmpty)(s"\n${e.description}"),
      Some(s"\neventId=${e.id} version=${e.version} タイムゾーン=${e.timezone}")
    ).flatten.mkString("\n")

  /** 場所（Web 会議の URL を含む）と、会議室を作れなかった理由. */
  private def meetingText(e: EventDto): String =
    Option.when(e.location.nonEmpty)(s"\n場所: ${e.location}").getOrElse("") +
      e.meetingError.fold("")(m => s"\n※ $m（予定は保存しました）")

  private def calNames(c: Ctx): IO[Map[UUID, String]] =
    calendars(c).map(_.getOrElse(Nil).map(x => x.id -> x.name).toMap)

  // ------------------------------------------------------------------ 空き時間

  private def hhmm(s: Option[String], d: LocalTime): Either[String, LocalTime] =
    s.filter(_.nonEmpty).fold(Right(d))(x =>
      Try(LocalTime.parse(if x.length == 4 then "0" + x else x)).toEither.left.map(_ => s"時刻は HH:mm で指定してください: $x")
    )

  private def freeSlots(c: Ctx,
                        os: List[OccurrenceDto],
                        from: LocalDate,
                        to: LocalDate,
                        minutes: Int,
                        ds: LocalTime,
                        de: LocalTime,
                        weekends: Boolean): List[(ZonedDateTime, ZonedDateTime)] = {
    val busy = os.filterNot(_.allDay).filterNot(_.myResponse.contains(Rsvp.Declined))
      .flatMap(o => (Time.instant(o.start), Time.instant(o.end)).tupled).sortBy(_._1)
    val now = Instant.now()
    Iterator.iterate(from)(_.plusDays(1)).takeWhile(!_.isAfter(to))
      .filter(d => weekends || !Set(DayOfWeek.SATURDAY, DayOfWeek.SUNDAY).contains(d.getDayOfWeek))
      .flatMap { d =>
        val s0 = d.atTime(ds).atZone(c.zone).toInstant
        val e0 = d.atTime(de).atZone(c.zone).toInstant
        val start = if now.isAfter(s0) then now.truncatedTo(ChronoUnit.MINUTES) else s0
        val (slots, last) =
          busy.filter((s, e) => s.isBefore(e0) && e.isAfter(start)).foldLeft((List.empty[(Instant, Instant)], start)) {
            case ((acc, cur), (s, e)) =>
              (if s.isAfter(cur) then acc :+ (cur -> s) else acc, if e.isAfter(cur) then e else cur)
          }
        (if e0.isAfter(last) then slots :+ (last -> e0) else slots)
          .filter((s, e) => ChronoUnit.MINUTES.between(s, e) >= minutes)
          .map((s, e) => (s.atZone(c.zone), e.atZone(c.zone)))
      }.toList
  }

  // ------------------------------------------------------------------ ツール

  private val occurrenceProps = List(
    "occurrence" -> str("繰り返しの予定の1回（list_events の occurrence の値）"),
    "scope" -> oneOf("繰り返しの予定の範囲. this: この回だけ / following: この回以降 / all: すべて（既定）", "this", "following", "all")
  )

  private val tools: List[Tool] = List(
    Tool("get_me", "自分（この API トークンの持ち主）の情報とタイムゾーン、今日の日付.", schema(Nil), readOnly = true) {
      (c, _) =>
        IO.pure(Right(
          s"${c.me.name}${c.me.email.fold("")(m => s" <$m>")}\nタイムゾーン: ${c.me.timezone}\n今日: ${dayText(c.today)}\n今: ${ZonedDateTime.now(c.zone).format(hm)}\nuserId=${c.me.id}"
        ))
    },
    Tool("list_calendars", "見られるカレンダーの一覧（名前・自分の権限・所有者）.", schema(Nil), readOnly = true) {
      (c, _) =>
        calendars(c).mapR(_.map(x =>
          s"- ${x.name}${if x.primary then "（メイン）" else ""}: ${CalRole.label(x.role)}、所有者 ${x.owner.name}" +
            s"${if x.viaTeam then "、チームに公開" else ""}${if x.visible then "" else "、非表示"} id=${x.id}"
        ).mkString("\n"))
    },
    Tool(
      "list_team_calendars",
      "チームの人が公開しているカレンダーを探す（加えると list_events・find_free_time で予定が見える）.",
      schema(Nil, "query" -> str("持ち主の名前・メールアドレス、カレンダーの名前（省けばすべて）")),
      readOnly = true
    ) {
      (c, a) =>
        c.call.as[List[TeamCalendarDto]](
          Method.GET,
          path("api", "team", "calendars").withQueryParam("q", opt(a, "query").getOrElse(""))
        )
          .mapR(xs =>
            if xs.isEmpty then "見つかりません"
            else
              xs.map(x =>
                s"- ${x.owner.name}: ${x.name}${if x.primary then "（メイン）" else ""}、${TeamAccess.label(x.access)}" +
                  s"${if x.added then "、一覧に追加済み" else ""} id=${x.id}"
              ).mkString("\n")
          )
    },
    Tool(
      "add_team_calendar",
      "チームに公開されたカレンダーを自分の一覧に加える（list_team_calendars の id）.",
      schema(List("calendarId"), "calendarId" -> str("カレンダーの ID")),
      readOnly = false
    ) {
      (c, a) =>
        lift(need(a, "calendarId").flatMap(uuid)).andThen(id =>
          c.call.as[CalendarDto](Method.POST, path("api", "team", "calendars", id.toString)).mapR(x =>
            s"「${x.name}」（${x.owner.name}）を一覧に加えました"
          )
        )
    },
    Tool(
      "list_events",
      "期間の予定を日時の順に（繰り返しは1回ずつ）. 日時は自分のタイムゾーン.",
      schema(
        Nil,
        "from" -> str("開始日 yyyy-MM-dd（既定は今日）"),
        "to" -> str("終了日 yyyy-MM-dd（この日を含む. 既定は from から7日間）"),
        "calendar" -> str("カレンダーの名前か ID で絞る")
      ),
      readOnly = true
    ) {
      (c, a) =>
        lift(range(c, a, 7)).andThen((f, t) =>
          opt(a, "calendar").filter(_.nonEmpty).traverse(v => calendar(c, Some(v))).map(_.sequence).andThen(only =>
            (occurrences(c, f, t), calNames(c)).tupled.map((r, names) =>
              r.map { os =>
                val xs = os.filter(o => only.forall(_.id == o.calendarId))
                (s"${dayText(f)}〜${dayText(t)}: ${xs.size}件" :: xs.map(occLine(c, names, _))).mkString("\n")
              }
            )
          )
        )
    },
    Tool(
      "search_events",
      "予定をタイトル・場所・説明で探す（繰り返しはこれから先の最初の回）.",
      schema(List("query"), "query" -> str("探す語")),
      readOnly = true
    ) {
      (c, a) =>
        lift(need(a, "query")).andThen(q =>
          (
            c.call.as[List[OccurrenceDto]](
              Method.GET,
              path("api", "events", "search").withQueryParam("q", q).withQueryParam("tz", c.zone.getId)
            ),
            calNames(c)
          ).tupled
            .map((r, names) => r.map(os => (s"${os.size}件" :: os.map(occLine(c, names, _))).mkString("\n")))
        )
    },
    Tool(
      "get_event",
      "予定の詳細（説明・ゲストと返事・繰り返し・version）.",
      schema(List("eventId"), "eventId" -> str("予定の ID")),
      readOnly = true
    ) {
      (c, a) =>
        lift(need(a, "eventId").flatMap(uuid)).andThen(id =>
          (c.call.as[EventDto](Method.GET, path("api", "events", id.toString)), calNames(c)).tupled.map((r, names) =>
            r.map(eventText(c, _, names))
          )
        )
    },
    Tool("create_event", "予定を作る. ゲストを入れると招待される.", schema(List("title", "start"), eventProps*), readOnly = false) {
      (c, a) =>
        eventReq(c, a, None).andThen(r =>
          c.call.as[EventDto](Method.POST, path("api", "events"), Some(r.asJson))
            .mapR(e =>
              s"予定を作りました: ${e.title} ${whenText(e.allDay, e.start, e.end, c.zone)}${meetingText(e)}\neventId=${e.id}\n$base/"
            )
        )
    },
    Tool(
      "update_event",
      "予定を変える（指定した項目だけ）. 繰り返しの予定は occurrence と scope で範囲を選ぶ.",
      schema(List("eventId"), (("eventId" -> str("予定の ID")) :: occurrenceProps ++ eventProps)*),
      readOnly = false
    ) {
      (c, a) =>
        lift(need(a, "eventId").flatMap(uuid)).andThen(id =>
          c.call.as[EventDto](Method.GET, path("api", "events", id.toString)).andThen { cur =>
            val occ = opt(a, "occurrence").filter(_.nonEmpty).filter(_ => cur.recurrence.isDefined)
            val thisOnly = opt(a, "scope").contains(EditScope.This)
            eventReq(
              c,
              if thisOnly then a.withFocus(_.mapObject(_.remove("recurrence"))) else a,
              Some(cur -> occ)
            ).andThen { r =>
              val req = if thisOnly then r.copy(recurrence = None) else r
              c.call.as[EventDto](Method.PUT, scopeQuery(path("api", "events", id.toString), a), Some(req.asJson))
                .mapR(e =>
                  s"予定を変えました: ${e.title} ${whenText(e.allDay, e.start, e.end, c.zone)}${meetingText(e)}\neventId=${e.id}"
                )
            }
          }
        )
    },
    Tool(
      "delete_event",
      "予定を削除する. 繰り返しの予定は occurrence と scope で範囲を選ぶ（scope を省くとすべて）.",
      schema(List("eventId"), (("eventId" -> str("予定の ID")) :: occurrenceProps)*),
      readOnly = false,
      destructive = true
    ) {
      (c, a) =>
        lift(need(a, "eventId").flatMap(uuid)).andThen(id =>
          c.call(Method.DELETE, scopeQuery(path("api", "events", id.toString), a)).mapR(_ => "削除しました")
        )
    },
    Tool(
      "respond_to_event",
      "招待された予定に返事をする.",
      schema(
        List("eventId", "response"),
        "eventId" -> str("予定の ID"),
        "response" -> oneOf("返事", Rsvp.Accepted, Rsvp.Declined, Rsvp.Tentative)
      ),
      readOnly = false
    ) {
      (c, a) =>
        (
          need(a, "eventId").flatMap(uuid),
          need(a, "response").filterOrElse(Rsvp.all.contains, "response は accepted / declined / tentative です")
        ).tupled match {
          case Left(e) => IO.pure(Left(e))
          case Right((id, r)) =>
            c.call.as[EventDto](Method.POST, path("api", "events", id.toString, "rsvp"), Some(RsvpReq(r).asJson))
              .mapR(e => s"「${e.title}」に「${Rsvp.label(r)}」と返事しました")
        }
    },
    Tool(
      "find_free_time",
      "自分（と people の人）の予定が入っていない時間を探す. 自分の予定は自分のカレンダーと招待された予定、ほかの人の予定はその人のカレンダーのうち見えるもの" +
        "（チームに公開されたものは list_team_calendars・add_team_calendar で加える）. 終日の予定と、不参加と返事した予定は空きとみなす.",
      schema(
        List("durationMinutes"),
        "durationMinutes" -> int("必要な長さ（分）"),
        "people" -> Json.obj(
          "type" -> "array".asJson,
          "items" -> Json.obj("type" -> "string".asJson),
          "description" -> "一緒に空いている時間を探す人（メールアドレス・名前・ID）".asJson
        ),
        "from" -> str("開始日 yyyy-MM-dd（既定は今日）"),
        "to" -> str("終了日（既定は from から5日間）"),
        "dayStart" -> str("1日の始まり HH:mm（既定 09:00）"),
        "dayEnd" -> str("1日の終わり HH:mm（既定 18:00）"),
        "includeWeekends" -> bool("土日も探すか（既定 false）")
      ),
      readOnly = true
    ) {
      (c, a) =>
        (
          range(c, a, 5),
          a.get[Int]("durationMinutes").toOption.filter(m => m > 0 && m <= 1440).toRight("durationMinutes は 1〜1440 です"),
          hhmm(opt(a, "dayStart"), LocalTime.of(9, 0)),
          hhmm(opt(a, "dayEnd"), LocalTime.of(18, 0))
        ).tupled match {
          case Left(e) => IO.pure(Left(e))
          case Right(((f, t), m, ds, de)) =>
            a.downField("people").as[List[String]].getOrElse(Nil).traverse(user(c, _)).map(_.sequence).andThen(people =>
              (occurrences(c, f, t), calendars(c)).tupled.map(_.tupled).mapR {
                (os, cs) =>
                  val owner = cs.map(x => x.id -> x.owner.id).toMap
                  val who = (c.me.id :: people).toSet
                  // 自分: 自分のカレンダーと招待された予定. ほかの人: その人のカレンダーの予定
                  val mine = os.filter(o => o.invited || o.myResponse.isDefined || owner.get(o.calendarId).exists(who))
                  val unseen = people.distinct.filterNot(p => cs.exists(_.owner.id == p))
                  val slots = freeSlots(c, mine, f, t, m, ds, de, a.get[Boolean]("includeWeekends").getOrElse(false))
                  val note = Option.when(unseen.nonEmpty)(
                    s"（${unseen.size}人はカレンダーが見えないため、予定を考えていません. list_team_calendars で探して加えてください）"
                  )
                  val body =
                    if slots.isEmpty then s"${dayText(f)}〜${dayText(t)} に ${m}分の空きはありません"
                    else
                      (s"${m}分以上の空き（${c.zone.getId}）:" :: slots.map((s, e) =>
                        s"- ${dayText(s.toLocalDate)} ${s.format(hm)}–${e.format(hm)}"
                      )).mkString("\n")
                  (body :: note.toList).mkString("\n")
              }
            )
        }
    },
    Tool(
      "list_api_endpoints",
      "REST API の一覧. 専用のツールがない操作（カレンダーの作成・共有など）は call_api で行える.",
      schema(Nil),
      readOnly = true
    ) {
      (_, _) =>
        IO.pure(Right(ApiCatalog.all.map(e => s"${e.method} ${e.path} — ${e.description}").mkString("\n")))
    },
    Tool(
      "call_api",
      "任意の REST API を呼ぶ（list_api_endpoints を参照）. 権限はこのトークンの持ち主と同じ. 日時は UTC の ISO 8601.",
      schema(
        List("method", "path"),
        "method" -> str("GET / POST / PUT / PATCH / DELETE"),
        "path" -> str("/api/ で始まるパス（クエリを含めてよい）"),
        "body" -> obj("JSON の本文")
      ),
      readOnly = false
    ) {
      (c, a) =>
        (need(a, "method").map(_.toUpperCase), need(a, "path")).tupled match {
          case Left(e) => IO.pure(Left(e))
          case Right((m, p)) =>
            (
              Method.fromString(m).toOption.filter(x => Set("GET", "POST", "PUT", "PATCH", "DELETE")(x.name)),
              Uri.fromString(p).toOption.filter(u =>
                u.path.renderString.startsWith("/api/") && !u.path.renderString.contains("..")
              )
            ) match {
              case (Some(method), Some(uri)) =>
                c.call(method, uri, a.downField("body").focus.filterNot(_.isNull)).mapR(_.spaces2)
              case (None, _) => IO.pure(Left("method は GET / POST / PUT / PATCH / DELETE です"))
              case _ => IO.pure(Left("path は /api/ で始めてください"))
            }
        }
    }
  )

  private val byName = tools.map(t => t.name -> t).toMap

  // ------------------------------------------------------------------ JSON-RPC

  private def ok(id: Json, result: Json): Json = Json.obj("jsonrpc" -> "2.0".asJson, "id" -> id, "result" -> result)
  private def rpcFail(id: Json, code: Int, msg: String): Json =
    Json.obj("jsonrpc" -> "2.0".asJson, "id" -> id, "error" -> Json.obj("code" -> code.asJson, "message" -> msg.asJson))
  private def toolResult(r: Either[String, String]): Json =
    Json.obj(
      "content" -> Json.arr(Json.obj("type" -> "text".asJson, "text" -> r.merge.asJson)),
      "isError" -> r.isLeft.asJson
    )

  private def handle(call: Caller, p: Principal, msg: Json): IO[Option[Json]] = {
    val c = msg.hcursor
    val params = c.downField("params")
    c.downField("id").focus match {
      case None => IO.pure(None) // 通知（notifications/initialized など）には返さない
      case Some(id) =>
        c.get[String]("method").getOrElse("") match {
          case "initialize" =>
            val asked = params.get[String]("protocolVersion").toOption
            IO.pure(Some(ok(
              id,
              Json.obj(
                "protocolVersion" -> asked.filter(Versions.contains).getOrElse(Versions.head).asJson,
                "capabilities" -> Json.obj("tools" -> Json.obj("listChanged" -> false.asJson)),
                "serverInfo" -> Json.obj("name" -> "skuaka".asJson, "version" -> version.asJson),
                "instructions" ->
                  ("skuaka（チームのカレンダー）の予定を読み書きするツールです。日時はこのトークンの持ち主のタイムゾーン（get_me で分かる）で、" +
                    "yyyy-MM-ddTHH:mm の形で渡します。予定は list_events・search_events で探し、eventId で get_event・update_event・delete_event を呼びます。" +
                    "繰り返しの予定の1回だけを変えるときは list_events の occurrence と scope=this を渡します。" +
                    "会議の時間を決めるときは find_free_time が使えます。専用のツールがない操作は list_api_endpoints で REST API を調べ、call_api で行えます。" +
                    (if p.canWrite then "" else "このトークンは参照のみなので、変更するツールは使えません。")).asJson
              )
            )))
          case "ping" => IO.pure(Some(ok(id, Json.obj())))
          case "tools/list" =>
            IO.pure(Some(ok(id, Json.obj("tools" -> tools.filter(t => p.canWrite || t.readOnly).map(_.json).asJson))))
          case "tools/call" =>
            val name = params.get[String]("name").getOrElse("")
            byName.get(name) match {
              case None => IO.pure(Some(rpcFail(id, -32602, s"unknown tool: $name")))
              case Some(t) if !p.canWrite && !t.readOnly =>
                IO.pure(Some(ok(id, toolResult(Left("参照のみの API トークンでは変更できません")))))
              case Some(t) =>
                val args = params.downField("arguments").focus.filter(_.isObject).getOrElse(Json.obj()).hcursor
                t.run(Ctx(call, p.user.me), args).handleError(e => Left(s"内部エラー: ${e.getMessage}")).map(r =>
                  Some(ok(id, toolResult(r)))
                )
            }
          case "resources/list" => IO.pure(Some(ok(id, Json.obj("resources" -> Json.arr()))))
          case "prompts/list" => IO.pure(Some(ok(id, Json.obj("prompts" -> Json.arr()))))
          case other => IO.pure(Some(rpcFail(id, -32601, s"method not found: $other")))
        }
    }
  }

  private val MaxBatch = 20

  private def unauthorized: IO[Response[IO]] =
    sbirds.http.Errors.respond(ErrorCode.McpTokenRequired).map(_.putHeaders(`WWW-Authenticate`(Challenge(
      "Bearer",
      "skuaka"
    ))))
}
