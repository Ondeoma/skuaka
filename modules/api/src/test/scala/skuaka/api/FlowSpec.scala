package skuaka.api

import cats.effect.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.client.Client
import org.http4s.headers.{Cookie, Location}
import org.http4s.implicits.*
import org.typelevel.ci.CIString
import skuaka.auth.{OidcClient, OidcConfig}
import skuaka.db.*
import skuaka.devidp.DevIdp
import skuaka.shared.*

import java.util.UUID

/**
 * OIDC のログインから予定の操作までを、開発用の IdP（DevIdp）と本物の DB（skuaka_test）で通しで確かめる.
 * DB は docker-compose.dev.yml（:54349）. TEST_DATABASE_URL で変えられる.
 */
class FlowSpec extends munit.CatsEffectSuite:

  private val dbCfg = DbConfig(sys.env.getOrElse("TEST_DATABASE_URL", "jdbc:postgresql://localhost:54349/skuaka_test"), "skuaka", "skuaka", 4)
  private val issuer = "http://localhost:9999"
  private val apiCfg = ApiConfig(cookieSecure = false, publicUrl = "http://localhost:8095")

  /** IdP とアプリ（ルート）を用意する. DB はまっさらにする. */
  private val fixture = ResourceSuiteLocalFixture("app", for
    _  <- Resource.eval(IO.blocking {
            val c = java.sql.DriverManager.getConnection(dbCfg.url, dbCfg.user, dbCfg.password)
            try c.createStatement().execute("drop schema public cascade; create schema public") finally c.close()
          })
    _  <- Resource.eval(Database.migrate(dbCfg))
    xa <- Database.transactor(dbCfg)
    idp <- Resource.eval(DevIdp.create(issuer, "skuaka", "skuaka-test-secret-0123456789"))
    oidc <- Resource.eval(OidcClient.create(OidcConfig(issuer, "skuaka", "skuaka-test-secret-0123456789", "http://localhost:8095/auth/callback"),
              Client.fromHttpApp(idp.routes.orNotFound)))
  yield
    val users = new UserStore(xa); val cals = new CalendarStore(xa, users); val events = new EventStore(xa)
    val sess  = new SessionAuth(users, apiCfg)
    val app = ApiRoutes(Some(oidc), users, cals, events, sess, apiCfg).orNotFound
    (idp.routes.orNotFound, app, xa))

  override def munitFixtures = List(fixture)

  private def location(r: Response[IO]): Uri = r.headers.get[Location].map(_.uri).getOrElse(fail(s"no Location (${r.status})"))

  /** IdP でログインして、セッションの Cookie を受け取る. */
  private def login(name: String, email: String): IO[String] =
    val (idp, app, _) = fixture()
    for
      r1 <- app.run(Request[IO](Method.GET, uri"/auth/login?returnTo=/week"))
      _   = assertEquals(r1.status, Status.Found)
      auth = location(r1)
      _   = assertEquals(auth.path.renderString, "/authorize")
      _   = assertEquals(auth.params.get("code_challenge_method"), Some("S256"))
      form = UrlForm(List("client_id", "redirect_uri", "state", "nonce", "code_challenge").map(k => k -> auth.params(k)) ++
               List("name" -> name, "email" -> email)*)
      r2 <- idp.run(Request[IO](Method.POST, uri"/authorize").withEntity(form))
      cb  = location(r2)
      _   = assertEquals(cb.path.renderString, "/auth/callback")
      r3 <- app.run(Request[IO](Method.GET, Uri(path = cb.path, query = cb.query)))
      _   = assertEquals(r3.status, Status.Found, "callback")
      _   = assertEquals(location(r3).renderString, "/week")
    yield r3.cookies.find(_.name == "skuaka_session").map(_.content).getOrElse(fail("no session cookie"))

  private def call(token: String, m: Method, path: String, body: Option[Json] = None): IO[Response[IO]] =
    val (_, app, _) = fixture()
    val base = Request[IO](m, Uri.unsafeFromString(path)).putHeaders(Cookie(RequestCookie("skuaka_session", token)), Header.Raw(CIString("X-Skuaka"), "1"))
    app.run(body.fold(base)(base.withEntity(_)))

  private def json[A: io.circe.Decoder](r: Response[IO]): IO[A] =
    if r.status.isSuccess then r.as[A] else r.bodyText.compile.string.flatMap(t => IO(fail(s"${r.status}: $t")))

  test("OIDC でログインし、自分のカレンダーができる. 同じ state は2回使えない") {
    for
      t   <- login("山田 太郎", "yamada@example.com")
      me  <- call(t, Method.GET, "/api/me").flatMap(json[MeDto])
      _    = assertEquals(me.name, "山田 太郎")
      cs  <- call(t, Method.GET, "/api/calendars").flatMap(json[List[CalendarDto]])
      _    = assertEquals(cs.map(c => (c.name, c.role, c.primary)), List(("山田 太郎", CalRole.Owner, true)))
      // ログアウトすると、IdP の end_session_endpoint へ（id_token_hint 付き）
      out <- call(t, Method.POST, "/auth/logout").flatMap(json[Json])
      url  = out.hcursor.get[String]("redirect").getOrElse("")
      _    = assert(url.startsWith(s"$issuer/logout?") && url.contains("id_token_hint="), url)
      gone <- call(t, Method.GET, "/api/me")
      _    = assertEquals(gone.status, Status.Unauthorized)
    yield ()
  }

  test("不正な state・X-Skuaka のない更新・未ログインは拒否する") {
    val (_, app, _) = fixture()
    for
      r1 <- app.run(Request[IO](Method.GET, uri"/auth/callback?code=x&state=unknown"))
      _   = assertEquals(r1.status, Status.BadRequest)
      t  <- login("佐藤", "sato@example.com")
      r2 <- app.run(Request[IO](Method.POST, uri"/api/calendars").putHeaders(Cookie(RequestCookie("skuaka_session", t)))
              .withEntity(CalendarReq("x", "#112233", "").asJson))
      _   = assertEquals(r2.status, Status.Forbidden)
      r3 <- app.run(Request[IO](Method.GET, uri"/api/calendars"))
      _   = assertEquals(r3.status, Status.Unauthorized)
    yield ()
  }

  test("繰り返しの予定: 展開・この予定だけ変更・これ以降を削除・招待と返事・共有") {
    for
      a    <- login("鈴木", "suzuki@example.com")
      b    <- login("田中", "tanaka@example.com")
      bMe  <- call(b, Method.GET, "/api/me").flatMap(json[MeDto])
      cal  <- call(a, Method.GET, "/api/calendars").flatMap(json[List[CalendarDto]]).map(_.head)
      req   = EventReq(cal.id, "定例", "", "会議室A", false, "2026-11-02T01:00:00Z", "2026-11-02T02:00:00Z", "Asia/Tokyo", None,
                Some(Recurrence(Recurrence.Weekly)), List(bMe.id), List(10, 1440, 10), None)
      ev   <- call(a, Method.POST, "/api/events", Some(req.asJson)).flatMap(json[EventDto])
      // 通知は複数（重複を除いて近い順）. 6つ以上は断る
      _     = assertEquals(ev.reminders, List(10, 1440))
      many <- call(a, Method.POST, "/api/events", Some(req.copy(reminders = List(0, 5, 10, 30, 60, 1440)).asJson))
      _     = assertEquals(many.status, Status.BadRequest)
      occ  <- call(a, Method.GET, "/api/events?from=2026-11-01&to=2026-12-01&tz=Asia/Tokyo").flatMap(json[List[OccurrenceDto]])
      _     = assertEquals(occ.map(_.start), List("2026-11-02T01:00:00Z", "2026-11-09T01:00:00Z", "2026-11-16T01:00:00Z", "2026-11-23T01:00:00Z", "2026-11-30T01:00:00Z"))
      // 3回目だけ1時間遅らせる
      third = occ(2)
      moved <- call(a, Method.PUT, s"/api/events/${ev.id}?scope=this&occurrence=${third.occurrence}",
                 Some(req.copy(start = "2026-11-16T02:00:00Z", end = "2026-11-16T03:00:00Z").asJson)).flatMap(json[EventDto])
      _     = assertEquals(moved.recurrence, None)
      // 4回目以降を消す
      _    <- call(a, Method.DELETE, s"/api/events/${ev.id}?scope=following&occurrence=${occ(3).occurrence}")
      occ2 <- call(a, Method.GET, "/api/events?from=2026-11-01&to=2026-12-01&tz=Asia/Tokyo").flatMap(json[List[OccurrenceDto]])
      _     = assertEquals(occ2.map(_.start), List("2026-11-02T01:00:00Z", "2026-11-09T01:00:00Z", "2026-11-16T02:00:00Z"))
      // 招待された田中さんにも見え、返事ができるが、変更はできない
      occB <- call(b, Method.GET, "/api/events?from=2026-11-01&to=2026-12-01&tz=Asia/Tokyo").flatMap(json[List[OccurrenceDto]])
      _     = assertEquals(occB.map(o => (o.invited, o.canEdit)).distinct, List((true, false)))
      rs   <- call(b, Method.POST, s"/api/events/${ev.id}/rsvp", Some(RsvpReq(Rsvp.Accepted).asJson)).flatMap(json[EventDto])
      _     = assertEquals(rs.myResponse, Some(Rsvp.Accepted))
      no   <- call(b, Method.PUT, s"/api/events/${ev.id}", Some(req.asJson))
      _     = assertEquals(no.status, Status.Forbidden)
      // 共有すると田中さんも変更できる. 古い version での変更は 409
      _    <- call(a, Method.POST, s"/api/calendars/${cal.id}/members", Some(MemberReq("tanaka@example.com", CalRole.Edit).asJson)).flatMap(json[List[MemberDto]])
      cur  <- call(b, Method.GET, s"/api/events/${ev.id}").flatMap(json[EventDto])
      ok   <- call(b, Method.PUT, s"/api/events/${ev.id}", Some(req.copy(title = "定例（変更）", start = cur.start, end = cur.end,
                recurrence = cur.recurrence, version = Some(cur.version)).asJson)).flatMap(json[EventDto])
      _     = assertEquals(ok.title, "定例（変更）")
      stale <- call(a, Method.PUT, s"/api/events/${ev.id}", Some(req.copy(version = Some(cur.version)).asJson))
      _     = assertEquals(stale.status, Status.Conflict)
      // iCalendar の配信
      ics  <- call(a, Method.POST, s"/api/calendars/${cal.id}/public", Some(PublicLinkReq(CalRole.View).asJson)).flatMap(json[CalendarDto])
      path  = ics.publicLink.map(_.icsUrl.stripPrefix(apiCfg.publicUrl)).getOrElse(fail("no ics url"))
      text <- call("", Method.GET, path).flatMap(_.bodyText.compile.string)
      _     = assert(text.startsWith("BEGIN:VCALENDAR\r\n") && text.contains("SUMMARY:定例（変更）\r\n") &&
                text.contains("RRULE:FREQ=WEEKLY;UNTIL=20261122\r\n"), text)
    yield ()
  }

  private def bearer(token: String, m: Method, path: String, body: Option[Json] = None): IO[Response[IO]] =
    val (_, app, _) = fixture()
    val base = Request[IO](m, Uri.unsafeFromString(path)).putHeaders(org.http4s.headers.Authorization(Credentials.Token(AuthScheme.Bearer, token)))
    app.run(body.fold(base)(base.withEntity(_)))

  // for 式でタプルを分解するため
  extension [A](io: IO[A]) private def withFilter(p: A => Boolean): IO[A] = io.flatMap(a => if p(a) then IO.pure(a) else IO.raiseError(new MatchError(a)))

  /** MCP のツールを呼び、結果の文字列と isError を返す. */
  private def mcp(token: String, tool: String, args: Json): IO[(String, Boolean)] =
    bearer(token, Method.POST, "/mcp", Some(Json.obj("jsonrpc" -> "2.0".asJson, "id" -> 1.asJson, "method" -> "tools/call".asJson,
      "params" -> Json.obj("name" -> tool.asJson, "arguments" -> args)))).flatMap(json[Json]).map { j =>
      val r = j.hcursor.downField("result")
      (r.downField("content").downArray.get[String]("text").getOrElse(j.noSpaces), r.get[Boolean]("isError").getOrElse(true))
    }

  test("API トークン: Bearer で REST API を呼べる. 参照のみは変更できない. 管理は画面からだけ. 失効したら使えない") {
    for
      a     <- login("鈴木 一郎", "suzuki@example.com")
      rw    <- call(a, Method.POST, "/api/tokens", Some(ApiTokenReq("スクリプト", TokenScope.Write, Some(30)).asJson)).flatMap(json[ApiTokenCreated])
      ro    <- call(a, Method.POST, "/api/tokens", Some(ApiTokenReq("参照", TokenScope.Read, None).asJson)).flatMap(json[ApiTokenCreated])
      _      = assert(rw.token.startsWith("skuaka_") && rw.token.startsWith(rw.info.prefix))
      me    <- bearer(rw.token, Method.GET, "/api/me").flatMap(json[MeDto])
      _      = assertEquals(me.name, "鈴木 一郎")
      cals  <- bearer(ro.token, Method.GET, "/api/calendars").flatMap(json[List[CalendarDto]])
      cal    = cals.find(_.primary).getOrElse(fail("no primary"))
      req    = EventReq(cal.id, "API から", "", "", false, "2026-12-01T01:00:00Z", "2026-12-01T02:00:00Z", "Asia/Tokyo", None, None, Nil, Nil, None)
      // 書き込みのトークンは X-Skuaka なしで作れる
      made  <- bearer(rw.token, Method.POST, "/api/events", Some(req.asJson))
      _      = assertEquals(made.status, Status.Created)
      // 参照のみは 403
      ro1   <- bearer(ro.token, Method.POST, "/api/events", Some(req.asJson))
      _      = assertEquals(ro1.status, Status.Forbidden)
      // トークンでトークンは作れない
      tk    <- bearer(rw.token, Method.POST, "/api/tokens", Some(ApiTokenReq("x", TokenScope.Write, None).asJson))
      _      = assertEquals(tk.status, Status.Forbidden)
      list  <- call(a, Method.GET, "/api/tokens").flatMap(json[List[ApiTokenDto]])
      _      = assertEquals(list.map(_.name).toSet, Set("スクリプト", "参照"))
      _      = assert(list.find(_.name == "スクリプト").flatMap(_.lastUsedAt).isDefined)
      // 失効・でたらめなトークンは 401（WWW-Authenticate 付き）
      _     <- call(a, Method.DELETE, s"/api/tokens/${rw.info.id}")
      gone  <- bearer(rw.token, Method.GET, "/api/me")
      _      = assertEquals(gone.status, Status.Unauthorized)
      _      = assert(gone.headers.get[org.http4s.headers.`WWW-Authenticate`].isDefined)
      bad   <- bearer("skuaka_nope", Method.GET, "/api/me")
      _      = assertEquals(bad.status, Status.Unauthorized)
    yield ()
  }

  test("MCP: 初期化・ツールの一覧・予定の作成と一覧・繰り返しの1回の変更・空き時間・参照のみの制限") {
    for
      a    <- login("山本 三郎", "yamamoto@example.com")
      _    <- login("小林 四郎", "kobayashi@example.com")
      rw   <- call(a, Method.POST, "/api/tokens", Some(ApiTokenReq("AI アシスタント", TokenScope.Write, None).asJson)).flatMap(json[ApiTokenCreated]).map(_.token)
      ro   <- call(a, Method.POST, "/api/tokens", Some(ApiTokenReq("AI アシスタント（参照）", TokenScope.Read, None).asJson)).flatMap(json[ApiTokenCreated]).map(_.token)
      none <- fixture()._2.run(Request[IO](Method.POST, uri"/mcp").withEntity(Json.obj()))
      _     = assertEquals(none.status, Status.Unauthorized)
      init <- bearer(rw, Method.POST, "/mcp", Some(Json.obj("jsonrpc" -> "2.0".asJson, "id" -> 1.asJson, "method" -> "initialize".asJson,
                "params" -> Json.obj("protocolVersion" -> "2025-06-18".asJson)))).flatMap(json[Json])
      _     = assertEquals(init.hcursor.downField("result").get[String]("protocolVersion"), Right("2025-06-18"))
      note <- bearer(rw, Method.POST, "/mcp", Some(Json.obj("jsonrpc" -> "2.0".asJson, "method" -> "notifications/initialized".asJson)))
      _     = assertEquals(note.status, Status.Accepted)
      tl   <- bearer(ro, Method.POST, "/mcp", Some(Json.obj("jsonrpc" -> "2.0".asJson, "id" -> 2.asJson, "method" -> "tools/list".asJson))).flatMap(json[Json])
      names = tl.hcursor.downField("result").downField("tools").as[List[Json]].getOrElse(Nil).flatMap(_.hcursor.get[String]("name").toOption)
      _     = assert(names.contains("list_events") && !names.contains("create_event"), names)
      // 作成（自分のタイムゾーン Asia/Tokyo の 10:00 = UTC 01:00）. 毎週月曜、3回
      (c1, e1) <- mcp(rw, "create_event", Json.obj("title" -> "週次MTG".asJson, "start" -> "2026-11-16T10:00".asJson, "end" -> "2026-11-16T11:00".asJson,
                    "recurrence" -> "FREQ=WEEKLY;BYDAY=MO;COUNT=3".asJson, "attendees" -> List("kobayashi@example.com").asJson, "location" -> "会議室A".asJson))
      _     = assert(!e1, c1)
      id    = c1.linesIterator.collectFirst { case l if l.startsWith("eventId=") => l.stripPrefix("eventId=") }.getOrElse(fail(c1))
      (l1, _) <- mcp(rw, "list_events", Json.obj("from" -> "2026-11-16".asJson, "to" -> "2026-12-06".asJson))
      _     = assert(l1.contains("3件") && l1.contains("2026/11/16(月) 10:00–11:00 週次MTG @会議室A") && l1.contains("ゲスト1人"), l1)
      // 2回目だけ 14:00 へ
      (u1, ue) <- mcp(rw, "update_event", Json.obj("eventId" -> id.asJson, "scope" -> "this".asJson, "occurrence" -> "2026-11-23T01:00:00Z".asJson,
                    "start" -> "2026-11-23T14:00".asJson))
      _     = assert(!ue, u1)
      (l2, _) <- mcp(rw, "list_events", Json.obj("from" -> "2026-11-23".asJson, "to" -> "2026-11-23".asJson))
      _     = assert(l2.contains("2026/11/23(月) 14:00–15:00 週次MTG"), l2)
      // 空き時間（11/16 の 9:00〜12:00 で 60分）
      (f1, _) <- mcp(rw, "find_free_time", Json.obj("from" -> "2026-11-16".asJson, "to" -> "2026-11-16".asJson, "durationMinutes" -> 60.asJson,
                   "dayStart" -> "09:00".asJson, "dayEnd" -> "12:00".asJson))
      _     = assert(f1.contains("09:00–10:00") && f1.contains("11:00–12:00"), f1)
      // 参照のみのトークンで変更すると isError
      (r1, re) <- mcp(ro, "delete_event", Json.obj("eventId" -> id.asJson))
      _     = assert(re && r1.contains("参照のみ"), r1)
      (g1, _) <- mcp(ro, "get_event", Json.obj("eventId" -> id.asJson))
      _     = assert(g1.contains("繰り返し: ") && g1.contains("小林 四郎") && g1.contains("通知: 10分前"), g1)
      (x1, xe) <- mcp(rw, "call_api", Json.obj("method" -> "GET".asJson, "path" -> "/api/me".asJson))
      _     = assert(!xe && x1.contains("山本 三郎"), x1)
    yield ()
  }

  test("チームへの公開: 探して加える. 予定あり・なしのみは内容を伏せ、探しても出ない. 公開をやめると見えなくなる") {
    val dec = "/api/events?from=2026-12-01&to=2026-12-02&tz=Asia/Tokyo"
    for
      a    <- login("石井 五郎", "ishii@team.example.com")
      b    <- login("森 六子", "mori@team.example.com")
      cal  <- call(a, Method.GET, "/api/calendars").flatMap(json[List[CalendarDto]]).map(_.head)
      _     = assertEquals((cal.primary, cal.teamAccess), (true, TeamAccess.Busy))
      _    <- call(a, Method.POST, "/api/events", Some(EventReq(cal.id, "人事面談", "評価の話", "会議室B", false, "2026-12-01T01:00:00Z",
                "2026-12-01T02:00:00Z", "Asia/Tokyo", None, Some(Recurrence(Recurrence.Weekly)), Nil, Nil, None).asJson)).flatMap(json[EventDto])
      found <- call(b, Method.GET, "/api/team/calendars?q=ishii").flatMap(json[List[TeamCalendarDto]])
      _     = assertEquals(found.map(c => (c.id, c.access, c.added)), List((cal.id, TeamAccess.Busy, false)))
      added <- call(b, Method.POST, s"/api/team/calendars/${cal.id}").flatMap(json[CalendarDto])
      _     = assertEquals((added.role, added.viaTeam), (CalRole.Busy, true))
      // 自分のカレンダーと同じ色にならない
      _     = assertNotEquals(added.displayColor, Palette.Default)
      // 予定あり・なしのみ: 時間だけ見え、題名・場所・説明は伏せる. 題名で探しても出ない. 共有の相手は見えない
      os   <- call(b, Method.GET, dec).flatMap(json[List[OccurrenceDto]])
      _     = assertEquals(os.map(o => (o.title, o.location, o.busy, o.canEdit, o.start)),
                List((TeamAccess.BusyTitle, "", true, false, "2026-12-01T01:00:00Z")))
      ev   <- call(b, Method.GET, s"/api/events/${os.head.eventId}").flatMap(json[EventDto])
      _     = assertEquals((ev.title, ev.description, ev.recurrence, ev.attendees, ev.busy), (TeamAccess.BusyTitle, "", None, Nil, true))
      sr   <- call(b, Method.GET, "/api/events/search?q=%E9%9D%A2%E8%AB%87").flatMap(json[List[OccurrenceDto]])
      _     = assertEquals(sr, Nil)
      ms   <- call(b, Method.GET, s"/api/calendars/${cal.id}/members")
      _     = assertEquals(ms.status, Status.Forbidden)
      // MCP の空き時間: 自分だけなら空いているが、石井さんと一緒なら 10:00〜11:00 が埋まっている
      tk   <- call(b, Method.POST, "/api/tokens", Some(ApiTokenReq("mcp", TokenScope.Read, None).asJson)).flatMap(json[ApiTokenCreated])
      free  = (people: List[String]) => mcp(tk.token, "find_free_time", Json.obj("from" -> "2026-12-01".asJson, "to" -> "2026-12-01".asJson,
                "durationMinutes" -> 60.asJson, "dayStart" -> "09:00".asJson, "dayEnd" -> "12:00".asJson, "people" -> people.asJson))
      (f1, _) <- free(Nil)
      _     = assert(f1.contains("09:00–12:00"), f1)
      (f2, _) <- free(List("ishii@team.example.com"))
      _     = assert(f2.contains("09:00–10:00") && f2.contains("11:00–12:00") && !f2.contains("09:00–12:00"), f2)
      // 詳細を公開すると内容が見える
      _    <- call(a, Method.PUT, s"/api/calendars/${cal.id}", Some(CalendarReq(cal.name, cal.color, "", Some(TeamAccess.View)).asJson)).flatMap(json[CalendarDto])
      os2  <- call(b, Method.GET, dec).flatMap(json[List[OccurrenceDto]])
      _     = assertEquals(os2.map(o => (o.title, o.location, o.busy)), List(("人事面談", "会議室B", false)))
      // 公開をやめると一覧からも予定からも消える
      _    <- call(a, Method.PUT, s"/api/calendars/${cal.id}", Some(CalendarReq(cal.name, cal.color, "", Some(TeamAccess.None)).asJson)).flatMap(json[CalendarDto])
      cs   <- call(b, Method.GET, "/api/calendars").flatMap(json[List[CalendarDto]])
      _     = assert(!cs.exists(_.id == cal.id), cs)
      os3  <- call(b, Method.GET, dec).flatMap(json[List[OccurrenceDto]])
      _     = assertEquals(os3, Nil)
      gone <- call(b, Method.GET, "/api/team/calendars?q=ishii").flatMap(json[List[TeamCalendarDto]])
      _     = assertEquals(gone, Nil)
      re   <- call(b, Method.POST, s"/api/team/calendars/${cal.id}")
      _     = assertEquals(re.status, Status.NotFound)
    yield ()
  }

  test("公開リンク: ログインしない人が閲覧できる. 予定あり・なしのみは伏せる. ゲストは出さない. 作り直し・停止で前のリンクは使えない") {
    val dec = "/events?from=2026-12-01&to=2026-12-02&tz=Asia/Tokyo"
    for
      a    <- login("村上 七海", "murakami@public.example.com")
      b    <- login("井上 八郎", "inoue@public.example.com")
      bMe  <- call(b, Method.GET, "/api/me").flatMap(json[MeDto])
      cal  <- call(a, Method.GET, "/api/calendars").flatMap(json[List[CalendarDto]]).map(_.head)
      ev   <- call(a, Method.POST, "/api/events", Some(EventReq(cal.id, "採用面接", "候補者A", "応接室", false, "2026-12-01T05:00:00Z",
                "2026-12-01T06:00:00Z", "Asia/Tokyo", None, None, List(bMe.id), Nil, None).asJson)).flatMap(json[EventDto])
      // 所有者でなければ公開できない
      no   <- call(b, Method.POST, s"/api/calendars/${cal.id}/public", Some(PublicLinkReq(CalRole.Busy).asJson))
      _     = assertEquals(no.status, Status.NotFound)
      pub  <- call(a, Method.POST, s"/api/calendars/${cal.id}/public", Some(PublicLinkReq(CalRole.Busy).asJson)).flatMap(json[CalendarDto])
      ln    = pub.publicLink.getOrElse(fail("no link"))
      token = ln.pageUrl.split("/p/").last
      _     = assertEquals((ln.access, ln.icsUrl.endsWith(s"/ics/$token.ics")), (CalRole.Busy, true))
      // ログインせずに（Cookie なしで）見る. 予定あり・なしのみは伏せる
      pc   <- call("", Method.GET, s"/api/public/$token").flatMap(json[PublicCalendarDto])
      _     = assertEquals((pc.name, pc.owner.name, pc.owner.email, pc.access), (cal.name, "村上 七海", None, CalRole.Busy))
      os   <- call("", Method.GET, s"/api/public/$token$dec").flatMap(json[List[OccurrenceDto]])
      _     = assertEquals(os.map(o => (o.title, o.location, o.busy, o.canEdit, o.guests)), List((TeamAccess.BusyTitle, "", true, false, 0)))
      ics1 <- call("", Method.GET, s"/ics/$token.ics").flatMap(_.bodyText.compile.string)
      _     = assert(ics1.contains(s"SUMMARY:${TeamAccess.BusyTitle}\r\n") && !ics1.contains("採用面接") && !ics1.contains("応接室"), ics1)
      // 詳細を公開: 題名・場所は見えるが、ゲストは出さない
      _    <- call(a, Method.POST, s"/api/calendars/${cal.id}/public", Some(PublicLinkReq(CalRole.View).asJson)).flatMap(json[CalendarDto])
      os2  <- call("", Method.GET, s"/api/public/$token$dec").flatMap(json[List[OccurrenceDto]])
      _     = assertEquals(os2.map(o => (o.title, o.location, o.busy, o.guests)), List(("採用面接", "応接室", false, 0)))
      d    <- call("", Method.GET, s"/api/public/$token/events/${ev.id}").flatMap(json[EventDto])
      _     = assertEquals((d.title, d.attendees, d.canEdit, d.organizer.email), ("採用面接", Nil, false, None))
      // ほかのカレンダーの予定は、このリンクでは見えない
      other <- call(b, Method.GET, "/api/calendars").flatMap(json[List[CalendarDto]]).map(_.find(_.primary).get)
      oe   <- call(b, Method.POST, "/api/events", Some(EventReq(other.id, "私用", "", "", false, "2026-12-01T07:00:00Z",
                "2026-12-01T08:00:00Z", "Asia/Tokyo", None, None, Nil, Nil, None).asJson)).flatMap(json[EventDto])
      x    <- call("", Method.GET, s"/api/public/$token/events/${oe.id}")
      _     = assertEquals(x.status, Status.NotFound)
      // 作り直すと前のリンクは使えない. やめるとどれも使えない
      re   <- call(a, Method.POST, s"/api/calendars/${cal.id}/public", Some(PublicLinkReq(CalRole.View, regenerate = true).asJson)).flatMap(json[CalendarDto])
      token2 = re.publicLink.map(_.pageUrl.split("/p/").last).getOrElse(fail("no link"))
      _     = assertNotEquals(token2, token)
      old  <- call("", Method.GET, s"/api/public/$token")
      _     = assertEquals(old.status, Status.NotFound)
      st   <- call(a, Method.DELETE, s"/api/calendars/${cal.id}/public").flatMap(json[CalendarDto])
      _     = assertEquals(st.publicLink, None)
      gone <- call("", Method.GET, s"/api/public/$token2$dec")
      _     = assertEquals(gone.status, Status.NotFound)
      icsG <- call("", Method.GET, s"/ics/$token2.ics")
      _     = assertEquals(icsG.status, Status.NotFound)
    yield ()
  }
