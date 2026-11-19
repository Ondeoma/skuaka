package skuaka.api

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.client.Client
import org.http4s.headers.{Cookie, Location}
import org.http4s.implicits.*
import org.typelevel.ci.CIString
import sbirds.auth.{OidcClient, OidcConfig}
import skuaka.db.*
import doobie.implicits.*
import sbirds.devidp.DevIdp
import skuaka.shared.*

import java.util.UUID

/**
 * OIDC のログインから予定の操作までを、開発用の IdP（DevIdp）と本物の DB（skuaka_test）で通しで確かめる.
 * DB は docker-compose.dev.yml（:54349）. TEST_DATABASE_URL で変えられる.
 */
class FlowSpec extends munit.CatsEffectSuite {

  private val dbCfg = DbConfig(
    sys.env.getOrElse("TEST_DATABASE_URL", "jdbc:postgresql://localhost:54349/skuaka_test"),
    "skuaka",
    "skuaka",
    4
  )
  private val issuer = "http://localhost:9999"
  private val kakapoCb = "http://localhost:5199/api/connections/callback"
  private val apiCfg = ApiConfig(
    cookieSecure = false,
    publicUrl = "http://localhost:8095",
    oauthClients = List(
      OAuthClient(
        "skakapo",
        "skakapo-test-secret-0123456789",
        kakapoCb,
        "skakapo",
        List(TokenScope.Read, TokenScope.Write)
      ),
      OAuthClient("reader", "reader-test-secret-0123456789", "http://localhost:5200/cb", "reader")
    )
  )

  // ---------------------------------------------------------------- 偽の skaka（Web 会議）

  private val skakaUrl = "http://skaka.test"

  /** 偽の skaka が受けた要求（"POST /api/rooms {...}" など）と、発行したトークンが有効か. */
  private val skakaCalls = new java.util.concurrent.ConcurrentLinkedQueue[String]()
  private val skakaValid = new java.util.concurrent.atomic.AtomicBoolean(true)
  private val roomSeq = new java.util.concurrent.atomic.AtomicInteger(0)

  private val fakeSkaka: HttpRoutes[IO] = {
    import org.http4s.dsl.io.*
    val basicOk = (r: Request[IO]) =>
      r.headers.get[org.http4s.headers.Authorization].exists(_.credentials ==
        org.http4s.headers.Authorization(BasicCredentials("skuaka", "skuaka-test-secret-0123456789")).credentials)
    val bearerOk = (r: Request[IO]) =>
      skakaValid.get && r.headers.get[org.http4s.headers.Authorization].exists(_.credentials ==
        Credentials.Token(AuthScheme.Bearer, "skaka_test"))
    HttpRoutes.of[IO] {
      case r @ POST -> Root / "oauth" / "token" if basicOk(r) =>
        r.as[UrlForm].flatMap { f =>
          skakaCalls.add(s"token ${f.getFirst("code").getOrElse("")} ${f.getFirst("redirect_uri").getOrElse("")}")
          if f.getFirst("code").contains("good-code") && f.getFirst("code_verifier").exists(_.length >= 43) then {
            skakaValid.set(true); Ok(Json.obj("access_token" -> "skaka_test".asJson, "token_type" -> "Bearer".asJson))
          } else BadRequest()
        }
      case r @ POST -> Root / "oauth" / "revoke" if basicOk(r) =>
        r.as[UrlForm].flatMap(f => IO(skakaCalls.add(s"revoke ${f.getFirst("token").getOrElse("")}")) *> Ok())
      case r @ POST -> Root / "api" / "rooms" =>
        if !bearerOk(r) then IO.pure(Response[IO](Status.Unauthorized))
        else
          r.as[Json].flatMap { j =>
            val code = s"room-${roomSeq.incrementAndGet()}"
            skakaCalls.add(s"POST /api/rooms ${j.noSpaces}")
            Created(Json.obj("code" -> code.asJson, "url" -> s"$skakaUrl/r/$code".asJson))
          }
      case r @ PUT -> Root / "api" / "rooms" / code / "event" =>
        if !bearerOk(r) then IO.pure(Response[IO](Status.Unauthorized))
        else r.as[Json].flatMap(j => IO(skakaCalls.add(s"PUT /api/rooms/$code/event ${j.noSpaces}")) *> Ok(Json.obj()))
    }
  }

  // ---------------------------------------------------------------- 偽の iCalendar の配信（購読のテスト）

  private val icsBody = new java.util.concurrent.atomic.AtomicReference[String]("")
  private val fakeIcs: HttpRoutes[IO] = {
    import org.http4s.dsl.io.*
    HttpRoutes.of[IO] {
      case GET -> Root / "cal.ics" =>
        IO.pure(TextResponse(Status.Ok, icsBody.get, MediaType.unsafeParse("text/calendar")))
      case GET -> Root / "moved.ics" =>
        IO.pure(Response[IO](Status.Found).putHeaders(Location(uri"http://ics.test/cal.ics")))
    }
  }

  /** IdP とアプリ（ルート）を用意する. DB はまっさらにする. */
  private val fixture = ResourceSuiteLocalFixture(
    "app",
    for {
      _ <- Resource.eval(IO.blocking {
        val c = java.sql.DriverManager.getConnection(dbCfg.url, dbCfg.user, dbCfg.password)
        try c.createStatement().execute("drop schema public cascade; create schema public")
        finally c.close()
      })
      _ <- Resource.eval(Database.migrate(dbCfg))
      xa <- Database.transactor(dbCfg)
      idp <- Resource.eval(DevIdp.create(issuer, "skuaka", "skuaka-test-secret-0123456789"))
      oidc <- Resource.eval(OidcClient.create(
        OidcConfig(issuer, "skuaka", "skuaka-test-secret-0123456789", "http://localhost:8095/auth/callback"),
        Client.fromHttpApp(idp.routes.orNotFound)
      ))
    } yield {
      val users = new UserStore(xa); val cals = new CalendarStore(xa, users); val events = new EventStore(xa)
      val sess = new SessionAuth(users, apiCfg)
      val skaka = new SkakaService(
        SkakaConfig(skakaUrl, skakaUrl, "skuaka", "skuaka-test-secret-0123456789"),
        Client.fromHttpApp(fakeSkaka.orNotFound),
        new IntegrationStore(xa),
        skuaka.auth.SecretBox("test-app-secret-0123456789")
      )
      val importer =
        new CalendarImporter(Client.fromHttpApp(fakeIcs.orNotFound), cals, events, users, UrlGuard.allowAll)
      val app = ApiRoutes(
        Some(oidc),
        users,
        cals,
        events,
        sess,
        apiCfg,
        Some(skaka),
        oauth = Some(new OAuthStore(xa)),
        importer = Some(importer)
      ).orNotFound
      (idp.routes.orNotFound, app, xa)
    }
  )

  override def munitFixtures = List(fixture)

  private def location(r: Response[IO]): Uri =
    r.headers.get[Location].map(_.uri).getOrElse(fail(s"no Location (${r.status})"))

  /** IdP でログインして、セッションの Cookie を受け取る. */
  private def login(name: String, email: String): IO[String] = {
    val (idp, app, _) = fixture()
    for {
      r1 <- app.run(Request[IO](Method.GET, uri"/auth/login?returnTo=/week"))
      _ = assertEquals(r1.status, Status.Found)
      auth = location(r1)
      _ = assertEquals(auth.path.renderString, "/authorize")
      _ = assertEquals(auth.params.get("code_challenge_method"), Some("S256"))
      form =
        UrlForm(List("client_id", "redirect_uri", "state", "nonce", "code_challenge").map(k => k -> auth.params(k)) ++
          List("name" -> name, "email" -> email)*)
      r2 <- idp.run(Request[IO](Method.POST, uri"/authorize").withEntity(form))
      cb = location(r2)
      _ = assertEquals(cb.path.renderString, "/auth/callback")
      r3 <- app.run(Request[IO](Method.GET, Uri(path = cb.path, query = cb.query)))
      _ = assertEquals(r3.status, Status.Found, "callback")
      _ = assertEquals(location(r3).renderString, "/week")
    } yield r3.cookies.find(_.name == "skuaka_session").map(_.content).getOrElse(fail("no session cookie"))
  }

  private def call(token: String, m: Method, path: String, body: Option[Json] = None): IO[Response[IO]] = {
    val (_, app, _) = fixture()
    val base = Request[IO](
      m,
      Uri.unsafeFromString(path)
    ).putHeaders(Cookie(RequestCookie("skuaka_session", token)), Header.Raw(CIString("X-Skuaka"), "1"))
    app.run(body.fold(base)(base.withEntity(_)))
  }

  private def json[A: io.circe.Decoder](r: Response[IO]): IO[A] =
    if r.status.isSuccess then r.as[A] else r.bodyText.compile.string.flatMap(t => IO(fail(s"${r.status}: $t")))

  test("OIDC でログインし、自分のカレンダーができる. 同じ state は2回使えない") {
    for {
      t <- login("山田 太郎", "yamada@example.com")
      me <- call(t, Method.GET, "/api/me").flatMap(json[MeDto])
      _ = assertEquals(me.name, "山田 太郎")
      cs <- call(t, Method.GET, "/api/calendars").flatMap(json[List[CalendarDto]])
      _ = assertEquals(cs.map(c => (c.name, c.role, c.primary)), List(("山田 太郎", CalRole.Owner, true)))
      // ログアウトすると、IdP の end_session_endpoint へ（id_token_hint 付き）
      out <- call(t, Method.POST, "/auth/logout").flatMap(json[Json])
      url = out.hcursor.get[String]("redirect").getOrElse("")
      _ = assert(url.startsWith(s"$issuer/logout?") && url.contains("id_token_hint="), url)
      gone <- call(t, Method.GET, "/api/me")
      _ = assertEquals(gone.status, Status.Unauthorized)
    } yield ()
  }

  test("不正な state・X-Skuaka のない更新・未ログインは拒否する") {
    val (_, app, _) = fixture()
    for {
      r1 <- app.run(Request[IO](Method.GET, uri"/auth/callback?code=x&state=unknown"))
      _ = assertEquals(r1.status, Status.BadRequest)
      t <- login("佐藤", "sato@example.com")
      r2 <- app.run(Request[IO](Method.POST, uri"/api/calendars").putHeaders(Cookie(RequestCookie("skuaka_session", t)))
        .withEntity(CalendarReq("x", "#112233", "").asJson))
      _ = assertEquals(r2.status, Status.Forbidden)
      r3 <- app.run(Request[IO](Method.GET, uri"/api/calendars"))
      _ = assertEquals(r3.status, Status.Unauthorized)
    } yield ()
  }

  test("戻り先は同じサイトのパスだけ. 別のサイトへ飛ばす書き方は / にする") {
    val ok = List("/", "/week", "/settings?tab=a&b=1", "/p/AbC-_9", "/day/2026-11-16#x", "/%E4%BA%88")
    ok.foreach(p => assertEquals(SafePath.of(Some(p)), p))
    val ng = List(
      "//evil.example",
      "/\\evil.example",
      "/\t/evil.example",
      "/\n/evil.example",
      "https://evil.example",
      "evil",
      "",
      " //x",
      "/a b",
      "/<script>",
      "/\"x"
    )
    ng.foreach(p => assertEquals(SafePath.of(Some(p)), "/", p))
    assertEquals(SafePath.of(None, "/settings"), "/settings")
    val (_, app, _) = fixture()
    // ログインの戻り先も同じ確認をする（不正なら / に戻る）
    app.run(Request[IO](Method.GET, Uri.unsafeFromString("/auth/login?returnTo=%2F%09%2Fevil.example"))).map(r =>
      assertEquals(r.status, Status.Found)
    )
  }

  test("応答にセキュリティのヘッダーが付く. 大きすぎる本文は 413 で断る") {
    for {
      t <- login("堀 十一", "hori@sec.example.com")
      r1 <- call(t, Method.GET, "/api/me")
      h = (k: String) => r1.headers.get(CIString(k)).map(_.head.value)
      _ = assertEquals(
        (h("X-Content-Type-Options"), h("X-Frame-Options"), h("Content-Security-Policy")),
        (Some("nosniff"), Some("DENY"), Some(SecurityHeaders.StrictCsp))
      )
      _ = assertEquals(h("Strict-Transport-Security"), None) // http で公開しているときは付けない
      r2 <-
        call(t, Method.POST, "/api/calendars", Some(Json.obj("name" -> ("x" * (ApiRoutes.MaxBody.toInt + 10)).asJson)))
      _ = assertEquals(r2.status, Status.PayloadTooLarge)
    } yield ()
  }

  test("IP ごとの回数の制限: ログイン・公開リンクだけ. 信じるプロキシからだけ X-Forwarded-For を使う. IPv6 は /64 でまとめる") {
    import com.comcast.ip4s.*
    def from(ip: String, path: String, xff: Option[String] = None): Request[IO] = {
      val conn = Request.Connection(
        SocketAddress(ip"127.0.0.1", port"8080"),
        SocketAddress(IpAddress.fromString(ip).get, port"50000"),
        false
      )
      val r = Request[IO](Method.GET, Uri.unsafeFromString(path)).withAttribute(Request.Keys.ConnectionInfo, conn)
      xff.fold(r)(v => r.putHeaders(Header.Raw(CIString("X-Forwarded-For"), v)))
    }
    val trusted = List("10.0.0.0/8", "192.0.2.7").flatMap(RateLimitConfig.proxy)
    assertEquals(
      sbirds.http.RateLimit.clientKey(from("10.1.2.3", "/", Some("6.6.6.6, 203.0.113.9")), trusted),
      Some("203.0.113.9")
    )
    assertEquals(
      sbirds.http.RateLimit.clientKey(from("10.1.2.3", "/", Some("203.0.113.9, 192.0.2.7")), trusted),
      Some("203.0.113.9")
    )
    assertEquals(
      sbirds.http.RateLimit.clientKey(from("198.51.100.1", "/", Some("6.6.6.6")), trusted),
      Some("198.51.100.1")
    ) // 直接来た人は偽れない
    assertEquals(sbirds.http.RateLimit.clientKey(from("2001:db8:1:2:3:4:5:6", "/"), trusted), Some("2001:db8:1:2::/64"))
    assertEquals(sbirds.http.RateLimit.clientKey(Request[IO](), trusted), None)
    var now = scala.concurrent.duration.FiniteDuration(10, "minutes")
    val ok = HttpRoutes.of[IO] { case _ => IO.pure(Response[IO](Status.Ok)) }
    val app = RateLimit(ok, RateLimitConfig(authPerMinute = 3, publicPerMinute = 1, trusted), IO(now)).orNotFound
    for {
      a <- (1 to 4).toList.traverse(_ => app.run(from("203.0.113.5", "/auth/login")))
      _ = assertEquals(a.map(_.status.code), List(200, 200, 200, 429))
      _ = assertEquals(a.last.headers.get(CIString("Retry-After")).map(_.head.value), Some("60"))
      b <- app.run(from("203.0.113.6", "/auth/login")) // 別の人は使える
      c <- app.run(from("203.0.113.5", "/api/me")) // ログインした後の API は数えない
      d <- (1 to 2).toList.traverse(_ => app.run(from("203.0.113.5", "/ics/x.ics")))
      _ = assertEquals((b.status.code, c.status.code, d.map(_.status.code)), (200, 200, List(200, 429)))
      _ = now = now + scala.concurrent.duration.FiniteDuration(1, "minute") // 次の1分になれば、また使える
      e <- app.run(from("203.0.113.5", "/auth/login"))
      _ = assertEquals(e.status.code, 200)
    } yield ()
  }

  test("繰り返しの予定: 展開・この予定だけ変更・これ以降を削除・招待と返事・共有") {
    for {
      a <- login("鈴木", "suzuki@example.com")
      b <- login("田中", "tanaka@example.com")
      bMe <- call(b, Method.GET, "/api/me").flatMap(json[MeDto])
      cal <- call(a, Method.GET, "/api/calendars").flatMap(json[List[CalendarDto]]).map(_.head)
      req = EventReq(
        cal.id,
        "定例",
        "",
        "会議室A",
        false,
        "2026-11-02T01:00:00Z",
        "2026-11-02T02:00:00Z",
        "Asia/Tokyo",
        None,
        Some(Recurrence(Recurrence.Weekly)),
        List(bMe.id),
        List(10, 1440, 10),
        None
      )
      ev <- call(a, Method.POST, "/api/events", Some(req.asJson)).flatMap(json[EventDto])
      // 通知は複数（重複を除いて近い順）. 6つ以上は断る
      _ = assertEquals(ev.reminders, List(10, 1440))
      many <- call(a, Method.POST, "/api/events", Some(req.copy(reminders = List(0, 5, 10, 30, 60, 1440)).asJson))
      _ = assertEquals(many.status, Status.BadRequest)
      occ <- call(
        a,
        Method.GET,
        "/api/events?from=2026-11-01&to=2026-12-01&tz=Asia/Tokyo"
      ).flatMap(json[List[OccurrenceDto]])
      _ = assertEquals(
        occ.map(_.start),
        List(
          "2026-11-02T01:00:00Z",
          "2026-11-09T01:00:00Z",
          "2026-11-16T01:00:00Z",
          "2026-11-23T01:00:00Z",
          "2026-11-30T01:00:00Z"
        )
      )
      // 3回目だけ1時間遅らせる
      third = occ(2)
      moved <- call(
        a,
        Method.PUT,
        s"/api/events/${ev.id}?scope=this&occurrence=${third.occurrence}",
        Some(req.copy(start = "2026-11-16T02:00:00Z", end = "2026-11-16T03:00:00Z").asJson)
      ).flatMap(json[EventDto])
      _ = assertEquals(moved.recurrence, None)
      // 4回目以降を消す
      _ <- call(a, Method.DELETE, s"/api/events/${ev.id}?scope=following&occurrence=${occ(3).occurrence}")
      occ2 <- call(
        a,
        Method.GET,
        "/api/events?from=2026-11-01&to=2026-12-01&tz=Asia/Tokyo"
      ).flatMap(json[List[OccurrenceDto]])
      _ = assertEquals(occ2.map(_.start), List("2026-11-02T01:00:00Z", "2026-11-09T01:00:00Z", "2026-11-16T02:00:00Z"))
      // 招待された田中さんにも見え、返事ができるが、変更はできない
      occB <- call(
        b,
        Method.GET,
        "/api/events?from=2026-11-01&to=2026-12-01&tz=Asia/Tokyo"
      ).flatMap(json[List[OccurrenceDto]])
      _ = assertEquals(occB.map(o => (o.invited, o.canEdit)).distinct, List((true, false)))
      rs <-
        call(b, Method.POST, s"/api/events/${ev.id}/rsvp", Some(RsvpReq(Rsvp.Accepted).asJson)).flatMap(json[EventDto])
      _ = assertEquals(rs.myResponse, Some(Rsvp.Accepted))
      no <- call(b, Method.PUT, s"/api/events/${ev.id}", Some(req.asJson))
      _ = assertEquals(no.status, Status.Forbidden)
      // 共有すると田中さんも変更できる. 古い version での変更は 409
      _ <- call(
        a,
        Method.POST,
        s"/api/calendars/${cal.id}/members",
        Some(MemberReq("tanaka@example.com", CalRole.Edit).asJson)
      ).flatMap(json[List[MemberDto]])
      cur <- call(b, Method.GET, s"/api/events/${ev.id}").flatMap(json[EventDto])
      ok <- call(
        b,
        Method.PUT,
        s"/api/events/${ev.id}",
        Some(req.copy(
          title = "定例（変更）",
          start = cur.start,
          end = cur.end,
          recurrence = cur.recurrence,
          version = Some(cur.version)
        ).asJson)
      ).flatMap(json[EventDto])
      _ = assertEquals(ok.title, "定例（変更）")
      stale <- call(a, Method.PUT, s"/api/events/${ev.id}", Some(req.copy(version = Some(cur.version)).asJson))
      _ = assertEquals(stale.status, Status.Conflict)
      // iCalendar の配信
      ics <- call(
        a,
        Method.POST,
        s"/api/calendars/${cal.id}/public",
        Some(PublicLinkReq(CalRole.View).asJson)
      ).flatMap(json[CalendarDto])
      path = ics.publicLink.map(_.icsUrl.stripPrefix(apiCfg.publicUrl)).getOrElse(fail("no ics url"))
      text <- call("", Method.GET, path).flatMap(_.bodyText.compile.string)
      _ = assert(
        text.startsWith("BEGIN:VCALENDAR\r\n") && text.contains("SUMMARY:定例（変更）\r\n") &&
          text.contains("RRULE:FREQ=WEEKLY;UNTIL=20261122\r\n"),
        text
      )
    } yield ()
  }

  private def bearer(token: String, m: Method, path: String, body: Option[Json] = None): IO[Response[IO]] = {
    val (_, app, _) = fixture()
    val base = Request[IO](
      m,
      Uri.unsafeFromString(path)
    ).putHeaders(org.http4s.headers.Authorization(Credentials.Token(AuthScheme.Bearer, token)))
    app.run(body.fold(base)(base.withEntity(_)))
  }

  // for 式でタプルを分解するため
  extension [A](io: IO[A]) {
    private def withFilter(p: A => Boolean): IO[A] =
      io.flatMap(a => if p(a) then IO.pure(a) else IO.raiseError(new MatchError(a)))
  }

  /** MCP のツールを呼び、結果の文字列と isError を返す. */
  private def mcp(token: String, tool: String, args: Json): IO[(String, Boolean)] =
    bearer(
      token,
      Method.POST,
      "/mcp",
      Some(Json.obj(
        "jsonrpc" -> "2.0".asJson,
        "id" -> 1.asJson,
        "method" -> "tools/call".asJson,
        "params" -> Json.obj("name" -> tool.asJson, "arguments" -> args)
      ))
    ).flatMap(json[Json]).map { j =>
      val r = j.hcursor.downField("result")
      (
        r.downField("content").downArray.get[String]("text").getOrElse(j.noSpaces),
        r.get[Boolean]("isError").getOrElse(true)
      )
    }

  test("API トークン: Bearer で REST API を呼べる. 参照のみは変更できない. 管理は画面からだけ. 失効したら使えない") {
    for {
      a <- login("鈴木 一郎", "suzuki@example.com")
      rw <- call(
        a,
        Method.POST,
        "/api/tokens",
        Some(ApiTokenReq("スクリプト", TokenScope.Write, Some(30)).asJson)
      ).flatMap(json[ApiTokenCreated])
      ro <- call(
        a,
        Method.POST,
        "/api/tokens",
        Some(ApiTokenReq("参照", TokenScope.Read, None).asJson)
      ).flatMap(json[ApiTokenCreated])
      _ = assert(rw.token.startsWith("skuaka_") && rw.token.startsWith(rw.info.prefix))
      me <- bearer(rw.token, Method.GET, "/api/me").flatMap(json[MeDto])
      _ = assertEquals(me.name, "鈴木 一郎")
      cals <- bearer(ro.token, Method.GET, "/api/calendars").flatMap(json[List[CalendarDto]])
      cal = cals.find(_.primary).getOrElse(fail("no primary"))
      req = EventReq(
        cal.id,
        "API から",
        "",
        "",
        false,
        "2026-12-01T01:00:00Z",
        "2026-12-01T02:00:00Z",
        "Asia/Tokyo",
        None,
        None,
        Nil,
        Nil,
        None
      )
      // 書き込みのトークンは X-Skuaka なしで作れる
      made <- bearer(rw.token, Method.POST, "/api/events", Some(req.asJson))
      _ = assertEquals(made.status, Status.Created)
      // 参照のみは 403
      ro1 <- bearer(ro.token, Method.POST, "/api/events", Some(req.asJson))
      _ = assertEquals(ro1.status, Status.Forbidden)
      // トークンでトークンは作れない
      tk <- bearer(rw.token, Method.POST, "/api/tokens", Some(ApiTokenReq("x", TokenScope.Write, None).asJson))
      _ = assertEquals(tk.status, Status.Forbidden)
      list <- call(a, Method.GET, "/api/tokens").flatMap(json[List[ApiTokenDto]])
      _ = assertEquals(list.map(_.name).toSet, Set("スクリプト", "参照"))
      _ = assert(list.find(_.name == "スクリプト").flatMap(_.lastUsedAt).isDefined)
      // 失効・でたらめなトークンは 401（WWW-Authenticate 付き）
      _ <- call(a, Method.DELETE, s"/api/tokens/${rw.info.id}")
      gone <- bearer(rw.token, Method.GET, "/api/me")
      _ = assertEquals(gone.status, Status.Unauthorized)
      _ = assert(gone.headers.get[org.http4s.headers.`WWW-Authenticate`].isDefined)
      bad <- bearer("skuaka_nope", Method.GET, "/api/me")
      _ = assertEquals(bad.status, Status.Unauthorized)
    } yield ()
  }

  test("MCP: 初期化・ツールの一覧・予定の作成と一覧・繰り返しの1回の変更・空き時間・参照のみの制限") {
    for {
      a <- login("山本 三郎", "yamamoto@example.com")
      _ <- login("小林 四郎", "kobayashi@example.com")
      rw <- call(
        a,
        Method.POST,
        "/api/tokens",
        Some(ApiTokenReq("AI アシスタント", TokenScope.Write, None).asJson)
      ).flatMap(json[ApiTokenCreated]).map(_.token)
      ro <- call(
        a,
        Method.POST,
        "/api/tokens",
        Some(ApiTokenReq("AI アシスタント（参照）", TokenScope.Read, None).asJson)
      ).flatMap(json[ApiTokenCreated]).map(_.token)
      none <- fixture()._2.run(Request[IO](Method.POST, uri"/mcp").withEntity(Json.obj()))
      _ = assertEquals(none.status, Status.Unauthorized)
      init <- bearer(
        rw,
        Method.POST,
        "/mcp",
        Some(Json.obj(
          "jsonrpc" -> "2.0".asJson,
          "id" -> 1.asJson,
          "method" -> "initialize".asJson,
          "params" -> Json.obj("protocolVersion" -> "2025-06-18".asJson)
        ))
      ).flatMap(json[Json])
      _ = assertEquals(init.hcursor.downField("result").get[String]("protocolVersion"), Right("2025-06-18"))
      note <- bearer(
        rw,
        Method.POST,
        "/mcp",
        Some(Json.obj("jsonrpc" -> "2.0".asJson, "method" -> "notifications/initialized".asJson))
      )
      _ = assertEquals(note.status, Status.Accepted)
      tl <- bearer(
        ro,
        Method.POST,
        "/mcp",
        Some(Json.obj("jsonrpc" -> "2.0".asJson, "id" -> 2.asJson, "method" -> "tools/list".asJson))
      ).flatMap(json[Json])
      names = tl.hcursor.downField(
        "result"
      ).downField("tools").as[List[Json]].getOrElse(Nil).flatMap(_.hcursor.get[String]("name").toOption)
      _ = assert(names.contains("list_events") && !names.contains("create_event"), names)
      // 作成（自分のタイムゾーン Asia/Tokyo の 10:00 = UTC 01:00）. 毎週月曜、3回
      (c1, e1) <- mcp(
        rw,
        "create_event",
        Json.obj(
          "title" -> "週次MTG".asJson,
          "start" -> "2026-11-16T10:00".asJson,
          "end" -> "2026-11-16T11:00".asJson,
          "recurrence" -> "FREQ=WEEKLY;BYDAY=MO;COUNT=3".asJson,
          "attendees" -> List("kobayashi@example.com").asJson,
          "location" -> "会議室A".asJson
        )
      )
      _ = assert(!e1, c1)
      id = c1.linesIterator.collectFirst { case l if l.startsWith("eventId=") => l.stripPrefix("eventId=") }.getOrElse(
        fail(c1)
      )
      (l1, _) <- mcp(rw, "list_events", Json.obj("from" -> "2026-11-16".asJson, "to" -> "2026-12-06".asJson))
      _ = assert(l1.contains("3件") && l1.contains("2026/11/16(月) 10:00–11:00 週次MTG @会議室A") && l1.contains("ゲスト1人"), l1)
      // 2回目だけ 14:00 へ
      (u1, ue) <- mcp(
        rw,
        "update_event",
        Json.obj(
          "eventId" -> id.asJson,
          "scope" -> "this".asJson,
          "occurrence" -> "2026-11-23T01:00:00Z".asJson,
          "start" -> "2026-11-23T14:00".asJson
        )
      )
      _ = assert(!ue, u1)
      (l2, _) <- mcp(rw, "list_events", Json.obj("from" -> "2026-11-23".asJson, "to" -> "2026-11-23".asJson))
      _ = assert(l2.contains("2026/11/23(月) 14:00–15:00 週次MTG"), l2)
      // 空き時間（9:00〜12:00 で 60分. 過ぎた時間は空きにしないので、今日に左右されない先の日で確かめる）
      (c2, e2) <- mcp(
        rw,
        "create_event",
        Json.obj("title" -> "先の予定".asJson, "start" -> "2099-11-16T10:00".asJson, "end" -> "2099-11-16T11:00".asJson)
      )
      _ = assert(!e2, c2)
      (f1, _) <- mcp(
        rw,
        "find_free_time",
        Json.obj(
          "from" -> "2099-11-16".asJson,
          "to" -> "2099-11-16".asJson,
          "durationMinutes" -> 60.asJson,
          "dayStart" -> "09:00".asJson,
          "dayEnd" -> "12:00".asJson,
          "includeWeekends" -> true.asJson
        )
      )
      _ = assert(f1.contains("09:00–10:00") && f1.contains("11:00–12:00"), f1)
      // 参照のみのトークンで変更すると isError
      (r1, re) <- mcp(ro, "delete_event", Json.obj("eventId" -> id.asJson))
      _ = assert(re && r1.contains("参照のみ"), r1)
      (g1, _) <- mcp(ro, "get_event", Json.obj("eventId" -> id.asJson))
      _ = assert(g1.contains("繰り返し: ") && g1.contains("小林 四郎") && g1.contains("通知: 10分前"), g1)
      (x1, xe) <- mcp(rw, "call_api", Json.obj("method" -> "GET".asJson, "path" -> "/api/me".asJson))
      _ = assert(!xe && x1.contains("山本 三郎"), x1)
    } yield ()
  }

  test("チームへの公開: 探して加える. 予定あり・なしのみは内容を伏せ、探しても出ない. 公開をやめると見えなくなる") {
    val dec = "/api/events?from=2026-12-01&to=2026-12-02&tz=Asia/Tokyo"
    for {
      a <- login("石井 五郎", "ishii@team.example.com")
      b <- login("森 六子", "mori@team.example.com")
      cal <- call(a, Method.GET, "/api/calendars").flatMap(json[List[CalendarDto]]).map(_.head)
      _ = assertEquals((cal.primary, cal.teamAccess), (true, TeamAccess.Busy))
      _ <- call(
        a,
        Method.POST,
        "/api/events",
        Some(EventReq(
          cal.id,
          "人事面談",
          "評価の話",
          "会議室B",
          false,
          "2026-12-01T01:00:00Z",
          "2026-12-01T02:00:00Z",
          "Asia/Tokyo",
          None,
          Some(Recurrence(Recurrence.Weekly)),
          Nil,
          Nil,
          None
        ).asJson)
      ).flatMap(json[EventDto])
      found <- call(b, Method.GET, "/api/team/calendars?q=ishii").flatMap(json[List[TeamCalendarDto]])
      _ = assertEquals(found.map(c => (c.id, c.access, c.added)), List((cal.id, TeamAccess.Busy, false)))
      added <- call(b, Method.POST, s"/api/team/calendars/${cal.id}").flatMap(json[CalendarDto])
      _ = assertEquals((added.role, added.viaTeam), (CalRole.Busy, true))
      // 自分のカレンダーと同じ色にならない
      _ = assertNotEquals(added.displayColor, Palette.Default)
      // 予定あり・なしのみ: 時間だけ見え、題名・場所・説明は伏せる. 題名で探しても出ない. 共有の相手は見えない
      os <- call(b, Method.GET, dec).flatMap(json[List[OccurrenceDto]])
      _ = assertEquals(
        os.map(o => (o.title, o.location, o.busy, o.canEdit, o.start)),
        List((TeamAccess.BusyTitle, "", true, false, "2026-12-01T01:00:00Z"))
      )
      ev <- call(b, Method.GET, s"/api/events/${os.head.eventId}").flatMap(json[EventDto])
      _ = assertEquals(
        (ev.title, ev.description, ev.recurrence, ev.attendees, ev.busy),
        (TeamAccess.BusyTitle, "", None, Nil, true)
      )
      sr <- call(b, Method.GET, "/api/events/search?q=%E9%9D%A2%E8%AB%87").flatMap(json[List[OccurrenceDto]])
      _ = assertEquals(sr, Nil)
      ms <- call(b, Method.GET, s"/api/calendars/${cal.id}/members")
      _ = assertEquals(ms.status, Status.Forbidden)
      // MCP の空き時間: 自分だけなら空いているが、石井さんと一緒なら 10:00〜11:00 が埋まっている
      tk <- call(
        b,
        Method.POST,
        "/api/tokens",
        Some(ApiTokenReq("mcp", TokenScope.Read, None).asJson)
      ).flatMap(json[ApiTokenCreated])
      free = (people: List[String]) =>
        mcp(
          tk.token,
          "find_free_time",
          Json.obj(
            "from" -> "2026-12-01".asJson,
            "to" -> "2026-12-01".asJson,
            "durationMinutes" -> 60.asJson,
            "dayStart" -> "09:00".asJson,
            "dayEnd" -> "12:00".asJson,
            "people" -> people.asJson
          )
        )
      (f1, _) <- free(Nil)
      _ = assert(f1.contains("09:00–12:00"), f1)
      (f2, _) <- free(List("ishii@team.example.com"))
      _ = assert(f2.contains("09:00–10:00") && f2.contains("11:00–12:00") && !f2.contains("09:00–12:00"), f2)
      // 詳細を公開すると内容が見える
      _ <- call(
        a,
        Method.PUT,
        s"/api/calendars/${cal.id}",
        Some(CalendarReq(cal.name, cal.color, "", Some(TeamAccess.View)).asJson)
      ).flatMap(json[CalendarDto])
      os2 <- call(b, Method.GET, dec).flatMap(json[List[OccurrenceDto]])
      _ = assertEquals(os2.map(o => (o.title, o.location, o.busy)), List(("人事面談", "会議室B", false)))
      // 公開をやめると一覧からも予定からも消える
      _ <- call(
        a,
        Method.PUT,
        s"/api/calendars/${cal.id}",
        Some(CalendarReq(cal.name, cal.color, "", Some(TeamAccess.None)).asJson)
      ).flatMap(json[CalendarDto])
      cs <- call(b, Method.GET, "/api/calendars").flatMap(json[List[CalendarDto]])
      _ = assert(!cs.exists(_.id == cal.id), cs)
      os3 <- call(b, Method.GET, dec).flatMap(json[List[OccurrenceDto]])
      _ = assertEquals(os3, Nil)
      gone <- call(b, Method.GET, "/api/team/calendars?q=ishii").flatMap(json[List[TeamCalendarDto]])
      _ = assertEquals(gone, Nil)
      re <- call(b, Method.POST, s"/api/team/calendars/${cal.id}")
      _ = assertEquals(re.status, Status.NotFound)
    } yield ()
  }

  test("公開リンク: ログインしない人が閲覧できる. 予定あり・なしのみは伏せる. ゲストは出さない. 作り直し・停止で前のリンクは使えない") {
    val dec = "/events?from=2026-12-01&to=2026-12-02&tz=Asia/Tokyo"
    for {
      a <- login("村上 七海", "murakami@public.example.com")
      b <- login("井上 八郎", "inoue@public.example.com")
      bMe <- call(b, Method.GET, "/api/me").flatMap(json[MeDto])
      cal <- call(a, Method.GET, "/api/calendars").flatMap(json[List[CalendarDto]]).map(_.head)
      ev <- call(
        a,
        Method.POST,
        "/api/events",
        Some(EventReq(
          cal.id,
          "採用面接",
          "候補者A",
          "応接室",
          false,
          "2026-12-01T05:00:00Z",
          "2026-12-01T06:00:00Z",
          "Asia/Tokyo",
          None,
          None,
          List(bMe.id),
          Nil,
          None
        ).asJson)
      ).flatMap(json[EventDto])
      // 所有者でなければ公開できない
      no <- call(b, Method.POST, s"/api/calendars/${cal.id}/public", Some(PublicLinkReq(CalRole.Busy).asJson))
      _ = assertEquals(no.status, Status.NotFound)
      pub <- call(
        a,
        Method.POST,
        s"/api/calendars/${cal.id}/public",
        Some(PublicLinkReq(CalRole.Busy).asJson)
      ).flatMap(json[CalendarDto])
      ln = pub.publicLink.getOrElse(fail("no link"))
      token = ln.pageUrl.split("/p/").last
      _ = assertEquals((ln.access, ln.icsUrl.endsWith(s"/ics/$token.ics")), (CalRole.Busy, true))
      // ログインせずに（Cookie なしで）見る. 予定あり・なしのみは伏せる
      pc <- call("", Method.GET, s"/api/public/$token").flatMap(json[PublicCalendarDto])
      _ = assertEquals((pc.name, pc.owner.name, pc.owner.email, pc.access), (cal.name, "村上 七海", None, CalRole.Busy))
      os <- call("", Method.GET, s"/api/public/$token$dec").flatMap(json[List[OccurrenceDto]])
      _ = assertEquals(
        os.map(o => (o.title, o.location, o.busy, o.canEdit, o.guests)),
        List((TeamAccess.BusyTitle, "", true, false, 0))
      )
      ics1 <- call("", Method.GET, s"/ics/$token.ics").flatMap(_.bodyText.compile.string)
      _ = assert(
        ics1.contains(s"SUMMARY:${TeamAccess.BusyTitle}\r\n") && !ics1.contains("採用面接") && !ics1.contains("応接室"),
        ics1
      )
      // 詳細を公開: 題名・場所は見えるが、ゲストは出さない
      _ <- call(
        a,
        Method.POST,
        s"/api/calendars/${cal.id}/public",
        Some(PublicLinkReq(CalRole.View).asJson)
      ).flatMap(json[CalendarDto])
      os2 <- call("", Method.GET, s"/api/public/$token$dec").flatMap(json[List[OccurrenceDto]])
      _ = assertEquals(os2.map(o => (o.title, o.location, o.busy, o.guests)), List(("採用面接", "応接室", false, 0)))
      d <- call("", Method.GET, s"/api/public/$token/events/${ev.id}").flatMap(json[EventDto])
      _ = assertEquals((d.title, d.attendees, d.canEdit, d.organizer.email), ("採用面接", Nil, false, None))
      // ほかのカレンダーの予定は、このリンクでは見えない
      other <- call(b, Method.GET, "/api/calendars").flatMap(json[List[CalendarDto]]).map(_.find(_.primary).get)
      oe <- call(
        b,
        Method.POST,
        "/api/events",
        Some(EventReq(
          other.id,
          "私用",
          "",
          "",
          false,
          "2026-12-01T07:00:00Z",
          "2026-12-01T08:00:00Z",
          "Asia/Tokyo",
          None,
          None,
          Nil,
          Nil,
          None
        ).asJson)
      ).flatMap(json[EventDto])
      x <- call("", Method.GET, s"/api/public/$token/events/${oe.id}")
      _ = assertEquals(x.status, Status.NotFound)
      // 作り直すと前のリンクは使えない. やめるとどれも使えない
      re <- call(
        a,
        Method.POST,
        s"/api/calendars/${cal.id}/public",
        Some(PublicLinkReq(CalRole.View, regenerate = true).asJson)
      ).flatMap(json[CalendarDto])
      token2 = re.publicLink.map(_.pageUrl.split("/p/").last).getOrElse(fail("no link"))
      _ = assertNotEquals(token2, token)
      old <- call("", Method.GET, s"/api/public/$token")
      _ = assertEquals(old.status, Status.NotFound)
      st <- call(a, Method.DELETE, s"/api/calendars/${cal.id}/public").flatMap(json[CalendarDto])
      _ = assertEquals(st.publicLink, None)
      gone <- call("", Method.GET, s"/api/public/$token2$dec")
      _ = assertEquals(gone.status, Status.NotFound)
      icsG <- call("", Method.GET, s"/ics/$token2.ics")
      _ = assertEquals(icsG.status, Status.NotFound)
    } yield ()
  }

  /** 本文を画像などのバイト列で送る. */
  private def upload(token: String, path: String, data: Array[Byte]): IO[Response[IO]] = {
    val (_, app, _) = fixture()
    app.run(Request[IO](Method.PUT, Uri.unsafeFromString(path)).withBodyStream(fs2.Stream.emits(data))
      .putHeaders(Cookie(RequestCookie("skuaka_session", token)), Header.Raw(CIString("X-Skuaka"), "1")))
  }

  test("アイコン: 画像を保存すると URL に版が付き、ほかの人からも見える. 画像でないもの・大きすぎるものは断る. 消すと元に戻る") {
    val png = Array(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a).map(_.toByte) ++ Array.fill[Byte](100)(7)
    for {
      a <- login("岡田 九子", "okada@avatar.example.com")
      b <- login("西村 十郎", "nishimura@avatar.example.com")
      me0 <- call(a, Method.GET, "/api/me").flatMap(json[MeDto])
      _ = assertEquals((me0.avatarUploaded, me0.avatarUrl), (false, None))
      me1 <- upload(a, "/api/me/avatar", png).flatMap(json[MeDto])
      _ = assertEquals((me1.avatarUploaded, me1.avatarUrl), (true, Some(s"/api/users/${me0.id}/avatar?v=1")))
      me2 <- upload(a, "/api/me/avatar", png).flatMap(json[MeDto])
      _ = assertEquals(me2.avatarUrl, Some(s"/api/users/${me0.id}/avatar?v=2"))
      // ほかの人から: 検索の結果に URL が出て、画像を読める
      found <- call(b, Method.GET, "/api/users?q=okada").flatMap(json[List[UserBrief]])
      _ = assertEquals(found.map(_.avatarUrl), List(me2.avatarUrl))
      img <- call(b, Method.GET, s"/api/users/${me0.id}/avatar")
      bytes <- img.body.compile.to(Array)
      _ = assertEquals(
        (img.status, img.contentType.map(c => s"${c.mediaType.mainType}/${c.mediaType.subType}"), bytes.toList),
        (Status.Ok, Some("image/png"), png.toList)
      )
      _ = assert(img.headers.get(CIString("X-Content-Type-Options")).isDefined)
      // ログインしていなければ読めない
      anon <- call("", Method.GET, s"/api/users/${me0.id}/avatar")
      _ = assertEquals(anon.status, Status.Unauthorized)
      // SVG（スクリプトを含められる）・画像でないもの・大きすぎるものは断る
      svg <- upload(
        a,
        "/api/me/avatar",
        """<svg xmlns="http://www.w3.org/2000/svg"><script>alert(1)</script></svg>""".getBytes("UTF-8")
      )
      _ = assertEquals(svg.status, Status.UnsupportedMediaType)
      big <- upload(a, "/api/me/avatar", png ++ Array.fill[Byte](AvatarImage.MaxBytes)(0))
      _ = assertEquals(big.status, Status.PayloadTooLarge)
      // 消すと元に戻る（画像も読めなくなる）
      me3 <- call(a, Method.DELETE, "/api/me/avatar").flatMap(json[MeDto])
      _ = assertEquals((me3.avatarUploaded, me3.avatarUrl), (false, None))
      gone <- call(b, Method.GET, s"/api/users/${me0.id}/avatar")
      _ = assertEquals(gone.status, Status.NotFound)
    } yield ()
  }

  /** skaka との連携: 許可の画面へ移り、戻ってきたらトークンを保存する. */
  private def connectSkaka(a: String): IO[Unit] =
    for {
      r1 <- call(a, Method.GET, "/api/integrations/skaka/connect?returnTo=/settings")
      _ = assertEquals(r1.status, Status.Found)
      auth = location(r1)
      _ = assertEquals(
        (
          auth.renderString.startsWith(s"$skakaUrl/oauth/authorize?"),
          auth.params.get("client_id"),
          auth.params.get("scope"),
          auth.params.get("code_challenge_method"),
          auth.params.get("redirect_uri")
        ),
        (
          true,
          Some("skuaka"),
          Some("read write"),
          Some("S256"),
          Some(s"${apiCfg.publicUrl}/api/integrations/skaka/callback")
        )
      )
      st = auth.params("state")
      r2 <- call(a, Method.GET, s"/api/integrations/skaka/callback?code=good-code&state=$st")
      _ = assertEquals(location(r2).renderString, "/settings?integration=skaka&result=connected")
      // 同じ state は2回使えない
      r3 <- call(a, Method.GET, s"/api/integrations/skaka/callback?code=good-code&state=$st")
      _ = assertEquals(location(r3).renderString, "/settings?integration=skaka&result=error")
    } yield ()

  test("Web 会議（skaka）: 連携すると予定に会議室を付けられる. 題名・日時を変えると会議室にも伝える. 失効したら連携が切れる. やめると失効させる") {
    val (_, _, xa) = fixture()
    def status(t: String) = call(t, Method.GET, "/api/integrations/skaka").flatMap(json[SkakaStatusDto])
    def ev(cal: UUID, title: String, loc: String, meeting: Boolean) =
      EventReq(
        cal,
        title,
        "",
        loc,
        false,
        "2026-12-03T01:00:00Z",
        "2026-12-03T02:00:00Z",
        "Asia/Tokyo",
        None,
        None,
        Nil,
        Nil,
        None,
        Option.when(meeting)(true)
      )
    def calls = { val xs = skakaCalls.toArray.toList.map(_.toString); skakaCalls.clear(); xs }
    for {
      a <- login("小川 十一", "ogawa@skaka.example.com")
      cfgD <- call(a, Method.GET, "/api/client-config").flatMap(json[ClientConfigDto])
      _ = assertEquals(cfgD.skakaUrl, Some(skakaUrl))
      s0 <- status(a)
      _ = assertEquals(s0, SkakaStatusDto(true, false, Some(skakaUrl)))
      cal <- call(a, Method.GET, "/api/calendars").flatMap(json[List[CalendarDto]]).map(_.head)
      // 連携していなければ、予定は保存して理由を返す
      e0 <- call(a, Method.POST, "/api/events", Some(ev(cal.id, "未連携", "", true).asJson)).flatMap(json[EventDto])
      _ = assert(e0.meetingError.exists(_.contains("連携していません")) && e0.location == "", e0)
      _ = calls
      _ <- connectSkaka(a)
      _ = assertEquals(calls, List(s"token good-code ${apiCfg.publicUrl}/api/integrations/skaka/callback"))
      s1 <- status(a)
      _ = assert(s1.connected)
      // トークンは暗号化して保存する
      enc <- sql"select token_enc from integration_token".query[String].to[List].transact(xa)
      _ = assert(enc.nonEmpty && enc.forall(e => e.startsWith("v1:") && !e.contains("skaka_test")), enc)
      // 会議室を付ける（場所に URL を足す）
      e1 <- call(a, Method.POST, "/api/events", Some(ev(cal.id, "設計レビュー", "会議室A", true).asJson)).flatMap(json[EventDto])
      url = s"$skakaUrl/r/room-1"
      _ = assertEquals((e1.location, e1.meetingError), (s"会議室A / $url", None))
      c1 = calls
      _ = assert(
        c1.size == 1 && c1.head.startsWith("POST /api/rooms ") && c1.head.contains(s""""eventId":"${e1.id}"""") &&
          c1.head.contains(""""name":"設計レビュー"""") && c1.head.contains(""""start":"2026-12-03T01:00:00Z""""),
        c1
      )
      // 題名を変えると会議室の予定の写しも変える. 説明だけなら伝えない. 付け済みなら作り直さない
      e2 <- call(
        a,
        Method.PUT,
        s"/api/events/${e1.id}",
        Some(ev(cal.id, "設計レビュー（2回目）", e1.location, true).copy(version = Some(e1.version)).asJson)
      )
        .flatMap(json[EventDto])
      c2 = calls
      _ = assert(
        c2.size == 1 && c2.head.startsWith("PUT /api/rooms/room-1/event ") && c2.head.contains("設計レビュー（2回目）"),
        c2
      )
      _ <- call(
        a,
        Method.PUT,
        s"/api/events/${e1.id}",
        Some(ev(cal.id, "設計レビュー（2回目）", e1.location, false)
          .copy(description = "資料あり", version = Some(e2.version)).asJson)
      ).flatMap(json[EventDto])
      _ = assertEquals(calls, Nil)
      // skaka でトークンが失効したら、連携が切れたと扱う
      _ = skakaValid.set(false)
      e3 <- call(a, Method.POST, "/api/events", Some(ev(cal.id, "失効後", "", true).asJson)).flatMap(json[EventDto])
      _ = assert(e3.meetingError.exists(_.contains("連携が切れました")), e3)
      s2 <- status(a)
      _ = assert(!s2.connected)
      // 連携し直してからやめると、skaka のトークンも失効させる
      _ <- connectSkaka(a)
      _ = calls
      del <- call(a, Method.DELETE, "/api/integrations/skaka/connection")
      _ = assertEquals(del.status, Status.NoContent)
      _ = assertEquals(calls, List("revoke skaka_test"))
      s3 <- status(a)
      _ = assert(!s3.connected)
    } yield ()
  }

  // ---------------------------------------------------------------- 兄弟のサービスへの認可（OAuth）

  private val verifier = "v" * 50
  private val challenge = sbirds.auth.Crypto.pkceChallenge(verifier)

  private def authorizeUri(client: String = "skakapo",
                           redirect: String = kakapoCb,
                           scope: String = "read write",
                           method: String = "S256"): String = {
    def e(v: String) = java.net.URLEncoder.encode(v, "UTF-8")
    s"/oauth/authorize?response_type=code&client_id=${e(client)}&redirect_uri=${e(redirect)}&scope=${e(scope)}&state=st1" +
      s"&code_challenge=$challenge&code_challenge_method=$method"
  }

  private def tokenReq(form: (String, String)*)(client: String = "skakapo",
                                                secret: String = "skakapo-test-secret-0123456789"): IO[Response[IO]] =
    fixture()._2.run(Request[IO](Method.POST, uri"/oauth/token").withEntity(UrlForm(form*))
      .putHeaders(org.http4s.headers.Authorization(BasicCredentials(client, secret))))

  /** ログインしたユーザーが許可して、トークンの応答（JSON）を受け取る. */
  private def grant(session: String, scope: String): IO[Json] = {
    val app = fixture()._2
    for {
      pg <- app.run(Request[IO](
        Method.GET,
        Uri.unsafeFromString(authorizeUri(scope = scope))
      ).putHeaders(Cookie(RequestCookie("skuaka_session", session))))
      _ = assertEquals(pg.status, Status.Ok)
      html <- pg.bodyText.compile.string
      _ = assert(html.contains("skakapo") && html.contains("<code>read</code>"), html)
      _ = assertEquals(html.contains("<code>write</code>"), scope.contains("write"))
      csrf = pg.cookies.find(_.name == "skuaka_oauth_csrf").map(_.content).getOrElse(fail("no csrf cookie"))
      form = UrlForm(List(
        "response_type" -> "code",
        "client_id" -> "skakapo",
        "redirect_uri" -> kakapoCb,
        "scope" -> scope,
        "state" -> "st1",
        "code_challenge" -> challenge,
        "code_challenge_method" -> "S256",
        "csrf" -> csrf,
        "decision" -> "allow"
      )*)
      // CSRF の値が合わなければ許可しない
      bad <- app.run(Request[IO](Method.POST, uri"/oauth/authorize").withEntity(form)
        .putHeaders(Cookie(RequestCookie("skuaka_session", session), RequestCookie("skuaka_oauth_csrf", "nope"))))
      _ = assertEquals(bad.status, Status.BadRequest)
      ok <- app.run(Request[IO](Method.POST, uri"/oauth/authorize").withEntity(form)
        .putHeaders(Cookie(RequestCookie("skuaka_session", session), RequestCookie("skuaka_oauth_csrf", csrf))))
      _ = assertEquals(ok.status, Status.Found)
      cb = location(ok)
      _ = assert(cb.renderString.startsWith(kakapoCb + "?"), cb.renderString)
      _ = assertEquals(cb.params.get("state"), Some("st1"))
      code = cb.params.getOrElse("code", fail("no code"))
      // 違う verifier・違う redirect_uri は invalid_grant（コードはそこで使用済みになる）
      wrong <- tokenReq(
        "grant_type" -> "authorization_code",
        "code" -> code,
        "redirect_uri" -> kakapoCb,
        "code_verifier" -> ("w" * 50)
      )()
      _ = assertEquals(wrong.status, Status.BadRequest)
      pg2 <- app.run(Request[IO](
        Method.GET,
        Uri.unsafeFromString(authorizeUri(scope = scope))
      ).putHeaders(Cookie(RequestCookie("skuaka_session", session))))
      csrf2 = pg2.cookies.find(_.name == "skuaka_oauth_csrf").map(_.content).getOrElse(fail("no csrf cookie"))
      ok2 <- app.run(Request[IO](
        Method.POST,
        uri"/oauth/authorize"
      ).withEntity(UrlForm(form.values.updated("csrf", cats.data.Chain(csrf2))))
        .putHeaders(Cookie(RequestCookie("skuaka_session", session), RequestCookie("skuaka_oauth_csrf", csrf2))))
      code2 = location(ok2).params.getOrElse("code", fail("no code"))
      badc <- tokenReq(
        "grant_type" -> "authorization_code",
        "code" -> code2,
        "redirect_uri" -> kakapoCb,
        "code_verifier" -> verifier
      )(secret = "wrong-secret-0123456789")
      _ = assertEquals(badc.status, Status.Unauthorized)
      tok <- tokenReq(
        "grant_type" -> "authorization_code",
        "code" -> code2,
        "redirect_uri" -> kakapoCb,
        "code_verifier" -> verifier
      )().flatMap(json[Json])
      // 同じコードは2回使えない
      again <- tokenReq(
        "grant_type" -> "authorization_code",
        "code" -> code2,
        "redirect_uri" -> kakapoCb,
        "code_verifier" -> verifier
      )()
      _ = assertEquals(again.status, Status.BadRequest)
    } yield tok
  }

  test("OAuth: 未ログインはログインへ 302. 知らない client・違う redirect_uri は 400. 許していないスコープは invalid_scope. PKCE は S256 だけ") {
    val app = fixture()._2
    def get(u: String) = app.run(Request[IO](Method.GET, Uri.unsafeFromString(u)))
    for {
      r1 <- get(authorizeUri())
      _ = assertEquals(r1.status, Status.Found)
      l1 = location(r1)
      _ = assertEquals(l1.path.renderString, "/auth/login")
      // ログインの後は、同じ認可の要求に戻る
      _ = assert(l1.params.get("returnTo").exists(_.startsWith("/oauth/authorize?")), l1.renderString)
      _ = assertEquals(SafePath.of(l1.params.get("returnTo")), l1.params("returnTo"))
      r2 <- get(authorizeUri(client = "nobody"))
      _ = assertEquals(r2.status, Status.BadRequest)
      r3 <- get(authorizeUri(redirect = "https://evil.example.com/cb"))
      _ = assertEquals(r3.status, Status.BadRequest)
      r4 <- get(authorizeUri(client = "reader", redirect = "http://localhost:5200/cb", scope = "read write"))
      _ = assertEquals(r4.status, Status.Found)
      _ = assertEquals(location(r4).params.get("error"), Some("invalid_scope"))
      r5 <- get(authorizeUri(scope = "read admin"))
      _ = assertEquals(location(r5).params.get("error"), Some("invalid_scope"))
      r6 <- get(authorizeUri(method = "plain"))
      _ = assertEquals(location(r6).params.get("error"), Some("invalid_request"))
      r7 <- tokenReq(
        "grant_type" -> "authorization_code",
        "code" -> "x",
        "redirect_uri" -> kakapoCb,
        "code_verifier" -> verifier
      )(client = "nobody")
      _ = assertEquals(r7.status, Status.Unauthorized)
    } yield ()
  }

  test("OAuth: 許可 → コード → トークンで /mcp と REST API を使える. refresh token で更新すると前のものは使えない. 設定の画面から取り消せる") {
    for {
      a <- login("小林 五郎", "kobayashi@example.com")
      tok <- grant(a, "read write")
      at = tok.hcursor.get[String]("access_token").getOrElse(fail(tok.noSpaces))
      rt = tok.hcursor.get[String]("refresh_token").getOrElse(fail(tok.noSpaces))
      _ = assertEquals(tok.hcursor.get[String]("scope").toOption, Some("read write"))
      _ = assertEquals(tok.hcursor.get[Long]("expires_in").toOption, Some(3600L))
      tl <- bearer(
        at,
        Method.POST,
        "/mcp",
        Some(Json.obj("jsonrpc" -> "2.0".asJson, "id" -> 1.asJson, "method" -> "tools/list".asJson))
      ).flatMap(json[Json])
      names = tl.hcursor.downField(
        "result"
      ).downField("tools").as[List[Json]].getOrElse(Nil).flatMap(_.hcursor.get[String]("name").toOption)
      _ = assert(names.contains("create_event"), tl.noSpaces)
      (c1, e1) <- mcp(
        at,
        "create_event",
        Json.obj("title" -> "連携から".asJson, "start" -> "2026-12-03T10:00".asJson, "end" -> "2026-12-03T11:00".asJson)
      )
      _ = assert(!e1, c1)
      me <- bearer(at, Method.GET, "/api/me").flatMap(json[MeDto])
      _ = assertEquals(me.name, "小林 五郎")
      // 連携のトークンでトークンの管理はできない
      tk <- bearer(at, Method.GET, "/api/tokens")
      _ = assertEquals(tk.status, Status.Forbidden)
      // 更新: 新しいトークンが使え、前のトークンと refresh token は使えない
      nt <- tokenReq("grant_type" -> "refresh_token", "refresh_token" -> rt)().flatMap(json[Json])
      at2 = nt.hcursor.get[String]("access_token").getOrElse(fail(nt.noSpaces))
      rt2 = nt.hcursor.get[String]("refresh_token").getOrElse(fail(nt.noSpaces))
      _ = assertEquals(nt.hcursor.get[String]("scope").toOption, Some("read write"))
      old <- bearer(at, Method.GET, "/api/me")
      _ = assertEquals(old.status, Status.Unauthorized)
      reuse <- tokenReq("grant_type" -> "refresh_token", "refresh_token" -> rt)()
      _ = assertEquals(reuse.status, Status.BadRequest)
      // 別のクライアントは、ほかのクライアントの refresh token を使えない
      other <- tokenReq(
        "grant_type" -> "refresh_token",
        "refresh_token" -> rt2
      )(client = "reader", secret = "reader-test-secret-0123456789")
      _ = assertEquals(other.status, Status.BadRequest)
      ok2 <- bearer(at2, Method.GET, "/api/me")
      _ = assertEquals(ok2.status, Status.Ok)
      // 設定の画面: 連携として出て、取り消すと使えなくなる（refresh token も）
      list <- call(a, Method.GET, "/api/tokens").flatMap(json[List[ApiTokenDto]])
      link = list.find(_.oauthClient.contains("skakapo")).getOrElse(fail(list.toString))
      _ = assertEquals((link.name, link.scope, list.size), ("skakapo 連携", TokenScope.Write, 1))
      del <- call(a, Method.DELETE, s"/api/tokens/${link.id}")
      _ = assertEquals(del.status, Status.NoContent)
      gone <- bearer(at2, Method.GET, "/api/me")
      _ = assertEquals(gone.status, Status.Unauthorized)
      dead <- tokenReq("grant_type" -> "refresh_token", "refresh_token" -> rt2)()
      _ = assertEquals(dead.status, Status.BadRequest)
    } yield ()
  }

  test("OAuth: read だけのトークンでは書き込みを拒む（REST API も MCP も）. 許可し直すと前のトークンは失効する. revoke で失効させられる") {
    for {
      a <- login("中村 六子", "nakamura@example.com")
      tok <- grant(a, "read")
      at = tok.hcursor.get[String]("access_token").getOrElse(fail(tok.noSpaces))
      _ = assertEquals(tok.hcursor.get[String]("scope").toOption, Some("read"))
      cals <- bearer(at, Method.GET, "/api/calendars").flatMap(json[List[CalendarDto]])
      cal = cals.find(_.primary).getOrElse(fail("no primary"))
      req = EventReq(
        cal.id,
        "書けない",
        "",
        "",
        false,
        "2026-12-01T01:00:00Z",
        "2026-12-01T02:00:00Z",
        "Asia/Tokyo",
        None,
        None,
        Nil,
        Nil,
        None
      )
      w <- bearer(at, Method.POST, "/api/events", Some(req.asJson))
      _ = assertEquals(w.status, Status.Forbidden)
      (m, isErr) <- mcp(
        at,
        "create_event",
        Json.obj("title" -> "書けない".asJson, "start" -> "2026-12-03T10:00".asJson, "end" -> "2026-12-03T11:00".asJson)
      )
      _ = assert(isErr && m.contains("参照のみ"), m)
      (l, lErr) <- mcp(at, "list_calendars", Json.obj())
      _ = assert(!lErr, l)
      // 許可し直すと、前のトークンは失効する
      tok2 <- grant(a, "read write")
      at2 = tok2.hcursor.get[String]("access_token").getOrElse(fail(tok2.noSpaces))
      old <- bearer(at, Method.GET, "/api/me")
      _ = assertEquals(old.status, Status.Unauthorized)
      rv <- fixture()._2.run(Request[IO](Method.POST, uri"/oauth/revoke").withEntity(UrlForm("token" -> at2))
        .putHeaders(org.http4s.headers.Authorization(BasicCredentials("skakapo", "skakapo-test-secret-0123456789"))))
      _ = assertEquals(rv.status, Status.Ok)
      gone <- bearer(at2, Method.GET, "/api/me")
      _ = assertEquals(gone.status, Status.Unauthorized)
    } yield ()
  }

  // ---------------------------------------------------------------- 外部のカレンダーの取り込み・購読

  private def ics(events: (String, String, String)*): String =
    (List("BEGIN:VCALENDAR", "VERSION:2.0", "X-WR-CALNAME:外部の予定") ++ events.toList.flatMap((uid, title, day) =>
      List(
        "BEGIN:VEVENT",
        s"UID:$uid",
        s"DTSTART;TZID=Asia/Tokyo:${day}T100000",
        s"DTEND;TZID=Asia/Tokyo:${day}T110000",
        s"SUMMARY:$title",
        "END:VEVENT"
      )
    ) ++
      List("END:VCALENDAR")).mkString("\r\n")

  private def importIcs(token: String, cal: UUID, text: String): IO[Response[IO]] = {
    val (_, app, _) = fixture()
    app.run(Request[IO](Method.POST, Uri.unsafeFromString(s"/api/calendars/$cal/import"))
      .putHeaders(Cookie(RequestCookie("skuaka_session", token)), Header.Raw(CIString("X-Skuaka"), "1"))
      .withEntity(text)(using EntityEncoder.stringEncoder).withContentType(org.http4s.headers.`Content-Type`(
        MediaType.unsafeParse("text/calendar")
      )))
  }

  private def december(t: String): IO[List[OccurrenceDto]] =
    call(t, Method.GET, "/api/events?from=2026-12-01&to=2027-01-01&tz=Asia/Tokyo").flatMap(json[List[OccurrenceDto]])

  test("取り込み: iCalendar のファイルを取り込む. 取り込み直すと変わった予定だけを上書きし、重ならない") {
    for {
      t <- login("取込 一郎", "torikomi@example.com")
      cal <- call(t, Method.GET, "/api/calendars").flatMap(json[List[CalendarDto]]).map(_.find(_.primary).get)
      r1 <-
        importIcs(t, cal.id, ics(("i1", "歯医者", "20261205"), ("i2", "飲み会", "20261210"))).flatMap(json[ImportResultDto])
      _ = assertEquals((r1.created, r1.updated, r1.removed), (2, 0, 0))
      r2 <-
        importIcs(t, cal.id, ics(("i1", "歯医者", "20261205"), ("i2", "飲み会", "20261210"))).flatMap(json[ImportResultDto])
      _ = assertEquals((r2.created, r2.updated), (0, 0))
      r3 <- importIcs(t, cal.id, ics(("i1", "歯医者（変更）", "20261205"))).flatMap(json[ImportResultDto])
      _ = assertEquals((r3.created, r3.updated, r3.removed), (0, 1, 0))
      occ <- december(t)
      _ = assertEquals(
        occ.filter(_.calendarId == cal.id).map(o => (o.title, o.canEdit)).sorted,
        List(("歯医者（変更）", true), ("飲み会", true))
      )
      _ = assertEquals(occ.find(_.title == "飲み会").map(_.start), Some("2026-12-10T01:00:00Z"))
      bad <- importIcs(t, cal.id, "<html>not ics</html>")
      _ = assertEquals(bad.status, Status.BadRequest)
    } yield ()
  }

  test("購読: iCalendar の URL を購読すると、読み取りだけのカレンダーができる. 読み直すと元から消えた予定は消える") {
    icsBody.set(ics(("s1", "全社会議", "20261207"), ("s2", "研修", "20261214")))
    for {
      t <- login("購読 二郎", "koudoku@example.com")
      bad <- call(t, Method.POST, "/api/calendars/subscribe", Some(SubscribeReq("ftp://ics.test/cal.ics").asJson))
      _ = assertEquals(bad.status, Status.BadRequest)
      cal <- call(
        t,
        Method.POST,
        "/api/calendars/subscribe",
        Some(SubscribeReq("webcal://ics.test/moved.ics").asJson)
      ).flatMap(json[CalendarDto])
      _ = assertEquals(cal.name, "外部の予定")
      _ = assertEquals(cal.subscription.flatMap(_.url), Some("https://ics.test/moved.ics"))
      _ = assert(!cal.writable)
      occ <- december(t)
      mine = occ.filter(_.calendarId == cal.id)
      _ = assertEquals(mine.map(_.title).sorted, List("全社会議", "研修"))
      _ = assert(mine.forall(!_.canEdit), mine)
      // 読み取りだけ: 予定を入れられず、取り込んだ予定も変えられない. ファイルの取り込みもできない
      req = EventReq(
        cal.id,
        "入れられない",
        "",
        "",
        false,
        "2026-12-01T01:00:00Z",
        "2026-12-01T02:00:00Z",
        "Asia/Tokyo",
        None,
        None,
        Nil,
        Nil,
        None
      )
      add <- call(t, Method.POST, "/api/events", Some(req.asJson))
      _ = assertEquals(add.status, Status.Forbidden)
      upd <- call(t, Method.PUT, s"/api/events/${mine.head.eventId}", Some(req.copy(title = "変えられない").asJson))
      _ = assertEquals(upd.status, Status.Forbidden)
      imp <- importIcs(t, cal.id, ics(("x", "x", "20261201")))
      _ = assertEquals(imp.status, Status.BadRequest)
      // 元が変わったら読み直す
      _ = icsBody.set(ics(("s1", "全社会議（場所変更）", "20261207"), ("s3", "忘年会", "20261225")))
      re <- call(t, Method.POST, s"/api/calendars/${cal.id}/sync").flatMap(json[CalendarDto])
      _ = assertEquals(re.subscription.flatMap(_.error), None)
      occ2 <- december(t)
      _ = assertEquals(occ2.filter(_.calendarId == cal.id).map(_.title).sorted, List("全社会議（場所変更）", "忘年会"))
      // 読めなくなったら理由を返し、予定はそのまま
      _ = icsBody.set("not ics")
      ng <- call(t, Method.POST, s"/api/calendars/${cal.id}/sync")
      _ = assertEquals(ng.status, Status.BadGateway)
      cals <- call(t, Method.GET, "/api/calendars").flatMap(json[List[CalendarDto]])
      _ = assert(cals.find(_.id == cal.id).flatMap(_.subscription).flatMap(_.error).nonEmpty)
      occ3 <- december(t)
      _ = assertEquals(occ3.count(_.calendarId == cal.id), 2)
    } yield ()
  }

  test("購読: 内部のアドレス（localhost・社内のネットワーク・メタデータ）は読みに行かない") {
    def blocked(ip: String) = UrlGuard.blocked(java.net.InetAddress.getByName(ip))
    List(
      "127.0.0.1",
      "10.1.2.3",
      "172.16.0.1",
      "192.168.1.1",
      "169.254.169.254",
      "100.64.0.1",
      "0.0.0.0",
      "::1",
      "fd00::1",
      "fe80::1",
      "::ffff:10.0.0.1"
    )
      .foreach(ip => assert(blocked(ip), ip))
    List("8.8.8.8", "142.250.196.110", "2001:4860:4860::8888").foreach(ip => assert(!blocked(ip), ip))
    UrlGuard.publicOnly.check(java.net.URI.create("http://localhost:8095/x.ics")).map(r =>
      assertEquals(r, Left(UrlGuard.NotPublic))
    )
  }
}
