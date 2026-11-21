package skuaka.app

import sbirds.db.*
import cats.effect.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import doobie.implicits.*
import org.http4s.*
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import skuaka.api.*
import sbirds.auth.{OidcClient, OidcConfig}
import skuaka.db.*
import sbirds.observe.{HttpClientTelemetry, HttpTelemetry, Metrics, RumRoutes, Telemetry}
import sbirds.push.{PushService, PushStore}
import sbirds.http.{Errors, Health, SecurityHeaders, Spa}
import sbirds.net.NetGuard
import sbirds.links.{IntegrationTokenStore, LinkClient, LinkConfig, StuiClient}
import skuaka.core.ErrorCode

import scala.concurrent.duration.*

final case class AppConfig(port: Int,
                           db: DbConfig,
                           oidc: Option[OidcConfig],
                           api: ApiConfig,
                           staticDir: Option[String],
                           skaka: Option[LinkConfig],
                           appSecret: Option[String],
                           icsSyncMinutes: Int = 60,
                           icsAllowPrivate: Boolean = false,
                           stui: Option[LinkConfig] = None,
                           hookPolicy: NetGuard.Policy = NetGuard.Policy.publicOnly,
                           webhookAllowHttp: Boolean = false)

object Main extends IOApp.Simple {

  // ログにトレースの ID・操作したユーザーを載せるため、IOLocal の文脈を Java のライブラリから見えるようにする
  // （IORuntime を作る前に設定する必要がある. オブジェクトの初期化は main より先に動く）
  Telemetry.prepare()

  private val version =
    Option(getClass.getPackage.getImplementationVersion).orElse(sys.env.get("APP_VERSION")).getOrElse("dev")

  private given org.typelevel.log4cats.LoggerFactory[IO] = org.typelevel.log4cats.slf4j.Slf4jFactory.create[IO]

  def run: IO[Unit] =
    for {
      cfg <- Config.load
      n <- migrateWithRetry(cfg)
      _ <- sbirds.observe.Log.println(s"[skuaka] database migrated ($n migration(s) applied)")
      _ <- (for {
        _ <- Telemetry.resource("skuaka", version, runtime)
        xa <- Database.transactor(cfg.db)
        _ <- gauges(xa)
        // 外への呼び出し（IdP・skaka・プッシュのサービス）もトレースに出す（traceparent を付けて送る）
        http <- EmberClientBuilder.default[IO].withTimeout(30.seconds).build.map(HttpClientTelemetry(_))
        oidc <- Resource.eval(cfg.oidc.traverse(OidcClient.create(_, http)))
        users = new UserStore(xa)
        calendars = new CalendarStore(xa, users)
        events = new EventStore(xa)
        sess = new SessionAuth(users, cfg.api)
        // 兄弟のサービスとの連携（ユーザーごとのトークンは APP_SECRET の鍵で暗号化して保存する）
        tokens = new IntegrationTokenStore(xa)
        link = (c: LinkConfig) => cfg.appSecret.map(k => new LinkClient(c, http, tokens, skuaka.auth.SecretBox(k)))
        skaka = cfg.skaka.flatMap(link).map(new SkakaService(_))
        stui = cfg.stui.flatMap(link).map(new StuiClient(_))
        _ <- Resource.eval(skaka.traverse_(s => sbirds.observe.Log.println(s"[skuaka] skaka（Web 会議）: ${s.cfg.url}")))
        _ <- Resource.eval(stui.traverse_(s => sbirds.observe.Log.println(s"[skuaka] stui（チャット）: ${s.link.cfg.url}")))
        _ <- Resource.eval(cfg.api.oauthClients.traverse_(c =>
          sbirds.observe.Log.println(s"[skuaka] OAuth クライアント: ${c.id}（${c.scopes.mkString(" ")}）")
        ))
        _ <- Resource.eval(IO.whenA(
          oidc.isEmpty
        )(sbirds.observe.Log.println("[skuaka] WARNING: OIDC が未設定です。開発用のログイン（DEV_LOGIN）を使います。本番では使わないでください")))
        // OIDC の設定の誤り（issuer の URL など）は起動時に知らせる（IdP が止まっていても起動はする）
        _ <- Resource.eval(oidc.traverse_(_.discover.attempt.flatMap {
          case Right(m) => sbirds.observe.Log.println(s"[skuaka] OIDC: ${m.issuer}")
          case Left(e) =>
            sbirds.observe.Log.println(s"[skuaka] WARNING: OIDC の discovery に失敗しました（${e.getMessage}）。ログインのときにもう一度試します")
        }))
        // プッシュ通知（PWA）: 招待と、予定のリマインダー（何分前のお知らせ）
        pushStore = new PushStore(xa)
        push <- Resource.eval(PushService.create(pushStore, http, cfg.api.publicUrl, "skuaka"))
        // カレンダーの通知先（stui のチャンネル・stui の受信 Webhook・Webhook）. 送り先は確かめた IP にそのままつなぐ
        hookHttp <- NetGuard.client(cfg.hookPolicy, timeout = 20.seconds).map(HttpClientTelemetry(_))
        hooks = new HookService(
          new HookStore(xa),
          stui,
          hookHttp,
          cfg.hookPolicy,
          cfg.api.publicUrl,
          cfg.webhookAllowHttp
        )
        _ <- hooks.loop.background
        _ <- hooks.purgeLoop.background
        notifier = new EventNotifier(events, users, push, stui, Some(hooks), cfg.api.publicUrl)
        // 外部のカレンダー（iCalendar の URL）の購読を定期的に読み直す.
        // 購読の URL は利用者が決めるので、確かめた IP にそのままつなぐクライアントで読む
        icsPolicy = NetGuard.Policy.of(cfg.icsAllowPrivate)
        icsHttp <- NetGuard.client(icsPolicy, timeout = 30.seconds).map(HttpClientTelemetry(_))
        importer = new CalendarImporter(icsHttp, calendars, events, users, icsPolicy, cfg.icsSyncMinutes.minutes)
        _ <- importer.loop.background
        _ <- purgeLoop(users, events).background
        _ <- notifier.reminderLoop.background
        web <- Resource.eval(cfg.staticDir.traverse(spa))
        all = ApiRoutes(
          oidc,
          users,
          calendars,
          events,
          sess,
          cfg.api,
          skaka,
          Some(pushStore -> push),
          Some(notifier),
          Some(new OAuthStore(xa)),
          Some(importer),
          stui,
          Some(hooks)
        ) <+>
          SecurityHeaders(ApiRoutes.headers(cfg.api))(
            Health.routes(Database.ping(xa)) <+> RumRoutes.routes <+> web.getOrElse(HttpRoutes.empty[IO])
          )
        httpApp <- HttpTelemetry(
          Errors.handle(all.orNotFound, ErrorCode.Unexpected),
          List("/api", "/auth", "/ics", "/mcp", "/oauth")
        )
        _ <- EmberServerBuilder.default[IO]
          .withHost(ipv4"0.0.0.0")
          .withPort(Port.fromInt(cfg.port).getOrElse(port"8080"))
          .withShutdownTimeout(5.seconds)
          .withHttpApp(httpApp)
          .build
        _ <- Resource.eval(sbirds.observe.Log.println(s"[skuaka] listening on :${cfg.port} (${cfg.api.publicUrl})"))
      } yield ()).useForever
    } yield ()

  /** DB が使えるまで待ってからマイグレーションする. */
  private def migrateWithRetry(cfg: AppConfig, attempt: Int = 1): IO[Int] =
    Database.migrate(cfg.db).handleErrorWith { e =>
      if attempt >= 60 then IO.raiseError(e)
      else
        sbirds.observe.Log.report(ErrorCode.DatabaseNotReady, s"${e.getMessage.take(120)}; retry $attempt/60") *>
          IO.sleep(2.seconds) *> migrateWithRetry(cfg, attempt + 1)
    }

  /** DB から定期的に読むメトリクス（skorimako のダッシュボード・アラート用）. */
  private def gauges(xa: doobie.Transactor[IO]): Resource[IO, Unit] = {
    def q(sqlText: doobie.Fragment): IO[Long] = sqlText.query[Long].unique.transact(xa)
    val events = new EventStore(xa)
    List(
      Metrics.gauge("skuaka.users.total", "有効なユーザーの数")(q(sql"select count(*) from app_user where not disabled")),
      Metrics.gauge(
        "skuaka.users.active_24h",
        "24時間以内にログインしたユーザーの数"
      )(q(sql"select count(*) from app_user where last_login_at > now() - interval '24 hours'")),
      Metrics.gauge(
        "skuaka.sessions.active",
        "有効なセッションの数"
      )(q(sql"select count(*) from session where expires_at > now()")),
      Metrics.gauge("skuaka.events.today", "今日（Asia/Tokyo）の予定の数（繰り返しは回ごと）", 5.minutes)(todayEvents(events)),
      Metrics.gauge("skuaka.events.total", "予定の数（繰り返しの予定は1つ）", 5.minutes)(q(sql"select count(*) from event")),
      Metrics.gauge("sbirds.push.subscriptions", "プッシュ通知を受け取る端末の数")(q(sql"select count(*) from push_subscription"))
    ).sequence_
  }

  /** 今日の予定の数（繰り返しを展開して数える）. */
  private def todayEvents(events: EventStore): IO[Long] = {
    val zone = java.time.ZoneId.of("Asia/Tokyo")
    IO(java.time.LocalDate.now(zone)).flatMap { d =>
      val (from, to) = (d.atStartOfDay(zone).toInstant, d.plusDays(1).atStartOfDay(zone).toInstant)
      events.overlapping(from, to).map(_.map(r =>
        skuaka.core.Expand.occurrences(r.span, r.zone, r.recurrence, r.exdates.toSet, from, to, zone).size.toLong
      ).sum)
    }
  }

  /** 期限の切れたセッション・ログイン途中の状態・古いリマインダーの送信の記録を消す（1時間ごと）. */
  private def purgeLoop(users: UserStore, events: EventStore): IO[Unit] =
    Periodic.run(1.minute, 1.hour, ErrorCode.PurgeFailed)(
      IO.realTimeInstant.flatMap(users.purgeExpired) *> events.purgeReminders
    ).void

  // IdP が http（開発環境など）のときは、そのアイコンの画像も許す（https の画像はもとから許している）
  private val idpImg: String = sys.env.get("OIDC_ISSUER").flatMap(u => scala.util.Try(java.net.URI.create(u)).toOption)
    .filter(u => u.getScheme == "http" && u.getAuthority != null).fold("")(u => s" http://${u.getAuthority}")

  /**
   * 画面の CSP. スクリプトは自分のサイトのものと、index.html に直接書いたもの（テーマの読み込み. ハッシュで許す）だけ.
   * skorimako のブラウザの観測（RUM）が設定されていれば、その rum.js の読み込みと送信も許す（rum はそのオリジン）.
   * 画像はアイコン（IdP の画像は外部の https）のため広めにする. Service Worker・manifest は自分のサイトなので default-src で足りる.
   */
  def spaCsp(indexHtml: String, rum: Option[String] = RumRoutes.origin): String = {
    val inline = Spa.inlineScriptHashes(indexHtml)
    (List(
      "default-src 'self'",
      ("script-src 'self'" :: inline ++ rum.toList).mkString(" "),
      "style-src 'self' 'unsafe-inline'",
      s"img-src 'self' data: blob: https:$idpImg",
      ("connect-src 'self'" :: rum.toList).mkString(" "),
      "object-src 'none'",
      "base-uri 'self'",
      "frame-ancestors 'none'",
      "form-action 'self'"
    )).mkString("; ")
  }

  /** ビルド済みの画面の配信（画面の CSP を付ける）. 存在しないパスは index.html（SPA）. */
  private def spa(dir: String): IO[HttpRoutes[IO]] =
    Spa.indexHtml(dir).map { indexHtml =>
      val csp = spaCsp(indexHtml)
      Spa.routes(
        dir,
        Spa.firstSegment("api", "auth", "ics", "mcp", "oauth", "healthz"),
        notFound = Errors.respond(ErrorCode.PageNotFound),
        finish = (_, r) => r.putHeaders("Content-Security-Policy" -> csp)
      )
    }
}
