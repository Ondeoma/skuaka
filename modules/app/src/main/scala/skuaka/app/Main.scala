package skuaka.app

import cats.effect.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import doobie.implicits.*
import fs2.io.file.Path
import org.http4s.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.dsl.io.*
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.middleware.ErrorHandling
import org.http4s.server.staticcontent.*
import skuaka.api.*
import skuaka.auth.{OidcClient, OidcConfig}
import skuaka.db.*
import skuaka.observe.{HttpClientTelemetry, HttpTelemetry, Metrics, RumRoutes, Telemetry}
import skuaka.push.{PushService, PushStore}
import skuaka.shared.ErrorDto

import scala.concurrent.duration.*

final case class AppConfig(port: Int, db: DbConfig, oidc: Option[OidcConfig], api: ApiConfig, staticDir: Option[String],
    skaka: Option[SkakaConfig], appSecret: Option[String])

/** 設定は環境変数から読む. */
object Config:
  private def opt(name: String): Option[String] = sys.env.get(name).map(_.trim).filter(_.nonEmpty)
  private def int(name: String): Option[Int]    = opt(name).flatMap(_.toIntOption)
  private def enabled(name: String): Boolean    = opt(name).exists(_.equalsIgnoreCase("true"))
  private def required(name: String): Either[String, String] = opt(name).toRight(s"環境変数 $name を設定してください")
  private def list(name: String): List[String] = opt(name).toList.flatMap(_.split(Array(',', ' ')).map(_.trim).filter(_.nonEmpty))

  private def local(url: String): Boolean =
    scala.util.Try(java.net.URI.create(url).getHost).toOption.flatMap(Option(_)).map(_.toLowerCase)
      .exists(h => Set("localhost", "127.0.0.1", "[::1]").contains(h) || h.endsWith(".localhost"))

  def load: IO[AppConfig] = IO.fromEither(
    (for
      dbUrl  <- required("DATABASE_URL")
      dbUser <- required("DATABASE_USER")
      dbPass <- required("DATABASE_PASSWORD")
      pub    <- required("PUBLIC_URL").map(_.stripSuffix("/"))
      devLogin = enabled("DEV_LOGIN")
      oidc <- (opt("OIDC_ISSUER"), opt("OIDC_CLIENT_ID"), opt("OIDC_CLIENT_SECRET")) match
                case (Some(i), Some(c), Some(s)) =>
                  Right(Some(OidcConfig(i, c, s, s"$pub/auth/callback", opt("OIDC_GROUPS_CLAIM").getOrElse("groups"),
                    Some(list("OIDC_SCOPES")).filter(_.nonEmpty).getOrElse(List("openid", "profile", "email")), list("OIDC_ALLOWED_GROUPS").toSet)))
                // 開発用のログインは名前だけでだれにでもなれるので、手元（localhost）で開くときだけ
                case (None, None, None) if devLogin && !local(pub) => Left("DEV_LOGIN は PUBLIC_URL が localhost のときだけ使えます")
                case (None, None, None) if devLogin => Right(None)
                case _ => Left("OIDC_ISSUER・OIDC_CLIENT_ID・OIDC_CLIENT_SECRET を設定してください（開発中は DEV_LOGIN=true で省略できます）")
      // Web 会議（skaka）: 3つ揃ったときだけ. 連携のトークンを暗号化するので APP_SECRET も要る
      skaka <- (opt("SKAKA_URL"), opt("SKAKA_CLIENT_ID"), opt("SKAKA_CLIENT_SECRET")) match
                 case (Some(u), Some(i), Some(s)) =>
                   val url = u.stripSuffix("/")
                   Right(Some(SkakaConfig(url, opt("SKAKA_INTERNAL_URL").map(_.stripSuffix("/")).getOrElse(url), i, s)))
                 case (None, None, None) => Right(None)
                 case _ => Left("SKAKA_URL・SKAKA_CLIENT_ID・SKAKA_CLIENT_SECRET は3つとも設定してください")
      proxies <- list("TRUSTED_PROXIES").traverse(s => RateLimitConfig.proxy(s).toRight(s"TRUSTED_PROXIES の「$s」は IP か CIDR で書いてください"))
      secret <- opt("APP_SECRET") match
                  case Some(s) if s.length < 16 => Left("APP_SECRET は16文字以上にしてください")
                  case None if skaka.isDefined  => Left("skaka と連携するには APP_SECRET（16文字以上の秘密の値）を設定してください")
                  case s                        => Right(s)
    yield AppConfig(
      port = int("HTTP_PORT").getOrElse(8080),
      db = DbConfig(dbUrl, dbUser, dbPass, int("DATABASE_POOL_SIZE").getOrElse(10)),
      oidc = oidc,
      api = ApiConfig(cookieSecure = pub.startsWith("https://"), publicUrl = pub, sessionTtl = int("SESSION_DAYS").getOrElse(14).days,
        devLogin = devLogin,
        rateLimit = RateLimitConfig(int("RATE_LIMIT_AUTH").getOrElse(20), int("RATE_LIMIT_PUBLIC").getOrElse(120), proxies)),
      staticDir = opt("STATIC_DIR"),
      skaka = skaka,
      appSecret = secret
    )).left.map(new IllegalArgumentException(_))
  )

object Main extends IOApp.Simple:

  // ログにトレースの ID・操作したユーザーを載せるため、IOLocal の文脈を Java のライブラリから見えるようにする
  // （IORuntime を作る前に設定する必要がある. オブジェクトの初期化は main より先に動く）
  Telemetry.prepare()

  private val version = Option(getClass.getPackage.getImplementationVersion).orElse(sys.env.get("APP_VERSION")).getOrElse("dev")

  private given org.typelevel.log4cats.LoggerFactory[IO] = org.typelevel.log4cats.slf4j.Slf4jFactory.create[IO]

  def run: IO[Unit] =
    for
      cfg <- Config.load
      n   <- migrateWithRetry(cfg)
      _   <- skuaka.observe.Log.println(s"[skuaka] database migrated ($n migration(s) applied)")
      _ <- (for
        _    <- Telemetry.resource("skuaka", version, runtime)
        xa   <- Database.transactor(cfg.db)
        _    <- gauges(xa)
        // 外への呼び出し（IdP・skaka・プッシュのサービス）もトレースに出す（traceparent を付けて送る）
        http <- EmberClientBuilder.default[IO].withTimeout(30.seconds).build.map(HttpClientTelemetry(_))
        oidc <- Resource.eval(cfg.oidc.traverse(OidcClient.create(_, http)))
        users     = new UserStore(xa)
        calendars = new CalendarStore(xa, users)
        events    = new EventStore(xa)
        sess      = new SessionAuth(users, cfg.api)
        skaka     = (cfg.skaka, cfg.appSecret).mapN((c, k) => new SkakaService(c, http, new IntegrationStore(xa), new skuaka.auth.SecretBox(k)))
        _ <- Resource.eval(skaka.traverse_(s => skuaka.observe.Log.println(s"[skuaka] skaka（Web 会議）: ${s.cfg.url}")))
        _ <- Resource.eval(IO.whenA(oidc.isEmpty)(skuaka.observe.Log.println("[skuaka] WARNING: OIDC が未設定です。開発用のログイン（DEV_LOGIN）を使います。本番では使わないでください")))
        // OIDC の設定の誤り（issuer の URL など）は起動時に知らせる（IdP が止まっていても起動はする）
        _ <- Resource.eval(oidc.traverse_(_.discover.attempt.flatMap {
               case Right(m) => skuaka.observe.Log.println(s"[skuaka] OIDC: ${m.issuer}")
               case Left(e)  => skuaka.observe.Log.println(s"[skuaka] WARNING: OIDC の discovery に失敗しました（${e.getMessage}）。ログインのときにもう一度試します")
             }))
        // プッシュ通知（PWA）: 招待と、予定のリマインダー（何分前のお知らせ）
        pushStore = new PushStore(xa)
        push     <- Resource.eval(PushService.create(pushStore, http, cfg.api.publicUrl, "skuaka"))
        notifier  = new EventNotifier(events, users, push)
        _ <- purgeLoop(users, events).background
        _ <- notifier.reminderLoop.background
        web <- Resource.eval(cfg.staticDir.traverse(spa))
        all = ApiRoutes(oidc, users, calendars, events, sess, cfg.api, skaka, Some(pushStore -> push), Some(notifier)) <+>
                SecurityHeaders(healthRoutes(xa) <+> RumRoutes.routes <+> web.getOrElse(HttpRoutes.empty[IO]), cfg.api.cookieSecure)
        httpApp <- HttpTelemetry(ErrorHandling.Custom.recoverWith(all.orNotFound)(handler), List("/api", "/auth", "/ics", "/mcp"))
        _ <- EmberServerBuilder.default[IO]
               .withHost(ipv4"0.0.0.0")
               .withPort(Port.fromInt(cfg.port).getOrElse(port"8080"))
               .withShutdownTimeout(5.seconds)
               .withHttpApp(httpApp)
               .build
        _ <- Resource.eval(skuaka.observe.Log.println(s"[skuaka] listening on :${cfg.port} (${cfg.api.publicUrl})"))
      yield ()).useForever
    yield ()

  /** DB が使えるまで待ってからマイグレーションする. */
  private def migrateWithRetry(cfg: AppConfig, attempt: Int = 1): IO[Int] =
    Database.migrate(cfg.db).handleErrorWith { e =>
      if attempt >= 60 then IO.raiseError(e)
      else skuaka.observe.Log.println(s"[skuaka] database not ready (${e.getMessage.take(120)}); retry $attempt/60") *> IO.sleep(2.seconds) *> migrateWithRetry(cfg, attempt + 1)
    }

  private def healthRoutes(xa: doobie.Transactor[IO]): HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "healthz" =>
      sql"select 1".query[Int].unique.transact(xa).timeout(3.seconds).attempt.map {
        case Right(_) => TextResponse(Status.Ok, "ok")
        case Left(_)  => TextResponse(Status.ServiceUnavailable, "database unavailable")
      }
  }

  private val handler: PartialFunction[Throwable, IO[Response[IO]]] = { case e =>
    IO(org.slf4j.LoggerFactory.getLogger("skuaka").error("unhandled error", e)) *> InternalServerError(ErrorDto("サーバーでエラーが発生しました"))
  }

  /** DB から定期的に読むメトリクス（skorimako のダッシュボード・アラート用）. */
  private def gauges(xa: doobie.Transactor[IO]): Resource[IO, Unit] =
    def q(sqlText: doobie.Fragment): IO[Long] = sqlText.query[Long].unique.transact(xa)
    val events = new EventStore(xa)
    List(
      Metrics.gauge("skuaka.users.total", "有効なユーザーの数")(q(sql"select count(*) from app_user where not disabled")),
      Metrics.gauge("skuaka.users.active_24h", "24時間以内にログインしたユーザーの数")(q(sql"select count(*) from app_user where last_login_at > now() - interval '24 hours'")),
      Metrics.gauge("skuaka.sessions.active", "有効なセッションの数")(q(sql"select count(*) from session where expires_at > now()")),
      Metrics.gauge("skuaka.events.today", "今日（Asia/Tokyo）の予定の数（繰り返しは回ごと）", 5.minutes)(todayEvents(events)),
      Metrics.gauge("skuaka.events.total", "予定の数（繰り返しの予定は1つ）", 5.minutes)(q(sql"select count(*) from event")),
      Metrics.gauge("skuaka.push.subscriptions", "プッシュ通知を受け取る端末の数")(q(sql"select count(*) from push_subscription"))
    ).sequence_

  /** 今日の予定の数（繰り返しを展開して数える）. */
  private def todayEvents(events: EventStore): IO[Long] =
    val zone = java.time.ZoneId.of("Asia/Tokyo")
    IO(java.time.LocalDate.now(zone)).flatMap { d =>
      val (from, to) = (d.atStartOfDay(zone).toInstant, d.plusDays(1).atStartOfDay(zone).toInstant)
      events.overlapping(from, to).map(_.map(r => skuaka.core.Expand.occurrences(r.span, r.zone, r.recurrence, r.exdates.toSet, from, to, zone).size.toLong).sum)
    }

  /** 期限の切れたセッション・ログイン途中の状態・古いリマインダーの送信の記録を消す（1時間ごと）. */
  private def purgeLoop(users: UserStore, events: EventStore): IO[Unit] =
    (IO.sleep(1.minute) *> ((IO.realTimeInstant.flatMap(users.purgeExpired) *> events.purgeReminders)
      .handleErrorWith(e => skuaka.observe.Log.println(s"[skuaka] purge failed: $e")) *> IO.sleep(1.hour)).foreverM).void

  // IdP が http（開発環境など）のときは、そのアイコンの画像も許す（https の画像はもとから許している）
  private val idpImg: String = sys.env.get("OIDC_ISSUER").flatMap(u => scala.util.Try(java.net.URI.create(u)).toOption)
    .filter(u => u.getScheme == "http" && u.getAuthority != null).fold("")(u => s" http://${u.getAuthority}")

  /**
   * 画面の CSP. スクリプトは自分のサイトのものと、index.html に直接書いたもの（テーマの読み込み. ハッシュで許す）だけ.
   * skorimako のブラウザの観測（RUM）が設定されていれば、その rum.js の読み込みと送信も許す（rum はそのオリジン）.
   * 画像はアイコン（IdP の画像は外部の https）のため広めにする. Service Worker・manifest は自分のサイトなので default-src で足りる.
   */
  def spaCsp(indexHtml: String, rum: Option[String] = RumRoutes.origin): String =
    val inline = "(?s)<script>(.*?)</script>".r.findAllMatchIn(indexHtml).map(m =>
      s"'sha256-${java.util.Base64.getEncoder.encodeToString(java.security.MessageDigest.getInstance("SHA-256").digest(m.group(1).getBytes("UTF-8")))}'")
    (List("default-src 'self'", ("script-src 'self'" :: inline.toList ++ rum.toList).mkString(" "), "style-src 'self' 'unsafe-inline'",
      s"img-src 'self' data: blob: https:$idpImg", ("connect-src 'self'" :: rum.toList).mkString(" "), "object-src 'none'", "base-uri 'self'",
      "frame-ancestors 'none'", "form-action 'self'")).mkString("; ")

  /** ビルド済みの画面の配信. 存在しないパスは index.html（SPA）. */
  private def spa(dir: String): IO[HttpRoutes[IO]] =
    IO.blocking(java.nio.file.Files.readString(java.nio.file.Path.of(dir, "index.html"))).map(spaRoutes(dir, _))

  private def spaRoutes(dir: String, indexHtml: String): HttpRoutes[IO] =
    val csp   = spaCsp(indexHtml)
    val files = fileService[IO](FileService.Config(dir))
    val index = HttpRoutes.of[IO] {
      case req @ GET -> path if !path.segments.headOption.map(_.decoded()).exists(Set("api", "auth", "ics", "mcp", "healthz")) =>
        StaticFile.fromPath(Path(dir) / "index.html", Some(req)).getOrElseF(NotFound())
    }
    HttpRoutes[IO] { req =>
      val cc = if req.pathInfo.renderString.startsWith("/assets/") then "public, max-age=31536000, immutable" else "no-cache"
      (files <+> index).run(req).map(_.putHeaders("Cache-Control" -> cc, "Content-Security-Policy" -> csp))
    }
