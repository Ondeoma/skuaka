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
                case (None, None, None) if devLogin => Right(None)
                case _ => Left("OIDC_ISSUER・OIDC_CLIENT_ID・OIDC_CLIENT_SECRET を設定してください（開発中は DEV_LOGIN=true で省略できます）")
      // Web 会議（skaka）: 3つ揃ったときだけ. 連携のトークンを暗号化するので APP_SECRET も要る
      skaka <- (opt("SKAKA_URL"), opt("SKAKA_CLIENT_ID"), opt("SKAKA_CLIENT_SECRET")) match
                 case (Some(u), Some(i), Some(s)) =>
                   val url = u.stripSuffix("/")
                   Right(Some(SkakaConfig(url, opt("SKAKA_INTERNAL_URL").map(_.stripSuffix("/")).getOrElse(url), i, s)))
                 case (None, None, None) => Right(None)
                 case _ => Left("SKAKA_URL・SKAKA_CLIENT_ID・SKAKA_CLIENT_SECRET は3つとも設定してください")
      secret <- opt("APP_SECRET") match
                  case Some(s) if s.length < 16 => Left("APP_SECRET は16文字以上にしてください")
                  case None if skaka.isDefined  => Left("skaka と連携するには APP_SECRET（16文字以上の秘密の値）を設定してください")
                  case s                        => Right(s)
    yield AppConfig(
      port = int("HTTP_PORT").getOrElse(8080),
      db = DbConfig(dbUrl, dbUser, dbPass, int("DATABASE_POOL_SIZE").getOrElse(10)),
      oidc = oidc,
      api = ApiConfig(cookieSecure = pub.startsWith("https://"), publicUrl = pub, sessionTtl = int("SESSION_DAYS").getOrElse(14).days,
        devLogin = devLogin),
      staticDir = opt("STATIC_DIR"),
      skaka = skaka,
      appSecret = secret
    )).left.map(new IllegalArgumentException(_))
  )

object Main extends IOApp.Simple:

  private given org.typelevel.log4cats.LoggerFactory[IO] = org.typelevel.log4cats.slf4j.Slf4jFactory.create[IO]

  def run: IO[Unit] =
    for
      cfg <- Config.load
      n   <- migrateWithRetry(cfg)
      _   <- IO.println(s"[skuaka] database migrated ($n migration(s) applied)")
      _ <- (for
        xa   <- Database.transactor(cfg.db)
        http <- EmberClientBuilder.default[IO].withTimeout(30.seconds).build
        oidc <- Resource.eval(cfg.oidc.traverse(OidcClient.create(_, http)))
        users     = new UserStore(xa)
        calendars = new CalendarStore(xa, users)
        events    = new EventStore(xa)
        sess      = new SessionAuth(users, cfg.api)
        skaka     = (cfg.skaka, cfg.appSecret).mapN((c, k) => new SkakaService(c, http, new IntegrationStore(xa), new skuaka.auth.SecretBox(k)))
        _ <- Resource.eval(skaka.traverse_(s => IO.println(s"[skuaka] skaka（Web 会議）: ${s.cfg.url}")))
        _ <- Resource.eval(IO.whenA(oidc.isEmpty)(IO.println("[skuaka] WARNING: OIDC が未設定です。開発用のログイン（DEV_LOGIN）を使います。本番では使わないでください")))
        // OIDC の設定の誤り（issuer の URL など）は起動時に知らせる（IdP が止まっていても起動はする）
        _ <- Resource.eval(oidc.traverse_(_.discover.attempt.flatMap {
               case Right(m) => IO.println(s"[skuaka] OIDC: ${m.issuer}")
               case Left(e)  => IO.println(s"[skuaka] WARNING: OIDC の discovery に失敗しました（${e.getMessage}）。ログインのときにもう一度試します")
             }))
        _ <- purgeLoop(users).background
        app = healthRoutes(xa) <+> ApiRoutes(oidc, users, calendars, events, sess, cfg.api, skaka)
        all = cfg.staticDir.fold(app)(d => app <+> spa(d))
        _ <- EmberServerBuilder.default[IO]
               .withHost(ipv4"0.0.0.0")
               .withPort(Port.fromInt(cfg.port).getOrElse(port"8080"))
               .withShutdownTimeout(5.seconds)
               .withHttpApp(ErrorHandling.Custom.recoverWith(all.orNotFound)(handler))
               .build
        _ <- Resource.eval(IO.println(s"[skuaka] listening on :${cfg.port} (${cfg.api.publicUrl})"))
      yield ()).useForever
    yield ()

  /** DB が使えるまで待ってからマイグレーションする. */
  private def migrateWithRetry(cfg: AppConfig, attempt: Int = 1): IO[Int] =
    Database.migrate(cfg.db).handleErrorWith { e =>
      if attempt >= 60 then IO.raiseError(e)
      else IO.println(s"[skuaka] database not ready (${e.getMessage.take(120)}); retry $attempt/60") *> IO.sleep(2.seconds) *> migrateWithRetry(cfg, attempt + 1)
    }

  private def healthRoutes(xa: doobie.Transactor[IO]): HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "healthz" =>
      sql"select 1".query[Int].unique.transact(xa).timeout(3.seconds).attempt.map {
        case Right(_) => TextResponse(Status.Ok, "ok")
        case Left(e)  => TextResponse(Status.ServiceUnavailable, s"database unavailable: ${e.getMessage}")
      }
  }

  private val handler: PartialFunction[Throwable, IO[Response[IO]]] = { case e =>
    IO.println(s"[skuaka] unhandled error: $e") *> InternalServerError(ErrorDto("サーバーでエラーが発生しました"))
  }

  /** 期限の切れたセッション・ログイン途中の状態を消す（1時間ごと）. */
  private def purgeLoop(users: UserStore): IO[Unit] =
    (IO.sleep(1.minute) *> (IO.realTimeInstant.flatMap(users.purgeExpired).handleErrorWith(e => IO.println(s"[skuaka] purge failed: $e")) *>
      IO.sleep(1.hour)).foreverM).void

  /** ビルド済みの画面の配信. 存在しないパスは index.html（SPA）. */
  private def spa(dir: String): HttpRoutes[IO] =
    val files = fileService[IO](FileService.Config(dir))
    val index = HttpRoutes.of[IO] {
      case req @ GET -> path if !path.segments.headOption.map(_.decoded()).exists(Set("api", "auth", "ics", "mcp", "healthz")) =>
        StaticFile.fromPath(Path(dir) / "index.html", Some(req)).getOrElseF(NotFound())
    }
    HttpRoutes[IO] { req =>
      val cc = if req.pathInfo.renderString.startsWith("/assets/") then "public, max-age=31536000, immutable" else "no-cache"
      (files <+> index).run(req).map(_.putHeaders("Cache-Control" -> cc))
    }
