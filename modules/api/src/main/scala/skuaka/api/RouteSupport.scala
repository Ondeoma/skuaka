package skuaka.api

import cats.effect.*
import io.circe.Decoder
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl
import org.typelevel.ci.CIString
import org.http4s.headers.`WWW-Authenticate`
import org.http4s.server.middleware.EntityLimiter
import sbirds.http.Errors
import sbirds.shared.AppError
import skuaka.core.ErrorCode
import skuaka.db.*

import java.util.UUID

/**
 * *Routes に共通の補助（エラー応答・認証・リクエスト本文）.
 */
trait RouteSupport extends Http4sDsl[IO] {
  protected def sessions: SessionAuth

  /** エラーの応答（コード付きでログに出る）. detail を省くとコードの既定の文言. */
  protected def err(code: ErrorCode, detail: String | Null = null): IO[Response[IO]] =
    Errors.respond(AppError(code, Option(detail)))

  /** 画面からの更新の要求に付けるヘッダー. 他のサイトのフォームからは付けられない（CORS の事前確認が要る）ので CSRF を防げる. */
  private val csrfHeader = CIString("X-Skuaka")

  private def readOnly(req: Request[IO]): Boolean = Set(Method.GET, Method.HEAD).contains(req.method)

  /**
   * ログインしていなければ 401.
   * Cookie のときは、GET 以外に X-Skuaka ヘッダーが必要. API トークン（Bearer）はブラウザが自動では付けないので CSRF の確認は要らないが、
   * 参照のみのトークンでは GET しかできない.
   */
  protected def authed(req: Request[IO])(f: User => IO[Response[IO]]): IO[Response[IO]] =
    principal(req)(p =>
      if p.canWrite || readOnly(req) then f(p.user) else err(ErrorCode.ApiTokenReadOnly)
    )

  /** 画面（Cookie のセッション）からだけ受け付ける操作（API トークンの管理など）. */
  protected def interactive(req: Request[IO])(f: User => IO[Response[IO]]): IO[Response[IO]] =
    principal(req)(p => if p.token.isEmpty then f(p.user) else err(ErrorCode.ApiTokenNotAllowed))

  private def principal(req: Request[IO])(f: Principal => IO[Response[IO]]): IO[Response[IO]] = {
    val bearer = sessions.bearer(req).isDefined
    if !bearer && !readOnly(req) && req.headers.get(csrfHeader).isEmpty then
      err(ErrorCode.CrossSiteRequest)
    else
      sessions.fromRequest(req).flatMap {
        // 操作したユーザーを、トレース（スパンの属性）とこの中のログに載せる
        case Some(p) => sbirds.observe.Telemetry.withUser(p.user.id.toString, p.user.name)(f(p))
        case None if bearer =>
          err(ErrorCode.ApiTokenInvalid).map(_.putHeaders(`WWW-Authenticate`(Challenge("Bearer", "skuaka"))))
        case None => err(ErrorCode.NotLoggedIn)
      }
  }

  protected def body[A: Decoder](req: Request[IO])(f: A => IO[Response[IO]]): IO[Response[IO]] =
    req.as[A].attempt.flatMap {
      case Left(_: EntityLimiter.EntityTooLarge) => err(ErrorCode.RequestTooLarge)
      case Left(e) => err(ErrorCode.RequestBodyInvalid, s"リクエストの内容が正しくありません: ${e.getMessage.take(200)}")
      case Right(b) => f(b)
    }

  protected def param(req: Request[IO], k: String): Option[String] = req.params.get(k).map(_.trim).filter(_.nonEmpty)

  protected object UuidVar {
    def unapply(s: String): Option[UUID] = scala.util.Try(UUID.fromString(s)).toOption
  }

  /** 結果が Right なら f、Left なら 400. */
  protected def valid[A](e: Either[String, A])(f: A => IO[Response[IO]]): IO[Response[IO]] =
    e.fold(err(ErrorCode.InvalidInput, _), f)
}
