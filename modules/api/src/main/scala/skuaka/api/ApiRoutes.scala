package skuaka.api

import cats.effect.*
import cats.syntax.all.*
import org.http4s.*
import sbirds.http.{BodyLimit, SecurityHeaders}
import sbirds.shared.AppError
import skuaka.core.ErrorCode
import skuaka.db.*
import skuaka.shared.*

/** アプリのルートをまとめる（REST API・iCalendar の配信・MCP・プッシュ通知の購読・兄弟のサービスへの認可（OAuth））. MCP は REST API をトークンで内部から呼ぶ. */
object ApiRoutes {
  def apply(oidc: Option[sbirds.auth.OidcClient],
            users: UserStore,
            calendars: CalendarStore,
            events: EventStore,
            sess: SessionAuth,
            cfg: ApiConfig,
            skaka: Option[SkakaService] = None,
            push: Option[(sbirds.push.PushStore, sbirds.push.PushService)] = None,
            notifier: Option[EventNotifier] = None,
            oauth: Option[OAuthStore] = None,
            importer: Option[CalendarImporter] = None): HttpRoutes[IO] = {
    val calendarRoutes = new CalendarRoutes(calendars, events, users, sess, cfg, importer)
    val rest = new AuthRoutes(oidc, users, calendars, sess, cfg, skaka.map(_.cfg.url)).routes <+>
      calendarRoutes.routes <+>
      new EventRoutes(events, calendars, users, sess, skaka, notifier).routes <+>
      new SkakaRoutes(skaka, sess, cfg).routes <+>
      oauth.fold(HttpRoutes.empty[IO])(new OAuthRoutes(_, sess, cfg).routes) <+>
      push.fold(HttpRoutes.empty[IO])((store, service) => new PushRoutes(store, service, sess).routes)
    // 本文は MaxBody まで（大きな本文でメモリを使い切らせない. アイコンの 1MB より大きくしておく）. 超えていれば 413.
    // iCalendar のファイルの取り込みだけは、本文を IcsImport.MaxBytes まで受ける
    val all = BodyLimit(
      req => Some(if calendarRoutes.isImport(req) then IcsImport.MaxBytes.toLong else MaxBody),
      (req, _) =>
        if calendarRoutes.isImport(req) then CalendarRoutes.importTooLarge else AppError(ErrorCode.RequestTooLarge)
    )(calendarRoutes.importRoutes <+> rest <+> new McpRoutes(rest, sess, cfg.publicUrl, cfg.version).routes)
    SecurityHeaders(headers(cfg))(RateLimit(all, cfg.rateLimit))
  }

  /** すべての応答に付けるセキュリティのヘッダー（HSTS は https で公開しているときだけ）. */
  def headers(cfg: ApiConfig): SecurityHeaders.Policy =
    SecurityHeaders.Policy(hsts = SecurityHeaders.hsts(cfg.cookieSecure))

  val MaxBody: Long = 2L * 1024 * 1024
}
