package skuaka.web

import io.circe.*
import io.circe.syntax.*
import org.scalajs.dom
import skuaka.shared.*

import java.util.UUID
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/** バックエンドの REST API（共通部分は sbirds-lib の ApiBase. 更新の要求に付ける X-Skuaka は Main で Fetch.extraHeaders に入れる）. */
object Api extends sbirds.web.ApiBase {

  // ---------------------------------------------------------------- 認証・自分

  def clientConfig(): Future[ClientConfigDto] = get[ClientConfigDto]("/api/client-config")
  def me(): Future[MeDto] = get[MeDto]("/api/me")
  def devLogin(name: String, email: String, returnTo: String): Future[Json] =
    post[Json](
      "/auth/dev-login",
      Json.obj("name" -> name.asJson, "email" -> email.asJson, "returnTo" -> returnTo.asJson)
    )
  def logout(): Future[String] = post[Json]("/auth/logout").map(_.hcursor.get[String]("redirect").getOrElse("/"))
  def users(q: String): Future[List[UserBrief]] = get[List[UserBrief]]("/api/users?q=" + enc(q))
  def updateMe(r: UpdateMeReq): Future[MeDto] = patch[MeDto]("/api/me", r.asJson)

  /** アイコンを保存する（画像そのものを送る）・消す. */
  def uploadAvatar(image: dom.Blob): Future[MeDto] =
    putRaw[MeDto]("/api/me/avatar", image, Some(image.`type`))
  def deleteAvatar(): Future[MeDto] = request(dom.HttpMethod.DELETE, "/api/me/avatar").map(as[MeDto])

  // ---------------------------------------------------------------- カレンダー

  def calendars(): Future[List[CalendarDto]] = get[List[CalendarDto]]("/api/calendars")
  def createCalendar(r: CalendarReq): Future[CalendarDto] = post[CalendarDto]("/api/calendars", r.asJson)
  def updateCalendar(id: UUID, r: CalendarReq): Future[CalendarDto] = put[CalendarDto](s"/api/calendars/$id", r.asJson)
  def calendarPrefs(id: UUID, r: CalendarPrefReq): Future[CalendarDto] =
    patch[CalendarDto](s"/api/calendars/$id/prefs", r.asJson)

  /** 所有者は削除、ほかの人は一覧から外す. */
  def deleteCalendar(id: UUID): Future[Unit] = delete(s"/api/calendars/$id")
  def members(id: UUID): Future[List[MemberDto]] = get[List[MemberDto]](s"/api/calendars/$id/members")
  def setMember(id: UUID, r: MemberReq): Future[List[MemberDto]] =
    post[List[MemberDto]](s"/api/calendars/$id/members", r.asJson)
  def removeMember(id: UUID, user: UUID): Future[Unit] = delete(s"/api/calendars/$id/members/$user")

  /** チームの人が公開しているカレンダーを探す・自分の一覧に加える（外すのは deleteCalendar）. */
  def teamCalendars(q: String): Future[List[TeamCalendarDto]] =
    get[List[TeamCalendarDto]]("/api/team/calendars?q=" + enc(q))
  def addTeamCalendar(id: UUID): Future[CalendarDto] = post[CalendarDto](s"/api/team/calendars/$id")

  /** iCalendar のファイルを取り込む（ファイルはそのまま送る）. */
  def importIcs(id: UUID, file: dom.Blob): Future[ImportResultDto] =
    postRaw[ImportResultDto](s"/api/calendars/$id/import", file, Some("text/calendar"))

  /** iCalendar の URL を購読する・今すぐ読み直す（購読をやめるのは deleteCalendar）. */
  def subscribe(r: SubscribeReq): Future[CalendarDto] = post[CalendarDto]("/api/calendars/subscribe", r.asJson)
  def syncCalendar(id: UUID): Future[CalendarDto] = post[CalendarDto](s"/api/calendars/$id/sync")

  /** ログインしない人への公開リンクを作る・範囲を変える（regenerate で作り直す）・やめる. */
  def setPublicLink(id: UUID, r: PublicLinkReq): Future[CalendarDto] =
    post[CalendarDto](s"/api/calendars/$id/public", r.asJson)
  def stopPublicLink(id: UUID): Future[Unit] = delete(s"/api/calendars/$id/public")

  // ---------------------------------------------------------------- Web 会議（skaka）との連携

  def skakaStatus(): Future[SkakaStatusDto] = get[SkakaStatusDto]("/api/integrations/skaka")

  /** 連携を始める（skaka の許可の画面へ移り、終わると戻り先へ戻る）. */
  def skakaConnectUrl(returnTo: String): String = "/api/integrations/skaka/connect?returnTo=" + enc(returnTo)
  def disconnectSkaka(): Future[Unit] = delete("/api/integrations/skaka/connection")

  // ---------------------------------------------------------------- 公開リンク（ログインしない人）

  def publicCalendar(token: String): Future[PublicCalendarDto] = get[PublicCalendarDto](s"/api/public/${enc(token)}")
  def publicEvents(token: String, from: String, to: String): Future[List[OccurrenceDto]] =
    get[List[OccurrenceDto]](s"/api/public/${enc(token)}/events?from=$from&to=$to&tz=${enc(Cal.timezone())}")
  def publicEvent(token: String, id: UUID): Future[EventDto] = get[EventDto](s"/api/public/${enc(token)}/events/$id")

  // ---------------------------------------------------------------- 予定

  def events(from: String, to: String): Future[List[OccurrenceDto]] =
    get[List[OccurrenceDto]](s"/api/events?from=$from&to=$to&tz=${enc(Cal.timezone())}")
  def event(id: UUID): Future[EventDto] = get[EventDto](s"/api/events/$id")
  def search(q: String): Future[List[OccurrenceDto]] =
    get[List[OccurrenceDto]](s"/api/events/search?q=${enc(q)}&tz=${enc(Cal.timezone())}")

  private def scoped(id: UUID, scope: Option[(String, String)]): String =
    s"/api/events/$id" + scope.fold("")((sc, occ) => s"?scope=$sc&occurrence=${enc(occ)}")

  def saveEvent(id: Option[UUID], r: EventReq, scope: Option[(String, String)]): Future[EventDto] = id match {
    case None => post[EventDto]("/api/events", r.asJson)
    case Some(id) => put[EventDto](scoped(id, scope), r.asJson)
  }
  def deleteEvent(id: UUID, scope: Option[(String, String)]): Future[Unit] = delete(scoped(id, scope))
  def rsvp(id: UUID, response: String): Future[EventDto] =
    post[EventDto](s"/api/events/$id/rsvp", RsvpReq(response).asJson)

  /** 招待された予定を自分のカレンダーから外す. */
  def leaveEvent(id: UUID): Future[Unit] = delete(s"/api/events/$id/rsvp")
}
