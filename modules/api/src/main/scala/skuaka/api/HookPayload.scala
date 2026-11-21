package skuaka.api

import io.circe.{ACursor, Json}
import io.circe.syntax.*
import sbirds.links.StuiMessage
import skuaka.core.*
import skuaka.db.*
import skuaka.shared.*

import java.util.UUID

/**
 * 通知先に送るできごとの本文（Webhook の JSON. docs/design.md の「通知先」）. stui には、これを文にして送る（message）.
 * {type, calendar: {id, name}, actor?: {id, name}, event: {id, title, description, location, allDay, start, end, timezone,
 *  recurring, when, url}, minutes?, scope?}
 */
object HookPayload {

  /** 予定を開く URL（その回の日を開いて詳細を出す）. 削除した予定は、その日のカレンダー. */
  def link(publicUrl: String, r: EventRow, o: Span, deleted: Boolean = false): String = {
    val date = When.date(o, r.zone)
    if deleted then s"$publicUrl/?date=$date" else s"$publicUrl/?event=${r.id}&date=$date"
  }

  /** 予定のできごと. o は知らせる回（リマインダーはその回、ほかは予定の最初の回）. scope は繰り返しの予定の変更・削除の範囲. */
  def of(kind: String,
         calendar: (UUID, String),
         r: EventRow,
         o: Span,
         publicUrl: String,
         actor: Option[User] = None,
         minutes: Option[Int] = None,
         scope: Option[String] = None): Json =
    Json.obj(
      "type" -> kind.asJson,
      "calendar" -> Json.obj("id" -> calendar._1.asJson, "name" -> calendar._2.asJson),
      "actor" -> actor.map(u => Json.obj("id" -> u.id.asJson, "name" -> u.name.asJson)).asJson,
      "event" -> Json.obj(
        "id" -> r.id.asJson,
        "title" -> r.title.asJson,
        "description" -> r.description.take(2000).asJson,
        "location" -> r.location.asJson,
        "allDay" -> r.allDay.asJson,
        "start" -> o.startText.asJson,
        "end" -> o.endText.asJson,
        "timezone" -> r.timezone.asJson,
        "recurring" -> r.rrule.isDefined.asJson,
        "when" -> When.range(o, r.zone).asJson,
        "url" -> link(publicUrl, r, o, kind == HookEvent.Deleted).asJson
      ),
      "minutes" -> minutes.asJson,
      "scope" -> scope.filter(_ => r.rrule.isDefined).asJson
    ).dropNullValues

  /** 「テストを送る」. */
  def ping(calendar: (UUID, String), actor: User, publicUrl: String): Json = Json.obj(
    "type" -> HookEvent.Ping.asJson,
    "calendar" -> Json.obj("id" -> calendar._1.asJson, "name" -> calendar._2.asJson),
    "actor" -> Json.obj("id" -> actor.id.asJson, "name" -> actor.name.asJson),
    "url" -> publicUrl.asJson
  )

  private def str(c: ACursor, k: String): String = c.get[String](k).getOrElse("")

  private def scopeText(s: String): String = s match {
    case EditScope.This => "（この回）"
    case EditScope.Following => "（この回以降）"
    case _ => ""
  }

  /** stui に送る文（見出し・Markdown の本文・リンク）. */
  def message(payload: Json): StuiMessage = {
    val c = payload.hcursor
    val e = c.downField("event")
    val cal = StuiMessage.plain(str(c.downField("calendar"), "name"))
    val actor = StuiMessage.plain(str(c.downField("actor"), "name"))
    val title = str(e, "title")
    val loc = Option(str(e, "location")).filter(_.nonEmpty).fold("")(l => "\n📍 " + StuiMessage.plain(l))
    val when = str(e, "when")
    val url = str(e, "url")
    val scope = c.get[String]("scope").toOption.fold("")(scopeText)
    def change(icon: String, verb: String) =
      StuiMessage(s"$icon $title", s"$actor さんが「$cal」の予定を$verb$scope\n$when$loc", url)
    c.get[String]("type").getOrElse("") match {
      case HookEvent.Reminder =>
        StuiMessage(s"🔔 $title", when + When.until(c.get[Int]("minutes").getOrElse(0)) + loc, url)
      case HookEvent.Created => change("🆕", "追加しました")
      case HookEvent.Updated => change("✏️", "変更しました")
      case HookEvent.Deleted => change("🗑", "削除しました")
      case HookEvent.Ping =>
        StuiMessage(
          "🔔 skuaka からのテスト通知",
          s"カレンダー「$cal」の通知先の設定を確かめるためのテストです（$actor さんが送りました）。",
          str(c, "url")
        )
      case other => StuiMessage(s"🔔 $title", s"${HookEvent.label(other)}\n$when$loc", url)
    }
  }
}
