package skuaka.api

import java.util.UUID

/**
 * Web 会議（skaka）との連携の設定. url は画面から開く skaka の URL（会議室の URL もこれで始まる）、
 * internalUrl はサーバーから呼ぶ URL（省けば url）.
 */
final case class SkakaConfig(
  url: String,
  internalUrl: String,
  clientId: String,
  clientSecret: String)

/** 会議室の結び付ける予定の写し（skaka の RoomEventDto と同じ形）. */
final case class SkakaEvent(
  eventId: UUID,
  title: String,
  start: String,
  end: String,
  allDay: Boolean)
