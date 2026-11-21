package skuaka.api

import java.util.UUID

/** 会議室の結び付ける予定の写し（skaka の RoomEventDto と同じ形）. */
final case class SkakaEvent(eventId: UUID, title: String, start: String, end: String, allDay: Boolean)
