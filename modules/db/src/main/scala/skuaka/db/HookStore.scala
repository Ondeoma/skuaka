package skuaka.db

import sbirds.db.*
import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import doobie.postgres.circe.jsonb.implicits.*
import io.circe.Json
import skuaka.shared.*

import java.time.Instant
import java.util.UUID

/** カレンダーの通知先. kind は HookKind. ownerId は stui のチャンネルに投稿するときに連携を使う人. */
final case class HookRow(id: UUID,
                         calendarId: UUID,
                         kind: String,
                         name: String,
                         url: String,
                         secret: String,
                         events: List[String],
                         enabled: Boolean,
                         ownerId: Option[UUID],
                         channelId: Option[String],
                         channelName: Option[String])

/** 送信待ちの1件（通知先の設定を合わせて読む）. */
final case class HookDelivery(id: UUID, event: String, payload: Json, attempts: Int, hook: HookRow)

/** 保存する通知先の中身（確かめた後のもの）. */
final case class HookSave(kind: String,
                          name: String,
                          url: String,
                          events: List[String],
                          enabled: Boolean,
                          ownerId: Option[UUID],
                          channelId: Option[String],
                          channelName: Option[String])

/** カレンダーの通知先（stui のチャンネル・stui の受信 Webhook・Webhook）と送信待ち. */
final class HookStore(xa: Transactor[IO]) extends TxStore(xa) {

  /** 送信を諦めるまでの回数. */
  val MaxAttempts = 8

  private val cols =
    fr"h.id, h.calendar_id, h.kind, h.name, h.url, h.secret, h.events, h.enabled, h.owner_id, h.channel_id, h.channel_name"

  def list(calendarId: UUID): IO[List[HookRow]] =
    tx((fr"select" ++ cols ++ fr"from calendar_hook h where h.calendar_id = $calendarId order by h.created_at").query[
      HookRow
    ].to[List])

  def get(id: UUID): IO[Option[HookRow]] =
    tx((fr"select" ++ cols ++ fr"from calendar_hook h where h.id = $id").query[HookRow].option)

  def create(calendarId: UUID, s: HookSave, secret: String, by: UUID): IO[UUID] = tx {
    val id = UUID.randomUUID()
    sql"""insert into calendar_hook (id, calendar_id, kind, name, url, secret, events, enabled, owner_id, channel_id, channel_name, created_by)
          values ($id, $calendarId, ${s.kind}, ${s.name}, ${s.url}, $secret, ${s.events.toArray}, ${s.enabled},
                  ${s.ownerId}, ${s.channelId}, ${s.channelName}, $by)""".update.run.as(id)
  }

  /** 変える（種類を変えたら secret も作り直したものにする）. */
  def update(id: UUID, s: HookSave, secret: String): IO[Unit] = tx(
    sql"""update calendar_hook set kind = ${s.kind}, name = ${s.name}, url = ${s.url}, secret = $secret,
          events = ${s.events.toArray}, enabled = ${s.enabled}, owner_id = ${s.ownerId},
          channel_id = ${s.channelId}, channel_name = ${s.channelName} where id = $id""".update.run.void
  )

  /** カレンダーの名前（本文に載せる）. */
  def calendarName(id: UUID): IO[Option[String]] =
    tx(sql"select name from calendar where id = $id".query[String].option)

  def delete(id: UUID): IO[Unit] = tx(sql"delete from calendar_hook where id = $id".update.run.void)

  /** できごとを、そのカレンダーの有効で、それを選んだ通知先の送信待ちに積む. 積んだ数. */
  def enqueue(calendarId: UUID, event: String, payload: Json): IO[Int] = tx(
    sql"""insert into calendar_hook_delivery (id, hook_id, event, payload)
          select gen_random_uuid(), id, $event, $payload from calendar_hook
          where calendar_id = $calendarId and enabled and $event = any(events)""".update.run
  )

  /** 1つの通知先だけに積む（テストの送信）. */
  def enqueueOne(hookId: UUID, event: String, payload: Json): IO[Unit] = tx(
    sql"""insert into calendar_hook_delivery (id, hook_id, event, payload)
          values (${UUID.randomUUID()}, $hookId, $event, $payload)""".update.run.void
  )

  /** 通知先ごとの最後の送信. */
  def lastDeliveries(hookIds: List[UUID]): IO[Map[UUID, HookDeliveryDto]] =
    if hookIds.isEmpty then IO.pure(Map.empty)
    else
      tx(sql"""select distinct on (hook_id) hook_id, id, event, created_at, done_at is not null, failed, attempts, status_code, last_error
               from calendar_hook_delivery where hook_id = any(${hookIds.toArray}) order by hook_id, created_at desc"""
        .query[(UUID, UUID, String, Instant, Boolean, Boolean, Int, Option[Int], Option[String])].to[List]).map(
        _.map((h, id, e, at, done, failed, n, code, err) =>
          h -> HookDeliveryDto(id, e, at.toString, done, failed, n, code, err)
        ).toMap
      )

  /** 最近の送信（新しい順）. */
  def deliveries(hookId: UUID, limit: Int): IO[List[HookDeliveryDto]] =
    tx(sql"""select id, event, created_at, done_at is not null, failed, attempts, status_code, last_error
             from calendar_hook_delivery where hook_id = $hookId order by created_at desc limit $limit"""
      .query[(UUID, String, Instant, Boolean, Boolean, Int, Option[Int], Option[String])].to[List]).map(
      _.map((id, e, at, done, failed, n, code, err) => HookDeliveryDto(id, e, at.toString, done, failed, n, code, err))
    )

  /**
   * 送る時刻の来た送信待ちを batch 件まで取り出して送る（複数ノードでも SKIP LOCKED で重ならない）.
   * 失敗したら間を空けて送り直し、MaxAttempts 回で諦める. Skip は送り直さない.
   */
  def process(batch: Int)(send: HookDelivery => IO[(OutboxResult, Option[Int])]): IO[Int] =
    processEach(
      (fr"select d.id, d.event, d.payload, d.attempts," ++ cols ++
        fr"""from calendar_hook_delivery d join calendar_hook h on h.id = d.hook_id
            where d.done_at is null and not d.failed and d.not_before <= now()
            order by d.not_before limit $batch for update of d skip locked""").query[HookDelivery].to[List]
    )(d => send(d).handleError(e => (OutboxResult.Retry(e.getClass.getSimpleName), None))) {
      case (d, (OutboxResult.Sent, code)) =>
        sql"""update calendar_hook_delivery set done_at = now(), attempts = attempts + 1, status_code = $code, last_error = null
              where id = ${d.id}""".update.run
      case (d, (OutboxResult.Skip(r), code)) =>
        sql"""update calendar_hook_delivery set failed = true, attempts = attempts + 1, last_error = $r, status_code = $code
              where id = ${d.id}""".update.run
      case (d, (OutboxResult.Retry(err), code)) =>
        sql"""update calendar_hook_delivery set attempts = attempts + 1, last_error = $err, status_code = $code,
              failed = ${d.attempts + 1 >= MaxAttempts},
              not_before = now() + make_interval(secs => ${Outbox.backoffSeconds(
            d.attempts
          )}) where id = ${d.id}""".update.run
    }

  /** 古い送信の記録を消す（送り終えたか諦めたもの）. */
  def purge(days: Int): IO[Int] = tx(
    sql"""delete from calendar_hook_delivery where created_at < now() - make_interval(days => $days)
          and (done_at is not null or failed)""".update.run
  )
}
