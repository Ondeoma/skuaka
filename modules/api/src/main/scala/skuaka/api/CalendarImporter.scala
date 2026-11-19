package skuaka.api

import cats.effect.*
import cats.syntax.all.*
import org.http4s.*
import org.http4s.client.Client
import org.http4s.headers.{Accept, Location}
import skuaka.core.*
import skuaka.db.*
import sbirds.observe.{Log, Metrics}
import skuaka.shared.*

import java.net.URI
import java.nio.charset.StandardCharsets
import java.time.{Instant, ZoneId, ZoneOffset}
import java.util.UUID
import scala.concurrent.duration.*

/** 外部のカレンダーの取り込み（iCalendar のファイル）と、iCalendar の URL の購読（定期的に読み直す）. */
final class CalendarImporter(
  http: Client[IO],
  calendars: CalendarStore,
  events: EventStore,
  users: UserStore,
  guard: UrlGuard = UrlGuard.publicOnly,
  val syncEvery: FiniteDuration = 1.hour) {

  /** 読み込む iCalendar の大きさの上限と、転送の回数の上限. */
  private val MaxBytes = IcsImport.MaxBytes
  private val MaxRedirects = 3

  /** 取り込んだ予定の行（ゲスト・通知・色は付けない. 長すぎる文字は切る）. */
  private def row(
    calendarId: UUID,
    organizer: UUID,
    e: ParsedEvent
  ): (String, EventRow) = {
    val (sa, ea, sd, ed) = e.span match {
      case Span.Timed(s, en) => (Some(s), Some(en), None, None)
      case Span.AllDay(s, en) => (None, None, Some(s), Some(en))
    }
    e.uid.take(500) -> EventRow(
      UUID.randomUUID(),
      calendarId,
      Some(organizer),
      e.title.take(300),
      e.description.take(10000),
      e.location.take(500),
      e.span.isInstanceOf[Span.AllDay],
      sa,
      ea,
      sd,
      ed,
      e.zone.getId,
      None,
      e.rule.map(_.toRrule),
      e.exdates,
      Nil,
      1,
      Instant.now()
    )
  }

  /**
   * iCalendar の文字列をカレンダーに取り込む（元の UID ごとに、なければ作り、あれば上書き）.
   * zone は浮動の時刻（タイムゾーンのない日時）を読むタイムゾーン. removeMissing なら、ファイルにない取り込み済みの予定を消す（購読）.
   * 戻り値の名前は iCalendar に書かれたカレンダーの名前.
   */
  def importText(
    calendarId: UUID,
    organizer: UUID,
    text: String,
    zone: ZoneId,
    removeMissing: Boolean
  ): IO[Either[String, (ImportResultDto, Option[String])]] =
    IO(IcsParse.parse(text, zone)).flatMap {
      case Left(e) => IO.pure(Left(e))
      case Right(p) =>
        // 同じ UID が重なっていたら後のものを使う（1つの文で同じ行を2回書けないため）
        val rows = p.events.map(row(calendarId, organizer, _)).reverse.distinctBy(_._1).reverse
        events.importEvents(calendarId, rows, removeMissing).map {
          (
            c,
            u,
            r
          ) =>
            Right(ImportResultDto(c, u, r, p.skipped, p.events.count(_.approximated)) -> p.name)
        }
    }

  /** webcal:// を https:// に直し、http(s) の URL か確かめる. */
  def normalize(
    url: String
  ): Either[String, URI] = {
    val s = url.trim.replaceFirst("(?i)^webcals?://", "https://")
    Either.catchNonFatal(URI.create(s)).toOption
      .filter(u =>
        Option(u.getScheme).exists(x => x.equalsIgnoreCase("https") || x.equalsIgnoreCase("http")) &&
          Option(u.getHost).exists(_.nonEmpty)
      )
      .filter(_ => s.length <= 2000)
      .toRight("iCalendar の URL（https://… か webcal://…）を入力してください")
  }

  /** URL から iCalendar を読む（転送は MaxRedirects 回まで. 転送先も確かめる）. */
  def fetch(
    uri: URI,
    redirects: Int = 0
  ): IO[Either[String, String]] =
    guard.check(uri).flatMap {
      case Left(e) => IO.pure(Left(e))
      case Right(_) =>
        Uri.fromString(uri.toString) match {
          case Left(_) => IO.pure(Left("URL が正しくありません"))
          case Right(u) =>
            val req = Request[IO](Method.GET, u).putHeaders(
              Accept(MediaType.unsafeParse("text/calendar")),
              "User-Agent" -> "skuaka (iCalendar subscription)"
            )
            http.run(req).use { res =>
              res.status match {
                // http4s の isSuccess は転送（3xx）も含むので、2xx だけにする
                case s if s.responseClass == Status.Successful =>
                  res.body.take(MaxBytes.toLong + 1).compile.to(Array).map { b =>
                    if b.length > MaxBytes then Left(s"iCalendar が大きすぎます（${MaxBytes / 1024 / 1024}MB まで）")
                    else Right(new String(b, StandardCharsets.UTF_8))
                  }
                case s if s.responseClass == Status.Redirection && redirects < MaxRedirects =>
                  res.headers.get[Location].map(l => uri.resolve(l.uri.renderString)) match {
                    case Some(next) => normalize(next.toString).fold(e => IO.pure(Left(e)), fetch(_, redirects + 1))
                    case None => IO.pure(Left(s"転送先がありません（${s.code}）"))
                  }
                case s if s.code == 401 || s.code == 403 => IO.pure(Left(s"読む権限がありません（${s.code}）. 非公開の URL か確かめてください"))
                case s if s.code == 404 => IO.pure(Left("見つかりません（404）. URL を確かめてください"))
                case s => IO.pure(Left(s"読めませんでした（${s.code}）"))
              }
            }.timeout(60.seconds).handleError(e =>
              Left(s"読めませんでした（${Option(e.getMessage).getOrElse(e.getClass.getSimpleName).take(200)}）")
            )
        }
    }

  private def zoneOf(
    uid: UUID
  ): IO[ZoneId] = users.find(uid).map(_.flatMap(u => Time.zone(u.timezone)).getOrElse(ZoneOffset.UTC))

  /** 購読を読み直す（失敗の理由はカレンダーに残す）. */
  def sync(
    s: Subscription
  ): IO[Either[String, ImportResultDto]] =
    (for {
      zone <- zoneOf(s.ownerId)
      r <- normalize(s.url).fold(e => IO.pure(Left(e)), fetch(_))
      res <- r.fold(
        e => IO.pure(Left(e)),
        importText(s.calendarId, s.ownerId, _, zone, removeMissing = true).map(_.map(_._1))
      )
    } yield res).flatTap(r =>
      calendars.setSynced(s.calendarId, r.left.toOption) *>
        Metrics.count("skuaka.ics.subscription.syncs", "result" -> (if r.isRight then "ok" else "error"))
    )

  /** 購読の URL を確かめて読み、カレンダーを作って取り込む（読めなければ作らない）. 戻り値は作ったカレンダー. */
  def subscribe(
    u: User,
    r: SubscribeReq
  ): IO[Either[String, UUID]] =
    normalize(r.url) match {
      case Left(e) => IO.pure(Left(e))
      case Right(uri) =>
        fetch(uri).flatMap {
          case Left(e) => IO.pure(Left(e))
          case Right(text) =>
            val zone = Time.zone(u.timezone).getOrElse(ZoneOffset.UTC)
            IO(IcsParse.parse(text, zone)).flatMap {
              case Left(e) => IO.pure(Left(e))
              case Right(p) =>
                val name = r.name.map(_.trim).filter(_.nonEmpty).orElse(p.name).getOrElse(uri.getHost).take(100)
                val color = r.color.filter(c => Check.color(c).isRight).getOrElse(Palette.Default)
                for {
                  id <- calendars.createSubscribed(u.id, CalendarReq(name, color, ""), uri.toString)
                  _ <- importText(id, u.id, text, zone, removeMissing = true)
                  _ <- calendars.setSynced(id, None)
                } yield Right(id)
            }
        }
    }

  /** 時期の来た購読を読み直し続ける（5分ごとに見て、最後に読んでから syncEvery 経ったもの）. */
  def loop: IO[Nothing] = {
    val once = IO.realTimeInstant.flatMap(now => calendars.claimDue(now.minusSeconds(syncEvery.toSeconds), 20))
      .flatMap(_.traverse_(sync)).handleErrorWith(e => Log.report(ErrorCode.SubscriptionSyncFailed, e.toString, e))
    (IO.sleep(1.minute) *> (once *> IO.sleep(5.minutes)).foreverM)
  }
}
