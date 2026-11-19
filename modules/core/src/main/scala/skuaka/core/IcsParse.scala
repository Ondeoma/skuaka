package skuaka.core

import skuaka.shared.Recurrence

import java.time.format.DateTimeFormatter
import java.time.{Duration, Instant, LocalDate, LocalDateTime, ZoneId, ZoneOffset}
import scala.util.Try

/**
 * iCalendar（RFC 5545）から読み取った予定. uid は取り込み先での識別（同じ予定を取り込み直したら上書きする）.
 * 繰り返しの一部の回だけを変えたもの（RECURRENCE-ID）は、元の予定から除いて別の予定にする（uid は「元の UID#その回」）.
 * approximated は、skuaka で表せない繰り返しの指定（BYYEARDAY・BYWEEKNO・BYHOUR など）を省いたもの.
 */
final case class ParsedEvent(uid: String,
                             title: String,
                             description: String,
                             location: String,
                             span: Span,
                             zone: ZoneId,
                             rule: Option[Recurrence],
                             exdates: List[String],
                             approximated: Boolean)

/** 読み取った結果. name は X-WR-CALNAME（カレンダーの名前）、skipped は読み取れなかった予定の数. */
final case class ParsedCalendar(name: Option[String], events: List[ParsedEvent], skipped: Int)

/** iCalendar の読み取り（Google カレンダー・Outlook・Apple のカレンダーなどが書き出したもの）. */
object IcsParse {

  /** 1つのファイルから読む予定の数の上限. */
  val MaxEvents = 20000

  /** Outlook などが使う Windows のタイムゾーンの名前のうち、よく使うもの. */
  private val windowsZones = Map(
    "Tokyo Standard Time" -> "Asia/Tokyo",
    "Korea Standard Time" -> "Asia/Seoul",
    "China Standard Time" -> "Asia/Shanghai",
    "Taipei Standard Time" -> "Asia/Taipei",
    "Singapore Standard Time" -> "Asia/Singapore",
    "India Standard Time" -> "Asia/Kolkata",
    "UTC" -> "UTC",
    "GMT Standard Time" -> "Europe/London",
    "W. Europe Standard Time" -> "Europe/Berlin",
    "Romance Standard Time" -> "Europe/Paris",
    "Central Europe Standard Time" -> "Europe/Budapest",
    "Eastern Standard Time" -> "America/New_York",
    "Central Standard Time" -> "America/Chicago",
    "Mountain Standard Time" -> "America/Denver",
    "Pacific Standard Time" -> "America/Los_Angeles",
    "AUS Eastern Standard Time" -> "Australia/Sydney",
    "Hawaiian Standard Time" -> "Pacific/Honolulu"
  )

  /** 1行のプロパティ（名前・パラメータ・値）. */
  final case class Prop(name: String, params: Map[String, String], value: String) {
    def param(k: String): Option[String] = params.get(k)
  }

  /** 折り返し（行頭の空白・タブ）を戻す. */
  def unfold(text: String): List[String] =
    text.stripPrefix("﻿").replace(
      "\r\n",
      "\n"
    ).replace("\r", "\n").replaceAll("\n[ \t]", "").split('\n').toList.filter(_.nonEmpty)

  /** 「NAME;P1=a;P2="b:c":値」を読む（引用符の中の : ; は区切りにしない）. */
  def prop(line: String): Option[Prop] = {
    var i = 0
    var quoted = false
    var colon = -1
    while colon < 0 && i < line.length do {
      val c = line.charAt(i)
      if c == '"' then quoted = !quoted
      else if c == ':' && !quoted then colon = i
      i += 1
    }
    Option.when(colon > 0) {
      val head = line.substring(0, colon)
      val parts = splitUnquoted(head, ';')
      val params = parts.drop(1).flatMap { p =>
        p.split("=", 2) match {
          case Array(k, v) => Some(k.trim.toUpperCase -> v.trim.stripPrefix("\"").stripSuffix("\""))
          case _ => None
        }
      }.toMap
      Prop(parts.head.trim.toUpperCase, params, line.substring(colon + 1))
    }
  }

  private def splitUnquoted(s: String, sep: Char): List[String] = {
    val out = List.newBuilder[String]
    val cur = new StringBuilder
    var quoted = false
    s.foreach { c =>
      if c == '"' then { quoted = !quoted; cur.append(c) }
      else if c == sep && !quoted then { out += cur.toString; cur.clear() }
      else cur.append(c)
    }
    out += cur.toString
    out.result()
  }

  /** TEXT の値の \n・\,・\;・\\ を戻す. */
  def unescape(s: String): String = {
    val out = new StringBuilder
    var i = 0
    while i < s.length do {
      val c = s.charAt(i)
      if c == '\\' && i + 1 < s.length then {
        s.charAt(i + 1) match {
          case 'n' | 'N' => out.append('\n')
          case other => out.append(other)
        }
        i += 2
      } else {
        out.append(c)
        i += 1
      }
    }
    out.toString
  }

  /** TZID からタイムゾーン（IANA の名前・Windows の名前. 分からなければ None）. */
  def zoneOf(tzid: String): Option[ZoneId] = {
    val id = tzid.trim.stripPrefix("/")
    Time.zone(id).orElse(windowsZones.get(id).flatMap(Time.zone)).orElse {
      // 「/mozilla.org/20050126_1/Asia/Tokyo」のような接頭辞の付いたもの
      id.split('/').toList.sliding(2).map(_.mkString("/")).toList.reverse.flatMap(Time.zone).headOption
    }
  }

  private val dateFmt = DateTimeFormatter.BASIC_ISO_DATE
  private val dateTimeFmt = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss")

  /** 日付・日時の値. */
  enum When {
    case Date(
      d: LocalDate)
    case At(
      i: Instant,
      zone: Option[ZoneId]
    ) // zone は TZID か浮動の時刻で決めたもの（UTC の Z なら None）
  }

  /** DTSTART などの値を読む. 浮動の時刻（Z も TZID もない）は fallback のタイムゾーンで読む. */
  def when(p: Prop, fallback: ZoneId): Option[When] = when(p.value.trim, p.param("VALUE"), p.param("TZID"), fallback)

  def when(v: String, valueType: Option[String], tzid: Option[String], fallback: ZoneId): Option[When] =
    if valueType.exists(_.equalsIgnoreCase("DATE")) || v.length == 8 then
      Try(LocalDate.parse(v.take(8), dateFmt)).toOption.map(When.Date(_))
    else if v.endsWith("Z") || v.endsWith("z") then
      Try(LocalDateTime.parse(v.dropRight(1).take(15), dateTimeFmt)).toOption.map(t =>
        When.At(t.toInstant(ZoneOffset.UTC), None)
      )
    else {
      val zone = tzid.flatMap(zoneOf).getOrElse(fallback)
      Try(LocalDateTime.parse(v.take(15), dateTimeFmt)).toOption.map(t => When.At(t.atZone(zone).toInstant, Some(zone)))
    }

  /** DURATION（P1D・PT1H30M・-PT15M など）. */
  def duration(v: String): Option[Duration] = {
    val s = v.trim.toUpperCase
    val (neg, body) = if s.startsWith("-") then (true, s.drop(1)) else (false, s.stripPrefix("+"))
    val weeks = "^P(\\d+)W$".r
    val d = body match {
      case weeks(w) => Some(Duration.ofDays(w.toLong * 7))
      case _ =>
        // java の Duration は日（D）までしか読めないので、日を時間に直してから読む
        val days = "^P(\\d+)D(.*)$".r
        body match {
          case days(n, rest) => Try(Duration.ofDays(n.toLong).plus(if rest.isEmpty then Duration.ZERO
            else Duration.parse("P" + rest))).toOption
          case _ => Try(Duration.parse(body)).toOption
        }
    }
    d.map(x => if neg then x.negated() else x)
  }

  /** 1つの VEVENT のプロパティ. */
  private final case class Raw(props: List[Prop]) {
    def get(name: String): Option[Prop] = props.find(_.name == name)
    def all(name: String): List[Prop] = props.filter(_.name == name)
    def text(name: String): String = get(name).map(p => unescape(p.value).trim).getOrElse("")
  }

  /** iCalendar の文字列を読む（VCALENDAR がなければ Left）. defaultZone は浮動の時刻と、タイムゾーンのない予定の繰り返しの基準. */
  def parse(text: String, defaultZone: ZoneId): Either[String, ParsedCalendar] = {
    val lines = unfold(text)
    if !lines.exists(_.trim.equalsIgnoreCase("BEGIN:VCALENDAR")) then Left("iCalendar（.ics）の形式ではありません")
    else {
      // VEVENT の中の VALARM などは読み飛ばす
      val events = List.newBuilder[Raw]
      var stack = List.empty[String]
      var cur = List.newBuilder[Prop]
      var calName: Option[String] = None
      var calZone: Option[ZoneId] = None
      lines.flatMap(prop).foreach { p =>
        p.name match {
          case "BEGIN" =>
            val c = p.value.trim.toUpperCase
            if c == "VEVENT" && stack == List("VCALENDAR") then cur = List.newBuilder[Prop]
            stack = c :: stack
          case "END" =>
            val c = p.value.trim.toUpperCase
            if c == "VEVENT" && stack.headOption.contains("VEVENT") && stack.drop(1) == List("VCALENDAR") then
              events += Raw(cur.result())
            stack = stack.drop(1)
          case _ if stack == List("VEVENT", "VCALENDAR") => cur += p
          case "X-WR-CALNAME" if stack == List("VCALENDAR") => calName = Some(unescape(p.value).trim).filter(_.nonEmpty)
          case "X-WR-TIMEZONE" if stack == List("VCALENDAR") => calZone = zoneOf(p.value)
          case _ => ()
        }
      }
      val raws = events.result()
      val zone = calZone.getOrElse(defaultZone)
      val read = raws.take(MaxEvents).map(r => r -> event(r, zone))
      val ok = read.collect { case (r, Some(e)) => (r, e) }
      // 繰り返しの一部の回の変更: 元の予定からその回を除く（取り消された回は除くだけ）
      val byMaster = ok.collect { case (_, (_, Some(master))) => master }.groupMap(_._1)(_._2)
      val parsed = ok.flatMap {
        case (r, (e, None)) =>
          if cancelled(r) then None
          else Some(e.copy(exdates = (e.exdates ++ byMaster.getOrElse(e.uid, Nil)).distinct))
        case (r, (e, Some(_))) => Option.unless(cancelled(r))(e)
      }
      Right(ParsedCalendar(calName, parsed, raws.size - ok.size))
    }
  }

  private def cancelled(r: Raw): Boolean = r.text("STATUS").equalsIgnoreCase("CANCELLED")

  /** 1つの予定を読む. 繰り返しの一部の回の変更なら、元の予定の UID とその回のキー（元の予定での開始）も返す. */
  private def event(r: Raw, defaultZone: ZoneId): Option[(ParsedEvent, Option[(String, String)])] =
    for {
      uid <- Some(r.text("UID")).filter(_.nonEmpty)
      dtp <- r.get("DTSTART")
      start <- when(dtp, defaultZone)
      zone = start match {
        case When.At(_, Some(z)) => z
        case _ => defaultZone
      }
      span <- spanOf(start, r.get("DTEND").flatMap(when(_, zone)), r.get("DURATION").flatMap(p => duration(p.value)))
      recId = r.get("RECURRENCE-ID").flatMap(when(_, zone)).map(key)
    } yield {
      val (rule, approx) =
        r.get("RRULE").filter(_ => recId.isEmpty).map(p => ruleOf(p.value, zone)).getOrElse((None, false))
      val exdates = if rule.isEmpty then Nil
      else
        r.all("EXDATE").flatMap(p =>
          p.value.split(',').toList.flatMap(v => when(v.trim, p.param("VALUE"), p.param("TZID"), zone)).flatMap(exKey(
            span,
            _
          ))
        )
      val id = recId.fold(uid)(k => s"$uid#$k")
      (
        ParsedEvent(
          id,
          Some(r.text("SUMMARY")).filter(_.nonEmpty).getOrElse("（タイトルなし）"),
          r.text("DESCRIPTION"),
          r.text("LOCATION"),
          span,
          zone,
          rule,
          exdates.distinct,
          approx
        ),
        recId.map(uid -> _)
      )
    }

  /** 繰り返しの1回のキー（Span.key と同じ: 時刻のある予定は UTC の日時、終日の予定は日付）. */
  private def key(w: When): String = w match {
    case When.Date(d) => d.toString
    case When.At(i, _) => i.toString
  }

  /** EXDATE を予定の種類に合わせたキーにする（時刻のある予定に日付だけの EXDATE は合わせられないので捨てる）. */
  private def exKey(span: Span, w: When): Option[String] = (span, w) match {
    case (Span.AllDay(_, _), When.Date(d)) => Some(d.toString)
    case (Span.AllDay(_, _), When.At(i, z)) => Some(i.atZone(z.getOrElse(ZoneOffset.UTC)).toLocalDate.toString)
    case (Span.Timed(_, _), When.At(i, _)) => Some(i.toString)
    case (Span.Timed(_, _), When.Date(_)) => None
  }

  /** 開始と終わり（DTEND か DURATION. なければ終日は1日、時刻のある予定は開始と同じ）. 終日の予定の終わりはその日を含む. */
  private def spanOf(start: When, end: Option[When], dur: Option[Duration]): Option[Span] = start match {
    case When.Date(s) =>
      val e = end.collect { case When.Date(d) => d.minusDays(1) }
        .orElse(dur.map(d => s.plusDays(math.max(1L, d.toDays) - 1)))
        .getOrElse(s)
      Some(Span.AllDay(s, if e.isBefore(s) then s else e))
    case When.At(s, _) =>
      val e = end.collect { case When.At(i, _) => i }.orElse(dur.map(s.plus)).getOrElse(s)
      Some(Span.Timed(s, if e.isBefore(s) then s else e))
  }

  /** RRULE を読む. UNTIL は予定のタイムゾーンの日付に直す. skuaka で表せない指定があれば、それを省いて近似（true）にする. */
  def ruleOf(v: String, zone: ZoneId): (Option[Recurrence], Boolean) = {
    val kv = v.split(';').toList.flatMap(p =>
      p.split("=", 2) match {
        case Array(k, x) => Some(k.trim.toUpperCase -> x.trim)
        case _ => None
      }
    )
    val until = kv.find(_._1 == "UNTIL").map(_._2).flatMap(u => when(u, None, None, zone)).map {
      case When.Date(d) => d
      case When.At(i, _) => i.atZone(zone).toLocalDate
    }
    val unsupported = kv.exists((k, _) => !Recurrence.supportedKeys.contains(k))
    val base =
      kv.filter((k, _) => Recurrence.supportedKeys.contains(k) && k != "UNTIL").map((k, x) => s"$k=$x").mkString(";")
    val rule = Recurrence.parse(base).map(r => r.copy(until = until.map(_.toString))).flatMap(_.valid.toOption)
    (rule, unsupported && rule.isDefined)
  }
}
