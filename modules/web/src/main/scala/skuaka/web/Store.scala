package skuaka.web

import io.github.ondeoma.scalactive.components.ComponentManager.*
import org.scalajs.dom
import skuaka.shared.*

import java.util.UUID
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.util.{Failure, Success}

final case class Toast(id: Int, text: String, error: Boolean)

/** 編集中の予定（作成・変更の画面の入力）. occurrence は繰り返しのどの回を開いたか. */
final case class Draft(id: Option[UUID], occurrence: Option[String], wasRecurring: Boolean, calendarId: UUID, title: String,
                       description: String, location: String, allDay: Boolean, startDate: String, startTime: String, endDate: String,
                       endTime: String, timezone: String, color: Option[String], recurrence: Option[Recurrence], attendees: List[UserBrief],
                       reminders: List[Int], version: Option[Int], withMeeting: Boolean = false) {

  def toReq: EventReq = {
    val (s, e) =
      if (allDay) (startDate, if (endDate < startDate) startDate else endDate)
      else (Cal.instant(startDate, Cal.parseHm(startTime)), Cal.instant(endDate, Cal.parseHm(endTime)))
    EventReq(calendarId, title, description, location, allDay, s, e, timezone, color, recurrence, attendees.map(_.id), reminders, version, Option.when(withMeeting)(true))
  }
}

/** 画面全体で共有する状態（表示中の日付・表示の種類・カレンダー・予定）と、予定の操作. */
object Store {

  val me: RV[Option[MeDto]] = RV(None)
  /** Web 会議（skaka）と連携しているか（設定されていなければ None）. */
  val skaka: RV[Option[SkakaStatusDto]] = RV(None)
  /** 公開リンクで見ているとき（ログインしない人. 閲覧だけ）: 秘密の値とカレンダー. */
  val guest: RV[Option[(String, PublicCalendarDto)]] = RV(None)
  val config: RV[Option[ClientConfigDto]] = RV(None)
  val calendars: RV[List[CalendarDto]] = RV(Nil)
  val occurrences: RV[List[OccurrenceDto]] = RV(Nil)
  val loading: RV[Boolean] = RV(false)
  val toasts: RV[List[Toast]] = RV(Nil)

  /** 表示の種類（day / week / month / agenda）と、表示の基準の日. */
  val view: RV[String] = RV(Prefs.get("view", if (dom.window.innerWidth < 720) "day" else "week"))
  val date: RV[String] = RV(Cal.today())

  /** 週の始まりの曜日（0 = 日曜日）・土日を表示するか（この端末の設定）. */
  val weekStart: RV[Int] = RV(Prefs.get("weekStart", "0").toIntOption.getOrElse(0))
  val weekends: RV[Boolean] = RV(Prefs.get("weekends", "true") == "true")
  val sidebar: RV[Boolean] = RV(dom.window.innerWidth > 900)

  /** 開いているダイアログ（予定の編集・予定の詳細・カレンダーの設定・チームのカレンダーの追加）. */
  val editor: RV[Option[Draft]] = RV(None)
  val detail: RV[Option[OccurrenceDto]] = RV(None)
  val calSettings: RV[Option[UUID]] = RV(None)
  val teamPicker: RV[Boolean] = RV(false)
  /** カレンダーを追加する画面（新しいカレンダー・URL の購読・ファイルの取り込み）. */
  val addCalendar: RV[Boolean] = RV(false)

  val views: List[(String, String)] = List("day" -> "日", "week" -> "週", "month" -> "月", "agenda" -> "スケジュール")


  // ---------------------------------------------------------------- トースト

  private var toastSeq = 0

  def toast(text: String, error: Boolean = false): Unit = {
    toastSeq += 1
    val t = Toast(toastSeq, text, error)
    toasts := (toasts.v :+ t).takeRight(4)
    dom.window.setTimeout(() => dismiss(t.id), if (error) 9000 else 5000)
  }

  def dismiss(id: Int): Unit = toasts := toasts.v.filterNot(_.id == id)

  def error(e: Throwable): Unit = e match {
    case ApiError(401, _) => ()
    case other            => toast(other.getMessage, error = true)
  }

  /** Future の失敗をトーストで知らせる. */
  def report[A](f: Future[A])(ok: A => Unit): Unit = f.onComplete {
    case Success(a) => ok(a)
    case Failure(e) => error(e)
  }

  /** 失敗したらトーストで知らせてから failed（先に画面を変えておく操作の取り消し）. */
  def orUndo[A](f: Future[A])(failed: => Unit): Unit = f.onComplete {
    case Failure(e) => error(e); failed
    case _          => ()
  }

  // ---------------------------------------------------------------- 表示の範囲

  /** 週の表示の日（土日を隠す設定なら平日だけ）. */
  def weekDays(d: String): List[String] = {
    val all = Cal.range(Cal.weekStart(d, weekStart.v), 7)
    if (weekends.v) all else all.filter(x => Cal.dow(x) != 0 && Cal.dow(x) != 6)
  }

  /** 月の表示の日（6週間分）. */
  def monthDays(d: String): List[String] = Cal.range(Cal.weekStart(Cal.monthStart(d), weekStart.v), 42)

  /** 読み込む期間 [from, to). */
  def range(v: String, d: String): (String, String) = v match {
    case "day"    => (d, Cal.addDays(d, 1))
    case "month"  => { val ds = monthDays(d); (ds.head, Cal.addDays(ds.last, 1)) }
    case "agenda" => (d, Cal.addDays(d, 31))
    case _        => { val s = Cal.weekStart(d, weekStart.v); (s, Cal.addDays(s, 7)) }
  }

  /** 前・次へ（表示の種類の単位で）. */
  def step(n: Int): Unit = date := (view.v match {
    case "day"    => Cal.addDays(date.v, n)
    case "month"  => Cal.addMonths(date.v, n)
    case "agenda" => Cal.addDays(date.v, 31 * n)
    case _        => Cal.addDays(date.v, 7 * n)
  })

  def setView(v: String): Unit = { view := v; Prefs.set("view", v) }

  /** カレンダーの画面のパス（公開リンクで見ているときはそのページ）. */
  def home: String = guest.v.fold("/")((t, _) => s"/p/$t")

  /** カレンダーの画面へ（ほかの画面にいるとき）. */
  def goHome(): Unit = if (dom.window.location.pathname != home) Router.go(home)

  /** その日を日の表示で開く. */
  def openDay(d: String): Unit = { date := d; setView("day"); goHome() }

  /** ヘッダーの「2026年 11月」. */
  def title: CRV[String] = computed {
    val d = date.v
    if (dom.window.innerWidth < 720) view.v match {
      // スマートフォンでは短く
      case "day"    => Cal.label(d)
      case "agenda" => s"${Cal.month(d)}/${Cal.day(d)} 〜"
      case "week"   => s"${Cal.month(weekDays(d).head)}月"
      case _        => s"${Cal.month(d)}月"
    }
    else view.v match {
      case "day" => Cal.longLabel(d)
      case "week" =>
        val ds = weekDays(d)
        val (a, b) = (ds.head, ds.last)
        if (Cal.month(a) == Cal.month(b)) s"${Cal.year(a)}年 ${Cal.month(a)}月"
        else if (Cal.year(a) == Cal.year(b)) s"${Cal.year(a)}年 ${Cal.month(a)}月 – ${Cal.month(b)}月"
        else s"${Cal.year(a)}年 ${Cal.month(a)}月 – ${Cal.year(b)}年 ${Cal.month(b)}月"
      case "agenda" => s"${Cal.longLabel(d)} 〜"
      case _        => s"${Cal.year(d)}年 ${Cal.month(d)}月"
    }
  }

  // ---------------------------------------------------------------- 読み込み

  private var loadSeq = 0

  /** 表示中の期間の予定を読み直す（続けて呼んでも最後の結果だけを使う）. */
  def load(): Unit = if (me.v.isDefined || guest.v.isDefined) {
    loadSeq += 1
    val seq = loadSeq
    val (f, t) = range(view.v, date.v)
    loading := true
    guest.v.fold(Api.events(f, t))((tk, _) => Api.publicEvents(tk, f, t)).onComplete { r =>
      if (seq == loadSeq) {
        loading := false
        r match {
          case Success(os) => if (os != occurrences.v) occurrences := os
          case Failure(e)  => error(e)
        }
      }
    }
  }

  def loadCalendars(): Future[Unit] = Api.calendars().map(cs => calendars := cs)

  def loadSkaka(): Future[Unit] = Api.skakaStatus().map(st => skaka := Option.when(st.enabled)(st))

  /** 場所にある Web 会議（skaka）の会議室の URL. */
  def meetingUrl(location: String): Option[String] = config.v.flatMap(_.skakaUrl).flatMap(Meeting.find(location, _)).map(_._1)

  /** 公開リンクで開く（カレンダーは公開されたものだけ. 色・名前の表示に使う）. */
  def openGuest(token: String): Future[Unit] = Api.publicCalendar(token).map { c =>
    guest := Some(token -> c)
    calendars := List(CalendarDto(c.id, c.name, c.description, c.color, None, visible = true, c.access, c.owner, primary = false, None,
      TeamAccess.None, viaTeam = false))
  }

  /** 予定の詳細（公開リンクで見ているときは公開の範囲で）. */
  def fetchEvent(id: UUID): Future[EventDto] = guest.v.fold(Api.event(id))((t, _) => Api.publicEvent(t, id))

  /** 自分の操作の後に、予定を読み直す. */
  def changed(): Unit = { load(); reminders.refresh() }

  def calendar(id: UUID): Option[CalendarDto] = calendars.v.find(_.id == id)

  /** 予定の色（予定の色 → カレンダーの自分の色 → カレンダーの色）. */
  def color(o: OccurrenceDto): String =
    o.color.getOrElse(calendar(o.calendarId).map(_.displayColor).getOrElse("#616161"))

  /** 表示するカレンダーの予定だけ（招待された予定は、カレンダーを見られなくても出す）. */
  val shown: CRV[List[OccurrenceDto]] = computed {
    val hidden = calendars.v.filterNot(_.visible).map(_.id).toSet
    occurrences.v.filter(o => !hidden.contains(o.calendarId) || o.invited)
  }

  /** 予定を入れられるカレンダー（自分のメインのカレンダーが先頭）. */
  def writable: List[CalendarDto] =
    calendars.v.filter(_.writable).sortBy(c => (!c.primary, c.role != CalRole.Owner, c.name))

  /** 予定の最初の日と最後の日（画面のタイムゾーンで. 0時ちょうどに終わる予定は前の日まで）. */
  def firstDay(o: OccurrenceDto): String = if (o.allDay) o.start else Cal.dateOf(o.start)
  def lastDay(o: OccurrenceDto): String =
    if (o.allDay) o.end
    else { val e = Cal.dateOf(o.end); if (Cal.minutesOf(o.end) == 0 && o.end > o.start) Cal.addDays(e, -1) else e }

  /** その日にかかる予定か. */
  def onDay(o: OccurrenceDto, d: String): Boolean = firstDay(o) <= d && lastDay(o) >= d

  def dotStyle(o: OccurrenceDto): String = s"background: ${color(o)}"

  /** 予定のチップのクラス（招待の返事・予定あり・なしのみで見た目を変える）. */
  def chipClass(o: OccurrenceDto, base: String): String = base + (if (o.busy) " busy" else "") + (o.myResponse match {
    case Some(Rsvp.Declined)    => " declined"
    case Some(Rsvp.NeedsAction) => " pending"
    case Some(Rsvp.Tentative)   => " tentative"
    case _                      => ""
  })

  /**
   * 予定の日時の表示. withDate なら1日の予定にも日付を付ける.
   * 10:00 – 11:00 / 終日 / 11月15日(日) 10:00 – 11月16日(月) 09:00（withDate: 2026年11月15日(日) ⋅ 10:00 – 11:00）
   */
  def timeText(o: OccurrenceDto, withDate: Boolean = false): String =
    if (firstDay(o) != lastDay(o)) {
      if (o.allDay) s"${Cal.label(o.start)} – ${Cal.label(o.end)}"
      else s"${Cal.label(Cal.dateOf(o.start))} ${Cal.time(o.start)} – ${Cal.label(Cal.dateOf(o.end))} ${Cal.time(o.end)}"
    } else {
      val t = if (o.allDay) "終日" else s"${Cal.time(o.start)} – ${Cal.time(o.end)}"
      if (withDate) s"${Cal.longLabel(firstDay(o))} ⋅ $t" else t
    }

  // ---------------------------------------------------------------- 予定の作成・変更・削除

  def defaultCalendar: Option[CalendarDto] = writable.find(c => c.visible).orElse(writable.headOption)

  /**
   * 新しい予定の編集を始める（公開リンクで見ているときは何もしない）.
   * startMin は d の、endMin は endDate（省けば d）の0時からの分（終日なら無視する. 1440 なら翌日の0時）.
   */
  def create(d: String, startMin: Int = -1, endMin: Int = -1, allDay: Boolean = false, endDate: Option[String] = None): Unit =
    if (guest.v.isEmpty) defaultCalendar match {
      case None => toast("予定を入れられるカレンダーがありません", error = true)
      case Some(c) =>
        val now   = new js.Date()
        val s     = if (startMin >= 0) startMin else math.min(23 * 60, (now.getHours().toInt + 1) * 60)
        val other = endDate.exists(_ > d) // 日をまたぐ
        val e     = if (endMin >= 0 && (other || endMin > s)) endMin else s + 60
        val sDate = d
        val eDate = if (allDay) endDate.getOrElse(d) else Cal.addDays(endDate.getOrElse(d), e / 1440)
        editor := Some(Draft(None, None, wasRecurring = false, c.id, "", "", "", allDay, sDate, Cal.hm(s), eDate, Cal.hm(e % 1440),
          Cal.timezone(), None, None, Nil, if (allDay) Nil else List(10), None))
    }

  /** 予定（の1回）の編集を始める. */
  def edit(o: OccurrenceDto): Unit = report(Api.event(o.eventId)) { e =>
    detail := None
    editor := Some(draftOf(e, o))
  }

  def draftOf(e: EventDto, o: OccurrenceDto): Draft =
    if (o.allDay) Draft(Some(e.id), Some(o.occurrence), e.recurrence.isDefined, e.calendarId, e.title, e.description, e.location, true,
      o.start, "09:00", o.end, "10:00", e.timezone, e.color, e.recurrence, e.attendees.map(_.user), e.reminders, Some(e.version))
    else Draft(Some(e.id), Some(o.occurrence), e.recurrence.isDefined, e.calendarId, e.title, e.description, e.location, false,
      Cal.dateOf(o.start), Cal.time(o.start), Cal.dateOf(o.end), Cal.time(o.end), e.timezone, e.color, e.recurrence,
      e.attendees.map(_.user), e.reminders, Some(e.version))

  /** 繰り返しの予定なら、どの範囲に適用するかを聞いてから f（verb は「変更」「削除」）. */
  def askScope(recurring: Boolean, verb: String, onCancel: () => Unit = () => ())(f: Option[String] => Unit): Unit =
    if (!recurring) f(None)
    else Dialog.choose(s"繰り返しの予定を${verb}します", List(EditScope.This -> "この予定", EditScope.Following -> "これ以降のすべての予定",
      EditScope.All -> "すべての予定"), s"${verb}する", danger = verb == "削除", onCancel = onCancel)(sc => f(Some(sc)))

  /** 予定を保存する. 「この予定」と「これ以降」は新しい予定になるので、version は送らない. */
  private def saveDraft(d: Draft, scope: Option[String]): Future[EventDto] = {
    val req = if (scope.exists(_ != EditScope.All)) d.toReq.copy(version = None) else d.toReq
    Api.saveEvent(d.id, req, scope.flatMap(sc => d.occurrence.map(sc -> _)))
  }

  /** 編集中の予定を保存する. */
  def save(d: Draft)(done: => Unit): Unit =
    askScope(d.wasRecurring, "変更") { scope =>
      report(saveDraft(d, scope)) { e =>
        editor := None
        // 会議室を作れなくても、予定は保存してある
        e.meetingError.fold(toast(if (d.id.isEmpty) "予定を作成しました" else "予定を変更しました"))(m => toast(s"予定を保存しました。$m", error = true))
        done
        changed()
      }
    }

  def remove(o: OccurrenceDto): Unit =
    askScope(o.recurring, "削除") { scope =>
      val go = () => report(Api.deleteEvent(o.eventId, scope.map(_ -> o.occurrence))) { _ =>
        detail := None; editor := None
        toast("予定を削除しました")
        changed()
      }
      if (scope.isDefined) go() else Dialog.confirm(s"「${o.title}」を削除しますか？", "削除")(go())
    }

  /** ドラッグで動かした・伸ばした予定を保存する（新しい開始・終わり. 終日なら日付）. */
  def move(o: OccurrenceDto, start: String, end: String)(cancel: => Unit): Unit =
    Api.event(o.eventId).onComplete {
      case Failure(e) => error(e); cancel
      case Success(e) =>
        val d0 = draftOf(e, o)
        val d =
          if (o.allDay) d0.copy(startDate = start, endDate = end)
          else d0.copy(startDate = Cal.dateOf(start), startTime = Cal.time(start), endDate = Cal.dateOf(end), endTime = Cal.time(end))
        askScope(d.wasRecurring, "変更", () => cancel)(scope => saveDraft(d, scope).onComplete {
          case Success(_) => changed()
          case Failure(e) => error(e); cancel
        })
    }

  def rsvp(o: OccurrenceDto, response: String): Unit =
    report(Api.rsvp(o.eventId, response)) { _ =>
      detail := detail.v.map(_.copy(myResponse = Some(response)))
      changed()
    }

  // ---------------------------------------------------------------- 起動

  /** 表示の期間が変わったら読み直し、ほかの人の変更も1分ごとに取りに行く. */
  def start(): Unit = {
    watch(computed((view.v, date.v, weekStart.v, weekends.v)))((_, _) => load())
    dom.window.setInterval(() => if (dom.document.visibilityState.toString == "visible" && editor.v.isEmpty) load(), 60000)
    dom.document.addEventListener("visibilitychange", (_: dom.Event) => if (dom.document.visibilityState.toString == "visible") load())
    reminders.start()
    Keys.install()
    loadSkaka().failed.foreach(_ => ())
  }

  // ---------------------------------------------------------------- 通知（予定の前に知らせる）

  object reminders {
    private var upcoming: List[OccurrenceDto] = Nil
    private var last = js.Date.now()

    def refresh(): Unit = if (me.v.isDefined) {
      val t = Cal.today()
      Api.events(t, Cal.addDays(t, 3)).foreach(os => upcoming = os.filter(o => o.reminders.nonEmpty && !o.allDay))
    }

    private def check(): Unit = {
      val now = js.Date.now()
      for (o <- upcoming if !o.myResponse.contains(Rsvp.Declined); m <- o.reminders) {
        val at = new js.Date(o.start).getTime() - m * 60000.0
        if (at > last && at <= now) notify(o, m)
      }
      last = now
    }

    private def notify(o: OccurrenceDto, minutes: Int): Unit = {
      val when = if (minutes == 0) "" else s"（${Reminders.label(minutes).stripSuffix("前")}後）"
      val text = s"${Cal.time(o.start)}$when ${o.title}" + (if (o.location.nonEmpty) s"（${o.location}）" else "")
      toast("🔔 " + text)
      scala.util.Try {
        val n = js.Dynamic.global.Notification
        if (!js.isUndefined(n) && n.permission.asInstanceOf[String] == "granted")
          js.Dynamic.newInstance(n)("skuaka", js.Dynamic.literal(body = text, tag = s"${o.eventId}${o.occurrence}-$minutes"))
      }
    }

    def start(): Unit = {
      refresh()
      dom.window.setInterval(() => check(), 20000)
      dom.window.setInterval(() => refresh(), 5 * 60000)
    }
  }
}

/** キーボードの操作（入力欄の外で）: t 今日・j/k 次/前・d/w/m/a 表示・c 作成・/ 検索・? 一覧. */
object Keys {
  val help: RV[Boolean] = RV(false)

  val list: List[(String, String)] = List("t" -> "今日", "j / n" -> "次の期間", "k / p" -> "前の期間", "d" -> "日", "w" -> "週", "m" -> "月",
    "a" -> "スケジュール", "c" -> "予定を作成", "/" -> "検索", "?" -> "この一覧")

  def install(): Unit = dom.document.addEventListener("keydown", (e: dom.KeyboardEvent) => {
    val tag = Option(e.target.asInstanceOf[dom.Element]).map(_.tagName.toLowerCase).getOrElse("")
    val busy = Set("input", "textarea", "select").contains(tag) || e.ctrlKey || e.metaKey || e.altKey ||
      Store.editor.v.isDefined || Store.detail.v.isDefined || Store.calSettings.v.isDefined || Store.teamPicker.v || Store.addCalendar.v || Dialog.current.v.isDefined
    if (!busy && dom.window.location.pathname == Store.home) {
      val handled = e.key match {
        case "t"       => Store.date := Cal.today(); true
        case "j" | "n" => Store.step(1); true
        case "k" | "p" => Store.step(-1); true
        case "d"       => Store.setView("day"); true
        case "w"       => Store.setView("week"); true
        case "m"       => Store.setView("month"); true
        case "a"       => Store.setView("agenda"); true
        case "c"       => Store.create(Store.date.v); true
        case "/"       => Option(dom.document.querySelector("header .search input")).foreach(_.asInstanceOf[dom.HTMLElement].focus()); true
        case "?"       => help := !help.v; true
        case _         => false
      }
      if (handled) e.preventDefault()
    }
  })
}

/** DOM の補助. */
object Dom {
  def value(e: dom.Event): String = e.target.asInstanceOf[js.Dynamic].value.asInstanceOf[String]
  def checked(e: dom.Event): Boolean = e.target.asInstanceOf[dom.HTMLInputElement].checked

  def copy(text: String, message: String = "コピーしました"): Unit = {
    dom.window.navigator.clipboard.writeText(text)
    Store.toast(message)
  }
}

/** 画面内のダイアログ（ブラウザ標準の confirm・prompt の代わり）. 表示は parts/DialogHost.st. */
final case class DialogReq(message: String, okLabel: String, danger: Boolean, input: Option[RV[String]], choices: List[(String, String)],
                           onOk: String => Unit, onCancel: () => Unit)

object Dialog {
  val current: RV[Option[DialogReq]] = RV(None)

  def confirm(message: String, ok: String = "OK", danger: Boolean = true)(f: => Unit): Unit =
    current := Some(DialogReq(message, ok, danger, None, Nil, _ => f, () => ()))

  def prompt(message: String, default: String = "", ok: String = "OK")(f: String => Unit): Unit =
    current := Some(DialogReq(message, ok, danger = false, Some(RV(default)), Nil, f, () => ()))

  /** 選択肢（値 → 表示名）から1つ選ぶ. */
  def choose(message: String, choices: List[(String, String)], ok: String = "OK", danger: Boolean = false,
             onCancel: () => Unit = () => ())(f: String => Unit): Unit =
    current := Some(DialogReq(message, ok, danger, Some(RV(choices.headOption.map(_._1).getOrElse(""))), choices, f, onCancel))

  def close(ok: Boolean): Unit = current.v.foreach { r =>
    current := None
    if (ok) r.onOk(r.input.map(_.v).getOrElse("")) else r.onCancel()
  }
}

/** この端末だけの表示の設定（localStorage. 使えないときは既定値）. */
object Prefs {
  def get(key: String, default: String): String =
    scala.util.Try(Option(dom.window.localStorage.getItem("skuaka." + key))).toOption.flatten.getOrElse(default)

  def set(key: String, value: String): Unit = scala.util.Try(dom.window.localStorage.setItem("skuaka." + key, value))
}

/** 画面の色（自動 = OS の設定、ライト、ダーク）. */
object Theme {
  val current: RV[String] = RV(Prefs.get("theme", "system"))

  def apply(): Unit = {
    val el = dom.document.documentElement
    if (current.v == "system") el.removeAttribute("data-theme") else el.setAttribute("data-theme", current.v)
  }

  def set(v: String): Unit = { current := v; Prefs.set("theme", v); apply() }

  val options: List[(String, String)] = List("system" -> "自動（OS に合わせる）", "light" -> "ライト", "dark" -> "ダーク")
}
