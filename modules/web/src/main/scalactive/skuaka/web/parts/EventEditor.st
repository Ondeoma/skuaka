<script lang="scala">
import skuaka.web.*
import skuaka.shared.*

import java.util.UUID

// 予定の作成・編集（日時・終日・繰り返し・カレンダー・色・場所・説明・ゲスト・通知）
case class Props(draft: Draft)

case class State(title: RV[String], allDay: RV[Boolean], startDate: RV[String], startTime: RV[String], endDate: RV[String], endTime: RV[String],
                 calendarId: RV[String], color: RV[String], location: RV[String], description: RV[String], repeat: RV[String],
                 cFreq: RV[String], cInterval: RV[Int], cDays: RV[Set[String]], cEnd: RV[String], cUntil: RV[String], cCount: RV[Int],
                 attendees: RV[List[UserBrief]], reminders: RV[List[Int]], meeting: RV[Boolean] = RV(false),
                 // カスタムの毎月・毎年: 日の決め方（same: 開始日と同じ日 / monthday: 日にち / weekday: 曜日）・日にち（-1 は末日）・第何週（0 は毎週）・月・何番目（0 はすべて）
                 cMode: RV[String] = RV("same"), cMonthDays: RV[Set[Int]] = RV(Set.empty), cOrdinals: RV[Set[Int]] = RV(Set.empty),
                 cMonths: RV[Set[Int]] = RV(Set.empty), cSetPos: RV[Int] = RV(0))

/** 曜日のコード（MO〜SU）. */
def code(d: String): String = Recurrence.Days((Cal.dow(d) + 6) % 7)
def nth(d: String): String = s"${Cal.nthInMonth(d)._1}${code(d)}"
def last(d: String): String = s"-1${code(d)}"

/** 繰り返しの選択肢のうち、どれに当たるか（当たらなければカスタム）. */
def keyOf(r: Option[Recurrence], d: String): String = r match {
  case None => "none"
  case Some(x) if x.interval != 1 || x.until.isDefined || x.count.isDefined || x.byMonthDay.nonEmpty || x.byMonth.nonEmpty || x.bySetPos.nonEmpty => "custom"
  case Some(x) if x.freq == Recurrence.Daily && x.byDay.isEmpty => "daily"
  case Some(x) if x.freq == Recurrence.Weekly && (x.byDay.isEmpty || x.byDay == List(code(d))) => "weekly"
  case Some(x) if x.freq == Recurrence.Weekly && x.byDay.toSet == Recurrence.Weekdays.toSet => "weekdays"
  case Some(x) if x.freq == Recurrence.Monthly && x.byDay.isEmpty => "monthlyDay"
  case Some(x) if x.freq == Recurrence.Monthly && x.byDay == List(nth(d)) => "monthlyNth"
  case Some(x) if x.freq == Recurrence.Monthly && x.byDay == List(last(d)) => "monthlyLast"
  case Some(x) if x.freq == Recurrence.Yearly && x.byDay.isEmpty => "yearly"
  case _ => "custom"
}

def initState(props: Props) = {
  val d = props.draft
  val r = d.recurrence
  State(RV(d.title), RV(d.allDay), RV(d.startDate), RV(d.startTime), RV(d.endDate), RV(d.endTime), RV(d.calendarId.toString),
    RV(d.color.getOrElse("")), RV(d.location), RV(d.description), RV(keyOf(r, d.startDate)),
    RV(r.map(_.freq).getOrElse(Recurrence.Weekly)), RV(r.map(_.interval).getOrElse(1)), RV(r.map(_.byDay.map(_.takeRight(2)).toSet).filter(_.nonEmpty).getOrElse(Set(code(d.startDate)))),
    RV(if (r.exists(_.until.isDefined)) "until" else if (r.exists(_.count.isDefined)) "count" else "never"),
    RV(r.flatMap(_.until).getOrElse(Cal.addMonths(d.startDate, 3))), RV(r.flatMap(_.count).getOrElse(10)),
    RV(d.attendees), RV(d.reminders),
    cMode = RV(r.fold("same")(x => if (x.byMonthDay.nonEmpty) "monthday" else if (x.byDay.nonEmpty && x.freq != Recurrence.Weekly) "weekday" else "same")),
    cMonthDays = RV(r.map(_.byMonthDay.toSet).filter(_.nonEmpty).getOrElse(Set(Cal.day(d.startDate)))),
    cOrdinals = RV(r.map(_.byDay.flatMap(Recurrence.parseDay).map(_._1.getOrElse(0)).toSet).filter(_.nonEmpty).getOrElse(Set(Cal.nthInMonth(d.startDate)._1))),
    cMonths = RV(r.map(_.byMonth.toSet).filter(_.nonEmpty).getOrElse(Set(Cal.month(d.startDate)))),
    cSetPos = RV(r.flatMap(_.bySetPos.headOption).getOrElse(0)))
}

def mounted(s: State, props: Props): Unit = {
  // 開始を動かしたら、終わりも同じだけ動かす（長さを保つ）
  watch(s.startDate)((o, n) => if (o.nonEmpty && n.nonEmpty) s.endDate := Cal.addDays(s.endDate.v, Cal.diffDays(o, n)))
  watch(s.startTime)((o, n) => if (o.nonEmpty && n.nonEmpty) {
    val dur = Cal.diffDays(s.startDate.v, s.endDate.v) * 1440 + Cal.parseHm(s.endTime.v) - Cal.parseHm(o)
    val end = Cal.parseHm(n) + math.max(dur, 15)
    s.endDate := Cal.addDays(s.startDate.v, end / 1440)
    s.endTime := Cal.hm(end % 1440)
  })
}

def repeatOptions(s: State) = s.startDate.mapC { d =>
  val w = Cal.WeekNames(Cal.dow(d))
  val (n, isLast) = Cal.nthInMonth(d)
  List("none" -> "繰り返さない", "daily" -> "毎日", "weekly" -> s"毎週 ${w}曜日", "monthlyNth" -> s"毎月 第$n ${w}曜日") ++
    (if (isLast) List("monthlyLast" -> s"毎月 最終 ${w}曜日") else Nil) ++
    List("monthlyDay" -> s"毎月 ${Cal.day(d)}日", "yearly" -> s"毎年 ${Cal.month(d)}月${Cal.day(d)}日", "weekdays" -> "毎週 平日（月〜金）",
      "custom" -> "カスタム…")
}

def recurrence(s: State): Option[Recurrence] = {
  val d = s.startDate.v
  s.repeat.v match {
    case "none"        => None
    case "daily"       => Some(Recurrence(Recurrence.Daily))
    case "weekly"      => Some(Recurrence(Recurrence.Weekly, byDay = List(code(d))))
    case "weekdays"    => Some(Recurrence(Recurrence.Weekly, byDay = Recurrence.Weekdays))
    case "monthlyNth"  => Some(Recurrence(Recurrence.Monthly, byDay = List(nth(d))))
    case "monthlyLast" => Some(Recurrence(Recurrence.Monthly, byDay = List(last(d))))
    case "monthlyDay"  => Some(Recurrence(Recurrence.Monthly))
    case "yearly"      => Some(Recurrence(Recurrence.Yearly))
    case _ =>
      val f     = s.cFreq.v
      val codes = Recurrence.Days.filter(s.cDays.v.contains)
      val mode  = if (f == Recurrence.Monthly || f == Recurrence.Yearly) s.cMode.v else "same"
      val byDay =
        if (f == Recurrence.Weekly) codes
        else if (mode != "weekday") Nil
        else if (s.cOrdinals.v.contains(0)) codes
        else ordinals.map(_._1).filter(s.cOrdinals.v.contains).flatMap(o => codes.map(c => s"$o$c"))
      Some(Recurrence(f, math.max(1, s.cInterval.v), byDay,
        until = Option.when(s.cEnd.v == "until")(s.cUntil.v).filter(_.nonEmpty),
        count = Option.when(s.cEnd.v == "count")(math.max(1, s.cCount.v)),
        byMonthDay = if (mode == "monthday") Recurrence.sortedMonthDays(s.cMonthDays.v.toList) else Nil,
        byMonth = if (f == Recurrence.Yearly) s.cMonths.v.toList.sorted else Nil,
        bySetPos = if (mode == "weekday" && s.cSetPos.v != 0) List(s.cSetPos.v) else Nil))
  }
}

// カスタムの毎月・毎年の選択肢
def customMonthLike(s: State) = s.cFreq.mapC(f => f == Recurrence.Monthly || f == Recurrence.Yearly)
def customYearly(s: State) = s.cFreq.mapC(_ == Recurrence.Yearly)
def modeIs(s: State, m: String) = s.cMode.mapC(_ == m)
val modes: List[(String, String)] = List("same" -> "開始日と同じ日", "monthday" -> "日にちで選ぶ", "weekday" -> "曜日で選ぶ")
val monthDayChoices: List[(Int, String)] = (1 to 31).toList.map(d => d -> d.toString) :+ (-1 -> "末日")
val ordinals: List[(Int, String)] = List(0 -> "毎週", 1 -> "第1", 2 -> "第2", 3 -> "第3", 4 -> "第4", 5 -> "第5", -1 -> "最終")
val setPosChoices: List[(Int, String)] = List(0 -> "すべての日", 1 -> "最初の日", 2 -> "2番目の日", 3 -> "3番目の日", -1 -> "最後の日", -2 -> "最後から2番目の日")
val monthChoices: List[Int] = (1 to 12).toList
/** 選んでいれば外し、なければ足す（1つは残す）. */
def toggleIn[A](rv: RV[Set[A]], x: A): Unit = rv := (if (rv.v.contains(x) && rv.v.size > 1) rv.v - x else rv.v + x)
def chip[A](rv: RV[Set[A]], x: A, base: String = "dbtn") = rv.mapC(xs => if (xs.contains(x)) s"$base on" else base)
/** 「毎週」と「第n」は一緒に選べない（毎週を選んだら第nを外す）. */
def toggleOrdinal(s: State, o: Int): Unit =
  if (o == 0) s.cOrdinals := Set(0)
  else { toggleIn(s.cOrdinals, o); if (s.cOrdinals.v.size > 1) s.cOrdinals := s.cOrdinals.v - 0 }

def isCustom(s: State) = s.repeat.mapC(_ == "custom")
def customWeekly(s: State) = s.cFreq.mapC(_ == Recurrence.Weekly)
def summary(s: State) = computed { recurrence(s).map(_.describe).getOrElse("") }
def dayBtn(s: State, c: String) = s.cDays.mapC(ds => if (ds.contains(c)) "dbtn on" else "dbtn")
def toggleDay(s: State, c: String): Unit =
  s.cDays := (if (s.cDays.v.contains(c) && s.cDays.v.size > 1) s.cDays.v - c else s.cDays.v + c)

val freqOptions: List[(String, String)] = List(Recurrence.Daily -> "日", Recurrence.Weekly -> "週", Recurrence.Monthly -> "か月", Recurrence.Yearly -> "年")
// 通知の候補（何分前）. 今の値が候補になければ（API で入れたものなど）それも出す
val reminderChoices: List[Int] = List(0, 5, 10, 15, 30, 60, 120, 1440, 2880, 10080)
def choicesFor(m: Int): List[(String, String)] = (reminderChoices :+ m).distinct.sorted.map(x => x.toString -> Reminders.label(x))
def canAddReminder(s: State) = s.reminders.mapC(_.size < Reminders.MaxCount)
/** 足すときは、まだ使っていない候補のうち一番近いもの. */
def addReminder(s: State): Unit =
  reminderChoices.find(m => !s.reminders.v.contains(m) && s.reminders.v.forall(_ < m)).orElse(reminderChoices.find(m => !s.reminders.v.contains(m)))
    .foreach(m => s.reminders := s.reminders.v :+ m)
def setReminder(s: State, i: Int, v: String): Unit = v.toIntOption.foreach(m => s.reminders := s.reminders.v.updated(i, m))
def removeReminder(s: State, i: Int): Unit = s.reminders := s.reminders.v.patch(i, Nil, 1)
def reminderRows(s: State) = s.reminders.mapC(_.zipWithIndex)

def calendars(props: Props): List[CalendarDto] = {
  val w = Store.writable
  if (w.exists(_.id == props.draft.calendarId)) w else Store.calendar(props.draft.calendarId).toList ++ w
}

def calColor(s: State) = s.calendarId.mapC(id => Store.calendars.v.find(_.id.toString == id).map(_.displayColor).getOrElse(Palette.Default))

def addGuest(s: State, u: UserBrief): Unit = if (!s.attendees.v.exists(_.id == u.id)) s.attendees := s.attendees.v :+ u
def removeGuest(s: State, u: UserBrief): Unit = s.attendees := s.attendees.v.filterNot(_.id == u.id)
def exclude(s: State) = s.attendees.mapC(as => as.map(_.id) ++ Store.me.v.map(_.id).toList)

// ---------------------------------------------------------------- Web 会議（skaka）

/** skaka との連携が設定されているか（されていなければ何も出さない）. */
def hasSkaka = Store.skaka.mapC(_.isDefined)
def attached(s: State) = s.location.mapC(Store.meetingUrl(_).isDefined)
/** 付けられる（連携していて、まだ付けていない）. */
def canAttach(s: State) = computed(Store.skaka.v.exists(_.connected) && Store.meetingUrl(s.location.v).isEmpty)
def needConnect(s: State) = computed(Store.skaka.v.exists(!_.connected) && Store.meetingUrl(s.location.v).isEmpty)
def connectUrl: String = Api.skakaConnectUrl("/settings")

def heading(props: Props): String = if (props.draft.id.isEmpty) "予定を作成" else "予定を編集"

def draft(s: State, props: Props): Draft = props.draft.copy(
  title = s.title.v.trim, allDay = s.allDay.v, startDate = s.startDate.v, startTime = s.startTime.v, endDate = s.endDate.v,
  endTime = s.endTime.v, calendarId = UUID.fromString(s.calendarId.v), color = Option(s.color.v).filter(_.nonEmpty),
  location = s.location.v.trim, description = s.description.v, recurrence = recurrence(s), attendees = s.attendees.v,
  reminders = Reminders.normalize(s.reminders.v), withMeeting = s.meeting.v && Store.meetingUrl(s.location.v).isEmpty)

def save(s: State, props: Props): Unit = {
  val d = draft(s, props)
  if (d.startDate.isEmpty || d.endDate.isEmpty) Store.toast("日付を入れてください", error = true)
  else if (!d.allDay && Cal.diffDays(d.startDate, d.endDate) * 1440 + Cal.parseHm(d.endTime) <= Cal.parseHm(d.startTime))
    Store.toast("終わりは開始より後にしてください", error = true)
  else if (d.allDay && d.endDate < d.startDate) Store.toast("終わりの日は開始の日より後にしてください", error = true)
  else Store.save(d)(())
}

def remove(props: Props): Unit = {
  val d = props.draft
  d.id.foreach { id =>
    val r = d.toReq
    Store.remove(OccurrenceDto(id, d.calendarId, d.title, d.location, d.allDay, r.start, r.end, d.color, d.wasRecurring,
      d.occurrence.getOrElse(r.start), canEdit = true, invited = false, None, d.reminders, d.attendees.size))
  }
}

def close(): Unit = Store.editor := None
</script>

<template>
  <Modal :onClose="_ => close()" :wide="true">
    <form class="ed" @submit.prevent="save(s, props)">
      <div class="row hd">
        <h2 class="grow">{{ heading(props) }}</h2>
        <button type="button" class="ghost" @click="close()">×</button>
      </div>
      <input type="text" class="title" v-model="s.title" placeholder="タイトルを追加" autofocus>
      <div class="when">
        <div class="row">
          <input type="date" v-model="s.startDate" required>
          <input v-if="s.allDay.mapC(!_)" type="time" v-model="s.startTime" step="900" required>
          <span class="muted">–</span>
          <input v-if="s.allDay.mapC(!_)" type="time" v-model="s.endTime" step="900" required>
          <input type="date" v-model="s.endDate" required>
        </div>
        <div class="row opts">
          <label class="row small"><input type="checkbox" v-model="s.allDay"> 終日</label>
          <select v-model="s.repeat">
            <option v-for="o in repeatOptions(s)" :value="o._1">{{ o._2 }}</option>
          </select>
          <span class="muted small">{{ summary(s) }}</span>
        </div>
        <div v-if="isCustom(s)" class="custom">
          <div class="row">
            <span class="small">繰り返す間隔</span>
            <input type="number" min="1" max="999" v-model="s.cInterval" class="num">
            <select v-model="s.cFreq"><option v-for="f in freqOptions" :value="f._1">{{ f._2 }}</option></select>
            <span class="small">ごと</span>
          </div>
          <div v-if="customWeekly(s)" class="row days">
            <button v-for="c in Recurrence.Days" type="button" :class="dayBtn(s, c)" @click="toggleDay(s, c)">{{ Recurrence.DayNames(c) }}</button>
          </div>
          <template v-if="customMonthLike(s)">
            <div v-if="customYearly(s)" class="row days">
              <span class="small">月</span>
              <button v-for="m in monthChoices" type="button" :class='chip(s.cMonths, m, "dbtn wide")' @click="toggleIn(s.cMonths, m)">{{ m }}月</button>
            </div>
            <div class="row">
              <label v-for="m in modes" class="row small"><input type="radio" name="cmode" :checked="modeIs(s, m._1)" @change="s.cMode := m._1"> {{ m._2 }}</label>
            </div>
            <div v-if='modeIs(s, "monthday")' class="row days">
              <button v-for="d in monthDayChoices" type="button" :class='chip(s.cMonthDays, d._1, if (d._1 < 0) "dbtn wide" else "dbtn")'
                @click="toggleIn(s.cMonthDays, d._1)">{{ d._2 }}</button>
            </div>
            <template v-if='modeIs(s, "weekday")'>
              <div class="row days">
                <button v-for="o in ordinals" type="button" :class='chip(s.cOrdinals, o._1, "dbtn wide")' @click="toggleOrdinal(s, o._1)">{{ o._2 }}</button>
              </div>
              <div class="row days">
                <button v-for="c in Recurrence.Days" type="button" :class="dayBtn(s, c)" @click="toggleDay(s, c)">{{ Recurrence.DayNames(c) }}</button>
              </div>
              <div class="row">
                <span class="small">そのうち</span>
                <select :value="s.cSetPos.v.toString" @change="s.cSetPos := Dom.value(event).toInt">
                  <option v-for="p in setPosChoices" :value="p._1.toString">{{ p._2 }}</option>
                </select>
              </div>
            </template>
          </template>
          <div class="row">
            <span class="small">終わり</span>
            <label class="row small"><input type="radio" name="cend" :checked='s.cEnd.mapC(_ == "never")' @change='s.cEnd := "never"'> なし</label>
            <label class="row small"><input type="radio" name="cend" :checked='s.cEnd.mapC(_ == "until")' @change='s.cEnd := "until"'> 日付
              <input type="date" v-model="s.cUntil"></label>
            <label class="row small"><input type="radio" name="cend" :checked='s.cEnd.mapC(_ == "count")' @change='s.cEnd := "count"'>
              <input type="number" min="1" max="9999" v-model="s.cCount" class="num"> 回</label>
          </div>
        </div>
      </div>
      <div class="grid2">
        <div>
          <label class="field"><span>場所</span><input type="text" v-model="s.location" placeholder="場所を追加"></label>
          <div v-if="hasSkaka" class="meeting small">
            <span v-if="attached(s)"><span class="ico ico-video"></span> Web 会議（{{ AppNames.of("skaka") }}）を付けてあります</span>
            <label v-if="canAttach(s)" class="row"><input type="checkbox" v-model="s.meeting"> <span class="ico ico-video"></span> Web 会議（{{ AppNames.of("skaka") }}）を付ける</label>
            <span v-if="needConnect(s)" class="muted"><span class="ico ico-video"></span> 設定の画面で {{ AppNames.of("skaka") }} と連携すると、Web 会議を付けられます</span>
          </div>
          <label class="field"><span>説明</span><textarea v-model="s.description" rows="5" placeholder="説明を追加"></textarea></label>
          <div class="field"><span>通知</span>
            <div v-for="r in reminderRows(s)" :key="r._2" class="rem">
              <select @change="setReminder(s, r._2, Dom.value(event))">
                <option v-for="c in choicesFor(r._1)" :value="c._1" :selected="c._1 == r._1.toString">{{ c._2 }}</option></select>
              <button type="button" class="ghost small" title="この通知を削除" @click="removeReminder(s, r._2)">×</button>
            </div>
            <button v-if="canAddReminder(s)" type="button" class="ghost small add-rem" @click="addReminder(s)">＋ 通知を追加</button>
          </div>
        </div>
        <div>
          <label class="field"><span>カレンダー</span>
            <select v-model="s.calendarId"><option v-for="c in calendars(props)" :value="c.id.toString">{{ c.name }}</option></select></label>
          <div class="field"><span>色</span>
            <ColorSwatches :value="s.color.v" :onPick="c => s.color := c" :auto="Some(calColor(s).v)" label="カレンダーの色" />
          </div>
          <div class="field"><span>ゲスト</span>
            <UserPicker :onPick="u => addGuest(s, u)" :exclude="exclude(s).v" />
            <div v-for="u in s.attendees" :key="u.id" class="guest">
              <Avatar :user="Some(u)" :size="24" /><span class="grow">{{ u.name }}</span>
              <button type="button" class="ghost small" title="外す" @click="removeGuest(s, u)">×</button>
            </div>
          </div>
        </div>
      </div>
      <div class="actions">
        <button v-if="props.draft.id.isDefined" type="button" class="danger" @click="remove(props)">削除</button>
        <span class="grow"></span>
        <button type="button" @click="close()">キャンセル</button>
        <button type="submit" class="primary">保存</button>
      </div>
    </form>
  </Modal>
</template>

<style scoped>
.hd h2 { margin: 0; }
.meeting { margin: -.4rem 0 .8rem; }
.meeting label { gap: .4rem; }
.rem { display: flex; align-items: center; gap: .3rem; margin-bottom: .3rem; }
.rem select { flex: 1; }
.add-rem { align-self: flex-start; }
.title { width: 100%; font-size: 1.35rem; border: none; border-bottom: 2px solid var(--line); border-radius: 0; padding: .4rem .2rem; margin: .6rem 0 .8rem;
  background: transparent; }
.title:focus { box-shadow: none; border-bottom-color: var(--accent); }
.when { display: flex; flex-direction: column; gap: .5rem; margin-bottom: 1rem; }
.opts { gap: .8rem; }
.custom { border: 1px solid var(--line); border-radius: 10px; padding: .6rem .8rem; display: flex; flex-direction: column; gap: .5rem; background: var(--side); }
.num { width: 5rem; }
.dbtn { width: 2rem; height: 2rem; border-radius: 999px; padding: 0; justify-content: center; font-size: .8rem; }
.dbtn.on { background: var(--accent); color: #fff; border-color: var(--accent); }
.dbtn.wide { width: auto; min-width: 2rem; padding: 0 .6rem; }
.grid2 { display: grid; grid-template-columns: 1fr 1fr; gap: 1.2rem; }
@media (max-width: 720px) { .grid2 { grid-template-columns: 1fr; gap: 0; } }
.guest { display: flex; align-items: center; gap: .5rem; padding: .3rem .2rem; }
.actions .grow { flex: 1; }
</style>
