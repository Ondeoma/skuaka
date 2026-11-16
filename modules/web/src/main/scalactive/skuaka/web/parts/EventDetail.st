<script lang="scala">
import skuaka.web.*
import skuaka.shared.*

// 予定の詳細（日時・繰り返し・場所・説明・ゲストと返事・カレンダー）. 編集・削除・招待への返事
case class Props(occ: OccurrenceDto)

case class State(ev: RV[Option[EventDto]] = RV(None))

def initState() = State()

def mounted(s: State, props: Props): Unit = Store.report(Store.fetchEvent(props.occ.eventId))(e => s.ev := Some(e))

def o(props: Props): OccurrenceDto = props.occ

def when(props: Props): String = Store.timeText(props.occ, withDate = true)

def repeatText(s: State) = s.ev.mapC(_.flatMap(_.recurrence).map(_.describe).getOrElse(""))
def desc(s: State) = s.ev.mapC(_.map(_.description).getOrElse(""))
def meeting(props: Props): Option[String] = Store.meetingUrl(props.occ.location)
def reminderText(s: State) = s.ev.mapC(_.map(_.reminders).filter(_.nonEmpty).map(Reminders.text).getOrElse(""))
def guests(s: State) = s.ev.mapC(_.map(_.attendees).getOrElse(Nil))
// 公開リンクで見ているときは主催者を出さない（持ち主のカレンダーなので）
def organizer(s: State) = computed(if (Store.guest.v.isDefined) None else s.ev.v.map(_.organizer))
def calName(props: Props): String = Store.calendar(props.occ.calendarId).map(_.name).getOrElse("招待された予定")
def colorStyle(props: Props): String = s"background: ${Store.color(props.occ)}"
def hasGuests(s: State) = guests(s).mapC(_.nonEmpty)
def guestSummary(s: State) = guests(s).mapC { as =>
  val yes = as.count(_.response == Rsvp.Accepted)
  val no  = as.count(_.response == Rsvp.Declined)
  val wait = as.count(a => a.response == Rsvp.NeedsAction || a.response == Rsvp.Tentative)
  s"ゲスト ${as.size} 人（参加 $yes・不参加 $no・未定 $wait）"
}
def mark(r: String): String = r match {
  case Rsvp.Accepted  => "✓"
  case Rsvp.Declined  => "✕"
  case Rsvp.Tentative => "?"
  case _              => "…"
}
def markClass(r: String): String = "mk " + r
def answerClass(props: Props, r: String): String = if (props.occ.myResponse.contains(r)) "small primary" else "small"

def close(): Unit = Store.detail := None

def leave(props: Props): Unit =
  Dialog.confirm("この予定を自分のカレンダーから外しますか？（ゲストの一覧からも外れます）", "外す") {
    Store.report(Api.leaveEvent(props.occ.eventId)) { _ => close(); Store.changed() }
  }
</script>

<template>
  <Modal :onClose="_ => close()">
    <div class="tools">
      <button v-if="props.occ.canEdit" class="ghost" title="編集" @click="Store.edit(props.occ)">✎ 編集</button>
      <button v-if="props.occ.canEdit" class="ghost" title="削除" @click="Store.remove(props.occ)">🗑 削除</button>
      <button class="ghost" title="閉じる" @click="close()">×</button>
    </div>
    <div class="line">
      <span class="sq" :style="colorStyle(props)"></span>
      <div>
        <h2 class="t">{{ props.occ.title }}</h2>
        <div>{{ when(props) }}</div>
        <div class="muted small">{{ repeatText(s) }}</div>
        <div v-if="props.occ.busy" class="muted small">予定あり・なしだけが公開されているため、内容は表示されません</div>
      </div>
    </div>
    <div v-if="props.occ.location.nonEmpty" class="line"><span class="ic">📍</span><span class="loc">{{ props.occ.location }}</span></div>
    <div v-if="meeting(props).isDefined" class="line"><span class="ic">🎥</span>
      <a class="btn primary" :href='meeting(props).getOrElse("")' target="_blank" rel="noopener">Web 会議に参加</a></div>
    <div v-if="hasGuests(s)" class="line">
      <span class="ic">👥</span>
      <div class="grow">
        <div>{{ guestSummary(s) }}</div>
        <div v-for="a in guests(s)" :key="a.user.id" class="guest">
          <Avatar :user="Some(a.user)" :size="24" /><span class="grow">{{ a.user.name }}</span>
          <span :class="markClass(a.response)" :title="Rsvp.label(a.response)">{{ mark(a.response) }}</span>
        </div>
      </div>
    </div>
    <div v-if="reminderText(s).mapC(_.nonEmpty)" class="line"><span class="ic">🔔</span><span>{{ reminderText(s) }}</span></div>
    <div v-if="desc(s).mapC(_.nonEmpty)" class="line"><span class="ic">≡</span><div class="desc">{{ desc(s) }}</div></div>
    <div class="line"><span class="ic">📅</span>
      <span>{{ calName(props) }}<template v-for="u in organizer(s).v.toList"><span class="muted small">　主催: {{ u.name }}</span></template></span>
    </div>
    <div v-if="props.occ.myResponse.isDefined" class="rsvp">
      <span class="small">参加しますか？</span>
      <button :class="answerClass(props, Rsvp.Accepted)" @click="Store.rsvp(props.occ, Rsvp.Accepted)">はい</button>
      <button :class="answerClass(props, Rsvp.Declined)" @click="Store.rsvp(props.occ, Rsvp.Declined)">いいえ</button>
      <button :class="answerClass(props, Rsvp.Tentative)" @click="Store.rsvp(props.occ, Rsvp.Tentative)">未定</button>
      <span class="grow"></span>
      <button v-if="props.occ.invited" class="ghost small" @click="leave(props)">カレンダーから外す</button>
    </div>
  </Modal>
</template>

<style scoped>
.tools { display: flex; justify-content: flex-end; gap: .2rem; margin: -.4rem -.6rem .4rem 0; }
.line { display: flex; gap: 1rem; align-items: flex-start; margin-bottom: .9rem; }
.sq { width: 16px; height: 16px; border-radius: 4px; margin-top: .45rem; flex: none; }
.ic { width: 16px; text-align: center; flex: none; }
.t { margin: 0 0 .2rem; font-size: 1.4rem; font-weight: 500; word-break: break-word; }
.desc { white-space: pre-wrap; word-break: break-word; }
.loc { word-break: break-all; }
.guest { display: flex; align-items: center; gap: .5rem; padding: .25rem 0; }
.mk { font-weight: 700; width: 1.2rem; text-align: center; }
.mk.accepted { color: var(--ok); }
.mk.declined { color: var(--danger); }
.mk.tentative, .mk.needs_action { color: var(--muted); }
.rsvp { display: flex; gap: .4rem; align-items: center; border-top: 1px solid var(--line); padding-top: .8rem; flex-wrap: wrap; }
</style>
