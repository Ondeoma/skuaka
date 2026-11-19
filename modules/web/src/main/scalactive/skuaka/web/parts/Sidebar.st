<script lang="scala">
import skuaka.web.*
import skuaka.shared.*

// サイドバー: 作成のボタン・ミニカレンダー・カレンダーの一覧（表示の切り替え・設定）
def mine = Store.calendars.mapC(_.filter(_.role == CalRole.Owner))
def others = Store.calendars.mapC(_.filterNot(_.role == CalRole.Owner))
def noOthers = others.mapC(_.isEmpty)

def tip(c: CalendarDto): String =
  s"${c.owner.name} さんのカレンダー（${if (c.viaTeam) "チームに公開" else "共有"}: ${CalRole.label(c.role)}）"

def boxStyle(c: CalendarDto): String = s"accent-color: ${c.displayColor}"

def toggle(c: CalendarDto): Unit = {
  // 先に画面を切り替え、保存に失敗したら読み直して戻す
  Store.calendars := Store.calendars.v.map(x => if (x.id == c.id) x.copy(visible = !c.visible) else x)
  Store.orUndo(Api.calendarPrefs(c.id, CalendarPrefReq(None, Some(!c.visible))))(Store.loadCalendars())
}

/** URL を購読したカレンダーの説明（読めなかったときはその理由）. */
def subTip(c: CalendarDto): String =
  c.subscription.fold("")(x => "iCalendar の URL を購読（読み取りだけ）" + x.error.fold("")(e => s"\n読めませんでした: $e"))
def subMark(c: CalendarDto): String = c.subscription.fold("")(x => if (x.error.isDefined) "⚠" else "⟳")

def create(): Unit = {
  if (org.scalajs.dom.window.innerWidth < 720) Store.sidebar := false
  Store.create(Store.date.v)
}
</script>

<template>
  <div class="sb">
    <button class="create" @click="create()"><span class="plus">＋</span> 作成</button>
    <MiniMonth />
    <div class="group">
      <div class="gh"><span>マイカレンダー</span><button class="ghost small" title="カレンダーを追加（新しいカレンダー・URL で購読・ファイルから取り込む）" @click="Store.addCalendar := true">＋</button></div>
      <div v-for="c in mine" :key="c.id" class="cal">
        <label class="grow" :title="subTip(c)"><input type="checkbox" :checked="c.visible" :style="boxStyle(c)" @change="toggle(c)"><span class="name">{{ c.name }}</span>
          <span v-if="c.subscription.isDefined" class="sub muted small">{{ subMark(c) }}</span></label>
        <button class="ghost small more" title="設定と共有" @click="Store.calSettings := Some(c.id)">⋮</button>
      </div>
    </div>
    <div class="group">
      <div class="gh"><span>他のカレンダー</span><button class="ghost small" title="チームのカレンダーを追加" @click="Store.teamPicker := true">＋</button></div>
      <button v-if="noOthers" class="ghost small hint" @click="Store.teamPicker := true">チームの人のカレンダーを追加</button>
      <div v-for="c in others" :key="c.id" class="cal">
        <label class="grow" :title="tip(c)">
          <input type="checkbox" :checked="c.visible" :style="boxStyle(c)" @change="toggle(c)"><span class="name">{{ c.name }}</span></label>
        <button class="ghost small more" title="設定" @click="Store.calSettings := Some(c.id)">⋮</button>
      </div>
    </div>
  </div>
</template>

<style scoped>
.sb { padding: .8rem .6rem 2rem; }
.create { border-radius: 16px; padding: .7rem 1.3rem .7rem 1rem; font-size: .95rem; font-weight: 600; box-shadow: 0 1px 3px rgba(0,0,0,.18), 0 2px 8px rgba(0,0,0,.08);
  background: var(--surface); margin: 0 0 1rem .2rem; border: none; }
.create:hover { box-shadow: 0 2px 6px rgba(0,0,0,.22), 0 4px 14px rgba(0,0,0,.12); background: var(--hover); }
.plus { font-size: 1.4rem; color: var(--accent); line-height: 1; }
.group { margin-top: 1rem; }
.gh { display: flex; align-items: center; justify-content: space-between; font-size: .85rem; font-weight: 600; padding: .2rem .4rem; }
.cal { display: flex; align-items: center; border-radius: 6px; padding: 0 .2rem; }
.cal:hover { background: var(--hover); }
.cal label { display: flex; align-items: center; gap: .55rem; padding: .3rem .2rem; cursor: pointer; min-width: 0; }
.cal input { width: 16px; height: 16px; margin: 0; flex: none; }
.sub { flex: none; }
.name { white-space: nowrap; overflow: hidden; text-overflow: ellipsis; font-size: .88rem; }
.hint { color: var(--muted); padding: .3rem .4rem; }
.more { opacity: 0; }
.cal:hover .more { opacity: 1; }
@media (hover: none) { .more { opacity: 1; } }
</style>
