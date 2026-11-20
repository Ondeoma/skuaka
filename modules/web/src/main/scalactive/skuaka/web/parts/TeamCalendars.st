<script lang="scala">
import skuaka.web.*
import skuaka.shared.*

// チームの人が公開しているカレンダーを探して、自分の一覧（他のカレンダー）に加える
case class State(q: RV[String], found: RV[List[TeamCalendarDto]], loaded: RV[Boolean], search: () => Unit, typed: DebouncedFn[Unit])

def initState() = {
  val q = RV("")
  val found = RV(List.empty[TeamCalendarDto])
  val loaded = RV(false)
  val latest = useLatest()
  // 打ち終えて 250ms たってから探す（古い応答は捨てる）
  def search(): Unit = Toasts.report(latest(Api.teamCalendars(q.v.trim))) { xs => found := xs; loaded := true }
  State(q, found, loaded, () => search(), useDebounce[Unit](250)(_ => search()))
}

def mounted(s: State): Unit = s.search()

def typed(s: State, v: String): Unit = {
  s.q := v
  s.typed(())
}

def empty(s: State) = computed(s.loaded.v && s.found.v.isEmpty)
def boxStyle(c: TeamCalendarDto): String = s"background: ${c.color}"
def detail(c: TeamCalendarDto): String = (if (c.primary) "メイン・" else "") + TeamAccess.label(c.access)

def add(s: State, c: TeamCalendarDto): Unit =
  Toasts.report(Api.addTeamCalendar(c.id)) { _ =>
    s.found := s.found.v.map(x => if (x.id == c.id) x.copy(added = true) else x)
    Toasts.show(s"${c.owner.name} さんの「${c.name}」を追加しました")
    Toasts.report(Store.loadCalendars())(_ => Store.load())
  }

def close(): Unit = Store.teamPicker := false
</script>

<template>
  <Modal :onClose="_ => close()" :wide="true">
    <div class="row hd"><h2 class="grow">チームのカレンダーを追加</h2><button class="ghost" @click="close()">×</button></div>
    <p class="muted small">チームの人が公開しているカレンダーです。追加すると、サイドバーの「他のカレンダー」に入り、予定を重ねて表示できます。</p>
    <input type="search" class="q" placeholder="名前・メールアドレス・カレンダーの名前で絞る" :value="s.q.v" @input="typed(s, Dom.value(event))" v-focus>
    <div class="list">
      <div v-for="c in s.found" :key="c.id" class="item">
        <Avatar :user="Some(c.owner.person)" :size="32" />
        <div class="grow">
          <div><span class="sq" :style="boxStyle(c)"></span>{{ c.owner.name }}<span class="muted">　{{ c.name }}</span></div>
          <div class="muted small">{{ detail(c) }}</div>
        </div>
        <span v-if="c.added" class="muted small">追加済み</span>
        <button v-else class="small" @click="add(s, c)">追加</button>
      </div>
      <p v-if="empty(s)" class="muted">公開されているカレンダーは見つかりませんでした。</p>
    </div>
    <div class="actions"><span class="grow"></span><button @click="close()">閉じる</button></div>
  </Modal>
</template>

<style scoped>
.hd h2 { margin: 0; }
.q { width: 100%; margin: .4rem 0 .6rem; }
.list { max-height: 55vh; overflow-y: auto; }
.item { display: flex; align-items: center; gap: .7rem; padding: .5rem 0; border-bottom: 1px solid var(--line); }
.sq { display: inline-block; width: 10px; height: 10px; border-radius: 3px; margin-right: .4rem; }
.actions .grow { flex: 1; }
</style>
