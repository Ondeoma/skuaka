<script lang="scala">
import skuaka.web.*
import skuaka.shared.*
import org.scalajs.dom

// 予定の検索の結果（タイトル・場所・説明）. クリックするとその日を開く.
case class State(q: RV[String] = RV(""), results: RV[Option[List[OccurrenceDto]]] = RV(None))

def initState() = State()

def query(): String = Router.queryParam("q").v.getOrElse("")

def run(s: State): Unit = {
  s.q := query()
  s.results := None
  if (s.q.v.trim.nonEmpty) Toasts.report(Api.search(s.q.v.trim))(rs => s.results := Some(rs))
  else s.results := Some(Nil)
}

def mounted(s: State): Unit = {
  pageTitle("検索")
  run(s)
  watch(Router.href)((_, _) => if (dom.window.location.pathname == "/search") run(s))
}

def dateOf(o: OccurrenceDto): String = if (o.allDay) o.start else Cal.dateOf(o.start)

def open(o: OccurrenceDto): Unit = {
  Store.date := dateOf(o)
  Store.setView("day")
  Router.go("/")
  Store.detail := Some(o)
}

def list = (s: State) => s.results.mapC(_.getOrElse(Nil))
def none(s: State) = s.results.mapC(_.exists(_.isEmpty))
def loading(s: State) = s.results.mapC(_.isEmpty)
</script>

<template>
  <div class="page narrow">
    <h1>「{{ s.q }}」の検索結果</h1>
    <p v-if="loading(s)" class="muted">探しています…</p>
    <p v-if="none(s)" class="empty">見つかりませんでした</p>
    <div v-for="o in list(s)" class="res" @click="open(o)">
      <span class="d">{{ Cal.longLabel(dateOf(o)) }}</span>
      <span class="dot" :style="Store.dotStyle(o)"></span>
      <span class="tm muted small">{{ Store.timeText(o) }}</span>
      <span class="grow ti">{{ o.title }}<span v-if="o.location.nonEmpty" class="muted small">　{{ o.location }}</span></span>
      <span v-if="o.recurring" class="muted small" title="繰り返しの予定（これから先の最初の回）">↻</span>
    </div>
  </div>
</template>

<style scoped>
.res { display: flex; gap: .8rem; align-items: center; padding: .6rem .5rem; border-bottom: 1px solid var(--line); cursor: pointer; border-radius: 6px; }
.res:hover { background: var(--hover); }
.d { width: 150px; flex: none; font-size: .86rem; }
.tm { width: 110px; flex: none; }
.ti { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
@media (max-width: 720px) { .d { width: 110px; font-size: .78rem; } .tm { display: none; } }
</style>
