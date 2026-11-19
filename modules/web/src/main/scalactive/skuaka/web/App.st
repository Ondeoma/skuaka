<script lang="scala">
import io.github.ondeoma.scalactive.components.routes.RouterComponent
import org.scalajs.dom
import skuaka.shared.*
import skuaka.web.parts.*
import skuaka.web.views.*

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js

// 画面全体の枠: ヘッダー（今日・前後・表示の切り替え・検索・メニュー）、サイドバー（ミニカレンダー・カレンダーの一覧）、ルートごとの画面
case class State(ready: RV[Boolean] = RV(false), menu: RV[Boolean] = RV(false), q: RV[String] = RV(""))

def initState() = State()

val routes: PathPatternRoutes = List(
  ("/", _ => CalendarView()),
  ("/p/:token", _ => CalendarView()),
  ("/login", _ => LoginView()),
  ("/search", _ => SearchView()),
  ("/settings", _ => SettingsView()),
)

def mounted(s: State): Unit = {
  Fallback.install(routes)
  Api.clientConfig().foreach(c => Store.config := Some(c))
  val path = dom.window.location.pathname
  if (path.startsWith("/login")) s.ready := true
  // 公開リンク（ログインしない人）: そのカレンダーを閲覧だけ
  else if (path.startsWith("/p/")) Store.report(Store.openGuest(js.URIUtils.decodeURIComponent(path.stripPrefix("/p/")))) { _ =>
    Store.load()
    s.ready := true
  }
  else Store.report(Api.me()) { me =>
    Store.me := Some(me)
    AppNames.set(me.appNames)
    Observe.setUser(me.id.toString)
    Store.report(Store.loadCalendars()) { _ =>
      Store.start()
      Store.load()
      s.ready := true
    }
  }
}

def search(s: State): Unit = if (s.q.v.trim.nonEmpty) Router.go("/search?q=" + Api.enc(s.q.v.trim))

def logout(): Unit = Store.report(Api.logout())(url => dom.window.location.href = url)

def onCalendar = Router.href.mapC(h => new dom.URL(h).pathname == "/")
def loggedIn = Store.me.mapC(_.isDefined)
/** カレンダーを見ている（ログインしたか、公開リンク）. */
def browsing = computed(Store.me.v.isDefined || Store.guest.v.isDefined)
def guestText = Store.guest.mapC(_.map((_, c) => s"${c.owner.name} さんの「${c.name}」（${TeamAccess.label(c.access)}・閲覧のみ）").getOrElse(""))
def isGuest = Store.guest.mapC(_.isDefined)
def homeLink = computed(Store.home)
def meBrief = Store.me.mapC(_.map(_.brief))
def userName = Store.me.mapC(_.map(_.name).getOrElse(""))
def userEmail = Store.me.mapC(_.flatMap(_.email).getOrElse(""))
def showSide = computed { Store.sidebar.v && Store.me.v.isDefined }
def setView(v: String): Unit = { Store.setView(v); Store.goHome() }
def viewClass(v: String) = Store.view.mapC(c => if (c == v) "seg active" else "seg")
def editors = Store.editor.mapC(_.toList)
def details = Store.detail.mapC(_.toList)
def settings = Store.calSettings.mapC(_.toList)
def loadingClass = Store.loading.mapC(l => if (l) "progress on" else "progress")
</script>

<template>
  <header class="top">
    <button v-if="loggedIn" class="ghost icon" title="メニュー" @click="Store.sidebar := !Store.sidebar.v">☰</button>
    <RouterLink :to="homeLink.v" class="logo hide-sp"><span class="brand"><img :src='AppNames.logo("/logo.svg").v' alt="" height="34" class="brand-logo"><span class="hide-sp">{{ AppNames.self }}</span></span></RouterLink>
    <template v-if="browsing">
      <button class="today" @click="Store.date := Cal.today(); Store.goHome()" title="今日（t）">今日</button>
      <button class="ghost round" title="前（k）" @click="Store.step(-1)">‹</button>
      <button class="ghost round" title="次（j）" @click="Store.step(1)">›</button>
      <h1 class="title">{{ Store.title }}</h1>
    </template>
    <div class="spacer"></div>
    <form v-if="loggedIn" class="search hide-sp" @submit.prevent="search(s)">
      <input type="search" v-model="s.q" placeholder="予定を検索（/）">
    </form>
    <span v-if="isGuest" class="guest muted small hide-sp">{{ guestText }}</span>
    <div v-if="browsing" class="segs hide-sp">
      <button v-for="v in Store.views" :class="viewClass(v._1)" @click="setView(v._1)">{{ v._2 }}</button>
    </div>
    <select v-if="browsing" class="only-sp viewsel" :value="Store.view" @change="setView(Dom.value(event))">
      <option v-for="v in Store.views" :value="v._1">{{ v._2 }}</option>
    </select>
    <div v-if="loggedIn" class="me">
      <button class="ghost me-btn" @click="s.menu := !s.menu.v"><Avatar :user="meBrief.v" :size="30" /></button>
      <div v-if="s.menu" class="menu-bg" @click="s.menu := false"></div>
      <div v-if="s.menu" class="menu" @click="s.menu := false">
        <div class="who"><b>{{ userName }}</b><span class="muted small">{{ userEmail }}</span></div>
        <RouterLink to="/settings">設定</RouterLink>
        <a href="#" @click.prevent="Keys.help := true">キーボードの操作</a>
        <a href="#" @click.prevent="logout()">ログアウト</a>
      </div>
    </div>
    <div :class="loadingClass"></div>
  </header>
  <div class="shell">
    <aside v-if="showSide" class="side"><Sidebar /></aside>
    <main class="main">
      <RouterComponent v-if="s.ready" :routes="routes" />
    </main>
  </div>
  <EventEditor v-for="d in editors" :key="d" :draft="d" />
  <EventDetail v-for="o in details" :key="o" :occ="o" />
  <CalendarSettings v-for="c in settings" :key="c" :calId="c" />
  <TeamCalendars v-if="Store.teamPicker" />
  <AddCalendar v-if="Store.addCalendar" />
  <Modal v-if="Keys.help" :onClose="_ => Keys.help := false">
    <h2>キーボードの操作</h2>
    <table class="keys"><tr v-for="k in Keys.list"><td><kbd>{{ k._1 }}</kbd></td><td>{{ k._2 }}</td></tr></table>
    <div class="actions"><button class="primary" @click="Keys.help := false">閉じる</button></div>
  </Modal>
  <DialogHost />
  <div class="toasts">
    <div v-for="t in Store.toasts" :key="t.id" :class='if (t.error) "toast error" else "toast"'>
      <span class="grow">{{ t.text }}</span>
      <button @click="Store.dismiss(t.id)">×</button>
    </div>
  </div>
</template>

<style scoped>
.guest { white-space: nowrap; overflow: hidden; text-overflow: ellipsis; min-width: 0; flex: 0 1 auto; margin-right: .4rem; }
.top > .title { flex-shrink: 0; }
.top { position: sticky; top: 0; z-index: 100; height: var(--header); display: flex; align-items: center; gap: .5rem; padding: 0 .8rem;
  background: var(--bg); border-bottom: 1px solid var(--line); }
.brand { font-weight: 700; font-size: 1.3rem; color: var(--fg); letter-spacing: .01em; display: inline-flex; align-items: center; gap: .5rem;
  white-space: nowrap; margin-right: 1.2rem; }
.brand img { display: block; width: auto; max-width: 140px; }
.icon { font-size: 1.2rem; padding: .3rem .55rem; }
.today { border-radius: 999px; padding: .35rem 1rem; }
.round { font-size: 1.4rem; line-height: 1; padding: .1rem .55rem; border-radius: 999px; }
.title { font-size: 1.3rem; font-weight: 500; margin: 0 0 0 .4rem; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.spacer { flex: 1; }
.search input { width: 240px; border-radius: 999px; background: var(--side); }
.me { position: relative; }
.me-btn { padding: .15rem; border-radius: 999px; }
.menu { position: absolute; right: 0; top: 2.8rem; background: var(--surface); border: 1px solid var(--line); border-radius: 10px; box-shadow: var(--shadow);
  min-width: 220px; padding: .3rem; display: flex; flex-direction: column; z-index: 200; }
.menu-bg { position: fixed; inset: 0; z-index: 199; }
.who { display: flex; flex-direction: column; padding: .5rem .7rem; border-bottom: 1px solid var(--line); margin-bottom: .3rem; }
.progress { position: absolute; left: 0; bottom: -1px; height: 2px; width: 0; background: var(--accent); opacity: 0; transition: opacity .2s; }
.progress.on { width: 100%; opacity: .7; animation: prog 1.2s ease-in-out infinite; }
@keyframes prog { from { transform: scaleX(0); transform-origin: left; } to { transform: scaleX(1); transform-origin: left; } }
.shell { display: flex; height: calc(100vh - var(--header)); height: calc(100dvh - var(--header)); }
.side { width: 256px; flex: none; overflow-y: auto; border-right: 1px solid var(--line); background: var(--bg); }
.main { flex: 1; min-width: 0; overflow: auto; display: flex; flex-direction: column; }
.keys td { padding: .25rem .8rem .25rem 0; }
@media (max-width: 720px) {
  .top { gap: .25rem; padding: 0 .4rem; }
  .top > .title { flex: 1 1 auto; min-width: 0; margin: 0; }
  .spacer { display: none; }
  .viewsel { width: auto; max-width: 6.5em; padding: .25rem .2rem; font-size: .82rem; }
  .round { padding: .1rem .35rem; }
  .side { position: fixed; z-index: 90; top: var(--header); bottom: 0; left: 0; box-shadow: var(--shadow); }
  .title { font-size: 1rem; }
  .today { padding: .3rem .6rem; }
  .brand { margin-right: 0; }
}
</style>
