<script lang="scala">
import skuaka.web.*
import skuaka.shared.*
import skuaka.web.parts.*

import scala.scalajs.js

// 設定: 名前・タイムゾーン（サーバー）、週の始まり・土日・画面の色・通知（この端末）
case class State(name: RV[String] = RV(Store.me.v.map(_.name).getOrElse("")),
                 tz: RV[String] = RV(Store.me.v.map(_.timezone).getOrElse(Cal.timezone())),
                 weekStart: RV[String] = RV(Store.weekStart.v.toString),
                 weekends: RV[Boolean] = RV(Store.weekends.v),
                 theme: RV[String] = RV(Theme.current.v),
                 perm: RV[String] = RV(permission()))

def initState() = State()

def permission(): String = scala.util.Try {
  val n = js.Dynamic.global.Notification
  if (js.isUndefined(n)) "unsupported" else n.permission.asInstanceOf[String]
}.getOrElse("unsupported")

def mounted(s: State): Unit = {
  pageTitle("設定")
  watch(s.weekStart)((_, v) => { Store.weekStart := v.toIntOption.getOrElse(0); Prefs.set("weekStart", v) })
  watch(s.weekends)((_, v) => { Store.weekends := v; Prefs.set("weekends", v.toString) })
  watch(s.theme)((_, v) => Theme.set(v))
}

def save(s: State): Unit =
  Store.report(Api.updateMe(UpdateMeReq(Some(s.name.v.trim), Some(s.tz.v)))) { me =>
    Store.me := Some(me)
    Store.toast("保存しました")
  }

def askPermission(s: State): Unit =
  js.Dynamic.global.Notification.requestPermission().asInstanceOf[js.Promise[String]].`then`[Unit]((p: String) => s.perm := p)

def permText(s: State) = s.perm.mapC {
  case "granted"     => "許可されています。予定の通知をデスクトップにも出します。"
  case "denied"      => "ブラウザで拒否されています。ブラウザのサイトの設定から許可してください。"
  case "unsupported" => "このブラウザはデスクトップの通知に対応していません。"
  case _             => "まだ許可されていません。"
}
def canAsk(s: State) = s.perm.mapC(_ == "default")
def zones: List[String] = Cal.timezones()
</script>

<template>
  <div class="page narrow">
    <h1>設定</h1>
    <section class="panel">
      <h2>アカウント</h2>
      <form @submit.prevent="save(s)">
        <label class="field"><span>表示名</span><input type="text" v-model="s.name" maxlength="100"></label>
        <label class="field"><span>タイムゾーン（招待のメールなど、サーバーでの日時の表示に使います. 画面はこの端末のタイムゾーン {{ Cal.timezone() }} で表示します）</span>
          <select v-model="s.tz"><option v-for="z in zones" :value="z">{{ z }}</option></select></label>
        <button type="submit" class="primary">保存</button>
      </form>
    </section>
    <section class="panel">
      <h2>表示（この端末）</h2>
      <label class="field"><span>週の始まり</span>
        <select v-model="s.weekStart"><option value="0">日曜日</option><option value="1">月曜日</option><option value="6">土曜日</option></select></label>
      <label class="row"><input type="checkbox" v-model="s.weekends"> 週の表示で土日を表示する</label>
      <label class="field top"><span>画面の色</span>
        <select v-model="s.theme"><option v-for="t in Theme.options" :value="t._1">{{ t._2 }}</option></select></label>
    </section>
    <section class="panel">
      <h2>通知</h2>
      <p class="small">予定に通知（10分前など）を付けると、この画面を開いている間にお知らせします。</p>
      <p class="small muted">デスクトップの通知: {{ permText(s) }}</p>
      <button v-if="canAsk(s)" @click="askPermission(s)">デスクトップの通知を許可する</button>
    </section>
    <ApiTokenSettings />
    <section class="panel">
      <h2>キーボードの操作</h2>
      <table class="keys"><tr v-for="k in Keys.list"><td><kbd>{{ k._1 }}</kbd></td><td class="small">{{ k._2 }}</td></tr></table>
    </section>
  </div>
</template>

<style scoped>
.top { margin-top: 1rem; }
.keys td { padding: .2rem .8rem .2rem 0; }
</style>
