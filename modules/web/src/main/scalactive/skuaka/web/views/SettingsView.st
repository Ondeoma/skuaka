<script lang="scala">
import skuaka.web.*
import skuaka.shared.*
import skuaka.web.parts.*

import org.scalajs.dom

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js

// 設定: アイコン・名前・タイムゾーン（サーバー）、週の始まり・土日・画面の色・通知（この端末）
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
  linkResult()
  watch(s.weekStart)((_, v) => { Store.weekStart := v.toIntOption.getOrElse(0); Prefs.set("weekStart", v) })
  watch(s.weekends)((_, v) => { Store.weekends := v; Prefs.set("weekends", v.toString) })
  watch(s.theme)((_, v) => Theme.set(v))
}

def save(s: State): Unit =
  Toasts.report(Api.updateMe(UpdateMeReq(Some(s.name.v.trim), Some(s.tz.v)))) { me =>
    Store.me := Some(me)
    Toasts.show("保存しました")
  }

// ---------------------------------------------------------------- 兄弟のサービス（skaka: Web 会議・stui: チャット）との連携

def hasSkaka = Store.skaka.mapC(_.isDefined)
def skakaConnected = Store.skaka.mapC(_.exists(_.connected))
def skakaUrl = Store.skaka.mapC(_.flatMap(_.url).getOrElse(""))
def connectUrl: String = Api.linkConnectUrl("skaka", "/settings")

def hasStui = Store.stui.mapC(_.isDefined)
def stuiConnected = Store.stui.mapC(_.exists(_.connected))
def stuiUrl = Store.stui.mapC(_.flatMap(_.url).getOrElse(""))
def stuiConnectUrl: String = Api.linkConnectUrl("stui", "/settings")

private val linkLabels = Map("skaka" -> "Web 会議", "stui" -> "チャット")

/** 許可の画面から戻ったときの結果（?integration=skaka&result=…）を知らせて、URL から消す. */
def linkResult(): Unit =
  Router.queryParam("integration").v.filter(linkLabels.contains).foreach { p =>
    Router.queryParam("result").v.getOrElse("") match {
      case "connected" => Toasts.show(s"${AppNames.nameOf(p)}（${linkLabels(p)}）と連携しました")
      case "denied"    => Toasts.show(s"${AppNames.nameOf(p)} で許可されませんでした", error = true)
      case _           => Toasts.show(s"${AppNames.nameOf(p)} と連携できませんでした。もう一度試してください", error = true)
    }
    Router.setQuery("integration" -> None, "result" -> None)
    Store.loadLink(p).failed.foreach(_ => ())
  }

def disconnect(p: String, note: String): Unit =
  Dialog.confirm(s"${AppNames.nameOf(p)}（${linkLabels(p)}）との連携をやめますか？（$note）", "やめる") {
    Toasts.report(Api.disconnectLink(p))(_ => { Toasts.show("連携をやめました"); Store.loadLink(p).failed.foreach(_ => ()) })
  }
def disconnectSkaka(): Unit = disconnect("skaka", "付けてある会議室の URL はそのまま残ります")
def disconnectStui(): Unit = disconnect("stui", "DM は届かなくなります。あなたが設定したカレンダーの通知先（stui のチャンネル）にも投稿できなくなります")

def chatReminders = Store.me.mapC(_.exists(_.chatReminders))
def chatInvites = Store.me.mapC(_.exists(_.chatInvites))
def setChat(reminders: Option[Boolean], invites: Option[Boolean]): Unit =
  Toasts.report(Api.updateMe(UpdateMeReq(None, None, reminders, invites)))(me => Store.me := Some(me))

// ---------------------------------------------------------------- アイコン

def meBrief = Store.me.mapC(_.map(_.brief.person))
def uploaded = Store.me.mapC(_.exists(_.avatarUploaded))

def avatarDone(me: MeDto, msg: String): Unit = {
  Store.me := Some(me)
  Toasts.show(msg)
  Toasts.report(Store.loadCalendars())(_ => ()) // 共有の相手などに出る自分のアイコンも新しくする
}

def pickAvatar(e: dom.Event): Unit = Images.pickAvatar(e)(Api.uploadAvatar)(me => avatarDone(me, "アイコンを保存しました"))

def removeAvatar(): Unit =
  Dialog.confirm("保存したアイコンを消しますか？（ログインの仕組みから受け取った画像か、名前の1文字目の表示に戻ります）", "消す") {
    Toasts.report(Api.deleteAvatar())(me => avatarDone(me, "アイコンを消しました"))
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
      <div class="field"><span>アイコン</span>
        <div class="row avatar-row">
          <Avatar :user="meBrief.v" :size="64" />
          <label class="btn">画像を選ぶ<input type="file" accept="image/png,image/jpeg,image/gif,image/webp" class="hidden-file" @change="pickAvatar(event)"></label>
          <button v-if="uploaded" type="button" class="ghost" @click="removeAvatar()">消す</button>
        </div>
        <span class="muted small">中央を正方形に切り抜いて保存します。</span>
      </div>
      <form @submit.prevent="save(s)">
        <label class="field"><span>表示名</span><input type="text" v-model="s.name" maxlength="100"></label>
        <label class="field"><span>タイムゾーン（招待のメールなど、サーバーでの日時の表示に使います. 画面はこの端末のタイムゾーン {{ Cal.timezone() }} で表示します）</span>
          <select v-model="s.tz"><option v-for="z in zones" :value="z">{{ z }}</option></select></label>
        <button type="submit" class="primary">保存</button>
      </form>
    </section>
    <section v-if="hasSkaka" class="panel">
      <h2>Web 会議（{{ AppNames.of("skaka") }}）</h2>
      <p class="small">{{ AppNames.of("skaka") }} と連携すると、予定を作るときに Web 会議の会議室を付けられます（会議室の URL を予定の場所に入れます）。</p>
      <template v-if="skakaConnected">
        <p class="small">✓ 連携しています（<a :href="skakaUrl.v" target="_blank" rel="noopener">{{ skakaUrl }}</a>）</p>
        <button class="danger" @click="disconnectSkaka()">連携をやめる</button>
      </template>
      <a v-else class="btn primary" :href="connectUrl">{{ AppNames.of("skaka") }}（Web 会議）と連携する</a>
    </section>
    <section v-if="hasStui" class="panel">
      <h2>チャット（{{ AppNames.of("stui") }}）</h2>
      <p class="small">{{ AppNames.of("stui") }} と連携すると、予定のリマインダーと招待が {{ AppNames.of("stui") }} のボット「{{ AppNames.selfName }}」からの DM でも届きます。
        カレンダーの設定の「通知先」で、{{ AppNames.of("stui") }} のチャンネルに予定を流すこともできます。</p>
      <template v-if="stuiConnected">
        <p class="small">✓ 連携しています（<a :href="stuiUrl.v" target="_blank" rel="noopener">{{ stuiUrl }}</a>）</p>
        <label class="row"><input type="checkbox" :checked="chatReminders.v" @change="setChat(Some(Dom.checked(event)), None)"> リマインダーを DM で受け取る</label>
        <label class="row"><input type="checkbox" :checked="chatInvites.v" @change="setChat(None, Some(Dom.checked(event)))"> 招待を DM で受け取る</label>
        <p><button class="danger" @click="disconnectStui()">連携をやめる</button></p>
      </template>
      <a v-else class="btn primary" :href="stuiConnectUrl">{{ AppNames.of("stui") }}（チャット）と連携する</a>
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
      <p class="small">プッシュ通知を受け取ると、画面を閉じていても予定の通知と招待が届きます。</p>
      <p><PushToggle /></p>
    </section>
    <ApiTokenSettings mcpName="skuaka" curlPath="/api/events?from=2026-11-01&amp;to=2026-12-01" writeLabel="読み書き（予定の作成・変更・削除もできる）" about="予定を読み書きしたりできます。" tools="予定の一覧・検索・詳細・作成・変更・削除、招待への返事、空き時間の検索、カレンダーの一覧、任意の REST API の呼び出し。" :linksAbout='s"あなたが許可して、あなたの権限で予定を使っているサービス（AI エージェントの ${AppNames.nameOf("skakapo")} など）です。"' linkLost="このサービスはあなたの予定を読み書きできなくなります" />
    <section class="panel">
      <h2>キーボードの操作</h2>
      <table class="keys"><tr v-for="k in Keys.list"><td><kbd>{{ k._1 }}</kbd></td><td class="small">{{ k._2 }}</td></tr></table>
    </section>
  </div>
</template>

<style scoped>
.avatar-row { align-items: center; gap: .8rem; }
.top { margin-top: 1rem; }
.keys td { padding: .2rem .8rem .2rem 0; }
</style>
