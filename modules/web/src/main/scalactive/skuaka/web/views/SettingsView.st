<script lang="scala">
import skuaka.web.*
import skuaka.shared.*
import skuaka.web.parts.*

import org.scalajs.dom

import scala.concurrent.{Future, Promise}
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
  skakaResult()
  watch(s.weekStart)((_, v) => { Store.weekStart := v.toIntOption.getOrElse(0); Prefs.set("weekStart", v) })
  watch(s.weekends)((_, v) => { Store.weekends := v; Prefs.set("weekends", v.toString) })
  watch(s.theme)((_, v) => Theme.set(v))
}

def save(s: State): Unit =
  Store.report(Api.updateMe(UpdateMeReq(Some(s.name.v.trim), Some(s.tz.v)))) { me =>
    Store.me := Some(me)
    Store.toast("保存しました")
  }

// ---------------------------------------------------------------- Web 会議（skaka）

def hasSkaka = Store.skaka.mapC(_.isDefined)
def skakaConnected = Store.skaka.mapC(_.exists(_.connected))
def skakaUrl = Store.skaka.mapC(_.flatMap(_.url).getOrElse(""))
def connectUrl: String = Api.skakaConnectUrl("/settings")

/** 許可の画面から戻ったときの結果（?integration=skaka&result=…）を知らせて、URL から消す. */
def skakaResult(): Unit = {
  val q = new dom.URLSearchParams(dom.window.location.search)
  if (q.get("integration") == "skaka") {
    q.get("result") match {
      case "connected" => Store.toast("skaka（Web 会議）と連携しました")
      case "denied"    => Store.toast("skaka で許可されませんでした", error = true)
      case _           => Store.toast("skaka と連携できませんでした。もう一度試してください", error = true)
    }
    dom.window.history.replaceState(null, "", dom.window.location.pathname)
    Store.loadSkaka().failed.foreach(_ => ())
  }
}

def disconnectSkaka(): Unit =
  Dialog.confirm("skaka（Web 会議）との連携をやめますか？（付けてある会議室の URL はそのまま残ります）", "やめる") {
    Store.report(Api.disconnectSkaka())(_ => { Store.toast("連携をやめました"); Store.loadSkaka().failed.foreach(_ => ()) })
  }

// ---------------------------------------------------------------- アイコン

val AvatarPx = 256

def meBrief = Store.me.mapC(_.map(_.brief))
def uploaded = Store.me.mapC(_.exists(_.avatarUploaded))

/** 画像の中央を正方形に切り抜き、256×256 にする（WebP. 対応していないブラウザでは PNG）. */
def square(file: dom.File): Future[dom.Blob] = {
  js.Dynamic.global.createImageBitmap(file).asInstanceOf[js.Promise[js.Dynamic]].toFuture.flatMap { img =>
    val (w, h) = (img.width.asInstanceOf[Double], img.height.asInstanceOf[Double])
    val side = math.min(w, h)
    val canvas = dom.document.createElement("canvas").asInstanceOf[dom.HTMLCanvasElement]
    canvas.width = AvatarPx; canvas.height = AvatarPx
    val ctx = canvas.getContext("2d").asInstanceOf[js.Dynamic]
    ctx.imageSmoothingQuality = "high"
    ctx.drawImage(img, (w - side) / 2, (h - side) / 2, side, side, 0, 0, AvatarPx, AvatarPx)
    val p = Promise[dom.Blob]()
    canvas.asInstanceOf[js.Dynamic].toBlob((b: dom.Blob) =>
      if (b == null) p.failure(new Exception("画像を作れませんでした")) else p.success(b), "image/webp", 0.9)
    p.future
  }
}

def avatarDone(me: MeDto, msg: String): Unit = {
  Store.me := Some(me)
  Store.toast(msg)
  Store.report(Store.loadCalendars())(_ => ()) // 共有の相手などに出る自分のアイコンも新しくする
}

def pickAvatar(e: dom.Event): Unit = {
  val input = e.target.asInstanceOf[dom.HTMLInputElement]
  Option(input.files).filter(_.length > 0).map(_(0)).foreach { f =>
    input.value = "" // 同じファイルを選び直せるように
    if (!f.`type`.startsWith("image/")) Store.toast("画像のファイルを選んでください", error = true)
    else Store.report(square(f).flatMap(Api.uploadAvatar))(me => avatarDone(me, "アイコンを保存しました"))
  }
}

def removeAvatar(): Unit =
  Dialog.confirm("保存したアイコンを消しますか？（ログインの仕組みから受け取った画像か、名前の1文字目の表示に戻ります）", "消す") {
    Store.report(Api.deleteAvatar())(me => avatarDone(me, "アイコンを消しました"))
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
      <h2>Web 会議（skaka）</h2>
      <p class="small">skaka と連携すると、予定を作るときに Web 会議の会議室を付けられます（会議室の URL を予定の場所に入れます）。</p>
      <template v-if="skakaConnected">
        <p class="small">✓ 連携しています（<a :href="skakaUrl.v" target="_blank" rel="noopener">{{ skakaUrl }}</a>）</p>
        <button class="danger" @click="disconnectSkaka()">連携をやめる</button>
      </template>
      <a v-else class="btn primary" :href="connectUrl">skaka（Web 会議）と連携する</a>
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
    <ApiTokenSettings />
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
