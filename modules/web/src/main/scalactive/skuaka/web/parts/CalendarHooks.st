<script lang="scala">
import skuaka.web.*
import skuaka.shared.*

import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// カレンダーの通知先（所有者と変更の権限がある人）: 予定のリマインダー・追加・変更・削除を、stui のチャンネル・stui の受信 Webhook・Webhook に送る
case class Props(calId: UUID)

case class State(hooks: RV[List[HookDto]] = RV(Nil),
                 // 開いている入力欄（None: 閉じている. Some(None): 新しい通知先. Some(Some(id)): その通知先の変更）
                 editing: RV[Option[Option[UUID]]] = RV(None),
                 kind: RV[String] = RV(HookKind.Stui), name: RV[String] = RV(""), url: RV[String] = RV(""),
                 events: RV[Set[String]] = RV(HookEvent.all.toSet), enabled: RV[Boolean] = RV(true),
                 channelId: RV[String] = RV(""), channels: RV[List[StuiChannelDto]] = RV(Nil), channelError: RV[String] = RV(""),
                 log: RV[Option[(UUID, List[HookDeliveryDto])]] = RV(None), act: ActionState = useAction())

def initState(props: Props) = State()

def mounted(s: State, props: Props): Unit = reload(s, props)

def reload(s: State, props: Props): Unit = Toasts.report(Api.hooks(props.calId))(hs => s.hooks := hs)

def hasStui = Store.stui.mapC(_.isDefined)
def stuiConnected = Store.stui.mapC(_.exists(_.connected))
def kinds = Store.stui.mapC(st => HookKind.all.filter(k => st.isDefined || k == HookKind.Url).map(k => k -> kindLabel(k)))
def kindLabel(k: String): String = k match {
  case HookKind.Stui     => s"${AppNames.nameOf("stui")} のチャンネル"
  case HookKind.StuiHook => s"${AppNames.nameOf("stui")} の受信 Webhook"
  case _                 => "Webhook（任意の URL）"
}
def connectUrl: String = Api.linkConnectUrl("stui", "/settings")

def isNew(s: State) = s.editing.mapC(_.contains(None))
def isOpen(s: State) = s.editing.mapC(_.isDefined)
def isKind(s: State, k: String) = s.kind.mapC(_ == k)
def hasEvent(s: State, e: String) = s.events.mapC(_.contains(e))
def toggle(s: State, e: String, on: Boolean): Unit = s.events := (if (on) s.events.v + e else s.events.v - e)

/** stui のチャンネルの一覧を読む（連携していなければ読まない）. */
def loadChannels(s: State): Unit =
  if (Store.stui.v.exists(_.connected) && s.channels.v.isEmpty)
    Api.stuiChannels().onComplete {
      case scala.util.Success(cs) => s.channels := cs; s.channelError := ""
      case scala.util.Failure(e) => s.channelError := e.getMessage
    }

def startNew(s: State): Unit = {
  s.editing := Some(None)
  s.kind := (if (Store.stui.v.isDefined) HookKind.Stui else HookKind.Url)
  s.name := ""; s.url := ""; s.events := HookEvent.all.toSet; s.enabled := true; s.channelId := ""
  loadChannels(s)
}

def startEdit(s: State, h: HookDto): Unit = {
  s.editing := Some(Some(h.id))
  s.kind := h.kind; s.name := h.name; s.url := h.url; s.events := h.events.toSet; s.enabled := h.enabled
  s.channelId := h.channelId.getOrElse("")
  loadChannels(s)
}

def cancel(s: State): Unit = s.editing := None

def save(s: State, props: Props): Unit = if (!s.act.busy.v) {
  val ch = s.channels.v.find(_.id == s.channelId.v)
  val req = HookReq(s.kind.v, s.name.v.trim, s.url.v.trim, HookEvent.all.filter(s.events.v.contains), s.enabled.v,
    Some(s.channelId.v).filter(_.nonEmpty), ch.map(_.name))
  val f = s.editing.v.flatten match {
    case Some(id) => Api.updateHook(id, req)
    case None     => Api.createHook(props.calId, req)
  }
  Toasts.report(s.act.run(f)) { hs => s.hooks := hs; s.editing := None; Toasts.show("通知先を保存しました") }
}

def remove(s: State, props: Props, h: HookDto): Unit =
  Dialog.confirm(s"通知先「${h.name}」を消しますか？", "消す")(Toasts.report(Api.deleteHook(h.id))(_ => reload(s, props)))

def test(s: State, props: Props, h: HookDto): Unit =
  Toasts.report(Api.testHook(h.id)) { _ =>
    Toasts.show("テストの通知を送ります（10秒ほどかかります）")
    org.scalajs.dom.window.setTimeout(() => { reload(s, props); if (s.log.v.exists(_._1 == h.id)) showLog(s, h) }, 12000)
  }

def showLog(s: State, h: HookDto): Unit = Toasts.report(Api.hookDeliveries(h.id))(ds => s.log := Some(h.id -> ds))
def logOf(s: State, h: HookDto) = s.log.mapC(_.filter(_._1 == h.id).map(_._2).getOrElse(Nil))
def logOpen(s: State, h: HookDto) = s.log.mapC(_.exists(_._1 == h.id))

def target(h: HookDto): String = h.kind match {
  case HookKind.Stui => s"#${h.channelName.getOrElse("")}" + h.owner.fold("")(o => s"（${o.name} さんの連携で投稿）")
  case HookKind.StuiHook => "受信 Webhook"
  case _ => h.url
}
def eventsText(h: HookDto): String = h.events.map(e => HookEvent.label(e).takeWhile(_ != '（')).mkString("・")
def status(d: HookDeliveryDto): String =
  if (d.done) "✓ 送りました"
  else if (d.failed) s"× 送れませんでした（${d.error.getOrElse("")}）"
  else s"… 送っています（${d.attempts}回失敗. ${d.error.getOrElse("")}）"
def lastText(h: HookDto): String = h.last.fold("まだ送っていません")(d => s"${Cal.longLabel(Cal.dateOf(d.at))} ${Cal.time(d.at)} ${HookEvent.label(d.event).takeWhile(_ != '（')}: ${status(d)}")
def lastBad(h: HookDto): Boolean = h.last.exists(d => d.failed || (!d.done && d.attempts > 0))

val events: List[String] = HookEvent.all
</script>

<template>
  <div>
    <h3>通知先</h3>
    <p class="muted small">このカレンダーの予定のリマインダー（予定に付けた通知の時刻）と、予定の追加・変更・削除を、
      {{ AppNames.of("stui") }} のチャンネルや Webhook に送ります。</p>
    <div v-for="h in s.hooks" :key="h.id" class="hook">
      <div class="row">
        <div class="grow">
          <div><b>{{ h.name }}</b> <span class="muted small">{{ kindLabel(h.kind) }}</span><span v-if="!h.enabled" class="muted small">（止めています）</span></div>
          <div class="small target">{{ target(h) }}</div>
          <div class="muted small">{{ eventsText(h) }}</div>
          <div :class='if (lastBad(h)) "small error" else "small muted"'>最後の送信: {{ lastText(h) }}</div>
        </div>
        <div class="row btns">
          <button class="small" @click="test(s, props, h)">テスト</button>
          <button class="small" @click="showLog(s, h)">記録</button>
          <button class="small" @click="startEdit(s, h)">変更</button>
          <button class="ghost small" title="消す" @click="remove(s, props, h)">×</button>
        </div>
      </div>
      <table v-if="logOpen(s, h)" class="log small">
        <tr v-for="d in logOf(s, h)"><td>{{ Cal.longLabel(Cal.dateOf(d.at)) }} {{ Cal.time(d.at) }}</td><td>{{ HookEvent.label(d.event) }}</td><td>{{ status(d) }}</td></tr>
        <tr v-if="logOf(s, h).v.isEmpty"><td class="muted">まだ送っていません</td></tr>
      </table>
    </div>
    <p v-if="!isOpen(s).v"><button @click="startNew(s)">＋ 通知先を追加</button></p>
    <form v-if="isOpen(s)" class="hook-form" @submit.prevent="save(s, props)">
      <label class="field"><span>種類</span>
        <select v-model="s.kind" @change="loadChannels(s)"><option v-for="k in kinds" :value="k._1">{{ k._2 }}</option></select></label>
      <template v-if="isKind(s, HookKind.Stui)">
        <template v-if="stuiConnected">
          <label class="field"><span>チャンネル（あなたが参加しているもの）</span>
            <select v-model="s.channelId" required>
              <option value="">選んでください</option>
              <option v-for="c in s.channels" :value="c.id">{{ if (c.kind == "private") "🔒 " else "#" }}{{ c.name }}</option>
            </select></label>
          <p v-if="s.channelError.v.nonEmpty" class="error small">{{ s.channelError }}</p>
          <p class="muted small">あなたの {{ AppNames.of("stui") }} との連携で、ボット「{{ AppNames.selfName }}」として投稿します。
            あなたが連携をやめたり、チャンネルを抜けたりすると投稿できなくなります。</p>
        </template>
        <p v-else class="small">{{ AppNames.of("stui") }} のチャンネルを選ぶには、先に <a :href="connectUrl">{{ AppNames.of("stui") }} と連携</a> してください。</p>
      </template>
      <template v-if="isKind(s, HookKind.StuiHook)">
        <label class="field"><span>受信 Webhook の URL（{{ AppNames.of("stui") }} のチャンネルの設定 →「受信 Webhook」で作ります）</span>
          <input type="url" v-model="s.url" :placeholder='if (isNew(s).v) "https://…/hooks/…" else "変えるときだけ入れてください"'></label>
        <p class="muted small">URL を知っている人はだれでもそのチャンネルに投稿できるので、保存した後は表示しません。</p>
      </template>
      <template v-if="isKind(s, HookKind.Url)">
        <label class="field"><span>送り先の URL（https://）</span>
          <input type="url" v-model="s.url" placeholder="https://example.com/hooks/skuaka"></label>
        <p class="muted small">予定の内容を JSON で POST します。X-Skuaka-Signature（sha256=…）に、本文を秘密の値で署名した値を付けます。
          秘密の値は保存した後に表示します。</p>
      </template>
      <label class="field"><span>名前（任意）</span><input type="text" v-model="s.name" maxlength="100"></label>
      <div class="field"><span>送るできごと</span>
        <label v-for="e in events" class="row"><input type="checkbox" :checked="hasEvent(s, e).v" @change="toggle(s, e, Dom.checked(event))"> {{ HookEvent.label(e) }}</label>
      </div>
      <label class="row"><input type="checkbox" v-model="s.enabled"> 送る（外すと止める）</label>
      <div class="row">
        <button type="submit" class="primary" :disabled="s.act.busy.v">保存</button>
        <button type="button" @click="cancel(s)">やめる</button>
      </div>
    </form>
    <template v-for="h in s.hooks">
      <div v-if='h.kind == HookKind.Url && s.editing.v.contains(Some(h.id))' class="field secret">
        <span>署名の秘密の値（{{ h.name }}）</span>
        <div class="row"><input type="text" class="grow" readonly :value="h.secret">
          <button type="button" @click='Dom.copy(h.secret, "秘密の値をコピーしました")'>コピー</button></div>
      </div>
    </template>
  </div>
</template>

<style scoped>
.hook { padding: .5rem 0; border-bottom: 1px solid var(--line); }
.hook .btns { gap: .3rem; flex-wrap: nowrap; }
.target { word-break: break-all; }
.hook-form { margin-top: .6rem; padding: .8rem; border: 1px solid var(--line); border-radius: 8px; }
.log { width: 100%; margin-top: .4rem; }
.log td { padding: .15rem .5rem .15rem 0; vertical-align: top; }
.secret { margin-top: .6rem; }
</style>
