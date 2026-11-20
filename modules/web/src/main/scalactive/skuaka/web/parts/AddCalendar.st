<script lang="scala">
import skuaka.web.*
import skuaka.shared.*

// カレンダーを追加する: 新しいカレンダーを作る・iCalendar の URL を購読する・iCalendar のファイルを取り込む
case class State(tab: RV[String] = RV("new"), name: RV[String] = RV(""), url: RV[String] = RV(""), urlName: RV[String] = RV(""),
                 target: RV[String] = RV(""), act: ActionState = useAction())

def initState() = State(target = RV(Store.writable.headOption.map(_.id.toString).getOrElse("")))

val tabs: List[(String, String)] = List("new" -> "新しいカレンダー", "url" -> "URL で購読", "file" -> "ファイルから取り込む")
def tabClass(s: State, t: String) = s.tab.mapC(x => if (x == t) "seg active" else "seg")
def targets = Store.calendars.mapC(_.filter(_.writable))

/** 次のカレンダーの色（一覧の数でずらす）. */
def nextColor(): String = Palette.colors((Store.calendars.v.size * 3) % Palette.colors.size)._1

def close(): Unit = Store.addCalendar := false

/** 作ったカレンダーを一覧に入れ、設定を開く. */
def opened(c: CalendarDto): Unit = Toasts.report(Store.loadCalendars()) { _ => Store.load(); close(); Store.calSettings := Some(c.id) }

def create(s: State): Unit = if (s.name.v.trim.nonEmpty)
  Toasts.report(Api.createCalendar(CalendarReq(s.name.v.trim, nextColor(), "")))(opened)

def subscribe(s: State): Unit = if (s.url.v.trim.nonEmpty && !s.act.busy.v)
  Toasts.report(s.act.run(Api.subscribe(SubscribeReq(s.url.v.trim, Some(s.urlName.v.trim).filter(_.nonEmpty), Some(nextColor()))))) { c =>
    Toasts.show(s"「${c.name}」を購読しました")
    opened(c)
  }

def pick(s: State, e: org.scalajs.dom.Event): Unit =
  scala.util.Try(java.util.UUID.fromString(s.target.v)).toOption.foreach(id => IcsFile.importFrom(e, id)(close()))
</script>

<template>
  <Modal :onClose="_ => close()" :wide="true">
    <div class="row hd"><h2 class="grow">カレンダーを追加</h2><button class="ghost" @click="close()">×</button></div>
    <div class="segs tabs">
      <button v-for="t in tabs" :class="tabClass(s, t._1)" @click="s.tab := t._1">{{ t._2 }}</button>
    </div>
    <form v-if='s.tab.v == "new"' @submit.prevent="create(s)">
      <label class="field"><span>名前</span><input type="text" v-model="s.name" maxlength="100" required v-focus></label>
      <p><button type="submit" class="primary">作成</button></p>
    </form>
    <form v-if='s.tab.v == "url"' @submit.prevent="subscribe(s)">
      <p class="muted small">ほかのカレンダーの iCalendar（.ics）の URL を入れると、その予定を読み取りだけのカレンダーとして表示します。
        1時間ごとに読み直すので、元のカレンダーの変更も反映されます。</p>
      <label class="field"><span>iCalendar の URL</span>
        <input type="url" v-model="s.url" placeholder="https://… または webcal://…" required v-focus></label>
      <label class="field"><span>名前（任意. 省くと元のカレンダーの名前）</span><input type="text" v-model="s.urlName" maxlength="100"></label>
      <details class="help small">
        <summary>URL の調べ方</summary>
        <ul>
          <li><b>Google カレンダー</b>: パソコンで Google カレンダーを開き、左の一覧のカレンダーの「⋮」→「設定と共有」→「カレンダーの統合」の「iCal 形式の非公開 URL」をコピーします。</li>
          <li><b>Outlook（Microsoft 365）</b>: 「設定」→「予定表」→「共有の予定表」→「予定表を公開する」で、ICS のリンクをコピーします。</li>
          <li><b>iCloud（Apple のカレンダー）</b>: カレンダーの共有の設定で「公開カレンダー」をオンにし、表示された URL（webcal://…）をコピーします。</li>
        </ul>
        <p>非公開の URL を知っている人はだれでも予定を見られるので、ほかの人に教えないでください。</p>
      </details>
      <p><button type="submit" class="primary" :disabled="s.act.busy.v">{{ if (s.act.busy.v) "読み込んでいます…" else "購読する" }}</button></p>
    </form>
    <div v-if='s.tab.v == "file"'>
      <p class="muted small">ほかのカレンダーから書き出した iCalendar（.ics）のファイルの予定を、{{ AppNames.self }} のカレンダーに写します（一度だけ. 写したあとは {{ AppNames.self }} の予定として変更できます）。
        同じファイルを取り込み直すと、重ならずに変わった予定だけを上書きします。Google カレンダーでは「設定」→「インポート/エクスポート」→「エクスポート」で書き出せます（ZIP を展開した .ics を選びます）。</p>
      <label class="field"><span>取り込み先</span>
        <select v-model="s.target"><option v-for="c in targets" :value="c.id.toString">{{ c.name }}</option></select>
      </label>
      <label class="btn primary">ファイルを選んで取り込む<input type="file" accept=".ics,text/calendar" class="hidden-file" @change="pick(s, event)"></label>
    </div>
    <div class="actions"><span class="grow"></span><button @click="close()">閉じる</button></div>
  </Modal>
</template>

<style scoped>
.hd h2 { margin: 0; }
.tabs { margin: .4rem 0 1rem; display: flex; flex-wrap: wrap; }
.help { margin: .2rem 0 .8rem; }
.help summary { cursor: pointer; color: var(--accent); }
.help ul { padding-left: 1.2rem; margin: .4rem 0; }
.help li { margin: .3rem 0; }
.actions .grow { flex: 1; }
</style>
