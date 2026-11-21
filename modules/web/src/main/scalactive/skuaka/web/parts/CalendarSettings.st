<script lang="scala">
import skuaka.web.*
import skuaka.shared.*

import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// カレンダーの設定と共有（所有者: 名前・色・説明・共有の相手・iCalendar の配信・削除. ほかの人: 自分の表示の色・外す. 所有者と変更の権限がある人: 通知先）
case class Props(calId: UUID)

case class State(name: RV[String], color: RV[String], desc: RV[String], access: RV[String], members: RV[List[MemberDto]] = RV(Nil),
                 role: RV[String] = RV(CalRole.View), linkAccess: RV[String] = RV(CalRole.Busy), syncing: RV[Boolean] = RV(false))

def cal(props: Props): Option[CalendarDto] = Store.calendar(props.calId)

def initState(props: Props) = {
  val c = cal(props)
  State(RV(c.map(_.name).getOrElse("")), RV(c.map(_.color).getOrElse(Palette.Default)), RV(c.map(_.description).getOrElse("")),
    RV(c.map(_.teamAccess).getOrElse(TeamAccess.None)))
}

// 予定あり・なしのみの人には共有の相手を見せない
def mounted(s: State, props: Props): Unit =
  if (cal(props).exists(c => CalRole.rank(c.role) >= CalRole.rank(CalRole.View))) Toasts.report(Api.members(props.calId))(ms => s.members := ms)

def current(props: Props) = Store.calendars.mapC(_.find(_.id == props.calId))
def isOwner(props: Props) = current(props).mapC(_.exists(_.role == CalRole.Owner))
def notOwner(props: Props) = isOwner(props).mapC(!_)
def link(props: Props) = current(props).mapC(_.flatMap(_.publicLink))
def hasLink(props: Props) = link(props).mapC(_.isDefined)
def pageUrl(props: Props) = link(props).mapC(_.map(_.pageUrl).getOrElse(""))
def icsUrl(props: Props) = link(props).mapC(_.map(_.icsUrl).getOrElse(""))
def linkAccess(props: Props) = link(props).mapC(_.map(_.access).getOrElse(CalRole.Busy))
def canHook(props: Props) = current(props).mapC(_.exists(c => CalRole.rank(c.role) >= CalRole.rank(CalRole.Edit)))
def canDelete(props: Props) = current(props).mapC(_.exists(c => c.role == CalRole.Owner && !c.primary))
def ownerText(props: Props) = current(props).mapC(_.map { c =>
  if (c.viaTeam) s"${c.owner.name} さんがチームに公開しているカレンダー（${TeamAccess.label(c.teamAccess)}）"
  else s"${c.owner.name} さんのカレンダー（あなたの権限: ${CalRole.label(c.role)}）"
}.getOrElse(""))
def hasMembers(s: State) = s.members.mapC(_.nonEmpty)
def leaveLabel(props: Props) = current(props).mapC(c => if (c.exists(_.viaTeam)) "一覧から外す（あとで追加し直せます）" else "一覧から外す")
def myColor(props: Props) = current(props).mapC(_.map(_.displayColor).getOrElse(""))

// iCalendar の URL の購読（所有者だけに URL が返る）と、ファイルの取り込み
def subscription(props: Props) = current(props).mapC(_.flatMap(_.subscription))
def isSub(props: Props) = subscription(props).mapC(_.isDefined)
def subUrl(props: Props) = subscription(props).mapC(_.flatMap(_.url).getOrElse(""))
def syncedText(props: Props) = subscription(props).mapC(_.flatMap(_.syncedAt).fold("まだ読んでいません")(t => s"${Cal.longLabel(Cal.dateOf(t))} ${Cal.time(t)}"))
def syncError(props: Props) = subscription(props).mapC(_.flatMap(_.error).getOrElse(""))
def hasSyncError(props: Props) = syncError(props).mapC(_.nonEmpty)
def syncNow(s: State, props: Props): Unit = if (!s.syncing.v) {
  s.syncing := true
  val f = Api.syncCalendar(props.calId)
  f.onComplete(_ => { s.syncing := false; reload() })
  Toasts.report(f)(_ => Toasts.show("読み直しました"))
}
def importFile(props: Props, e: org.scalajs.dom.Event): Unit = IcsFile.importFrom(e, props.calId)(())


def reload(): Unit = Toasts.report(Store.loadCalendars())(_ => Store.load())

def save(s: State, props: Props): Unit =
  Toasts.report(Api.updateCalendar(props.calId, CalendarReq(s.name.v.trim, s.color.v, s.desc.v, Some(s.access.v)))) { _ =>
    Toasts.show("カレンダーを保存しました"); reload()
  }

def setMyColor(props: Props, c: String): Unit = Toasts.report(Api.calendarPrefs(props.calId, CalendarPrefReq(Some(c), None)))(_ => reload())

def setMember(s: State, props: Props, u: UserBrief, role: String)(done: => Unit): Unit =
  Toasts.report(Api.setMember(props.calId, MemberReq(u.id.toString, role))) { ms => s.members := ms; done }

def share(s: State, props: Props, u: UserBrief): Unit = setMember(s, props, u, s.role.v)(Toasts.show(s"${u.name} さんと共有しました"))
def changeRole(s: State, props: Props, m: MemberDto, role: String): Unit = setMember(s, props, m.user, role)(())
def unshare(s: State, props: Props, m: MemberDto): Unit = Toasts.report(Api.removeMember(props.calId, m.user.id))(_ => mounted(s, props))

def exclude(s: State) = s.members.mapC(_.map(_.user.id))

def setLink(props: Props, access: String, regenerate: Boolean = false): Unit =
  Toasts.report(Api.setPublicLink(props.calId, PublicLinkReq(access, regenerate)))(_ => reload())
def publish(s: State, props: Props): Unit = setLink(props, s.linkAccess.v)
def unpublish(props: Props): Unit =
  Dialog.confirm("公開リンクをやめますか？（リンクを知っている人も、iCalendar で購読している人も見られなくなります）", "やめる") {
    Toasts.report(Api.stopPublicLink(props.calId))(_ => reload())
  }
def regenerate(props: Props): Unit =
  Dialog.confirm("公開リンクを作り直しますか？（今のリンクと iCalendar の URL は使えなくなります）", "作り直す")(setLink(props, linkAccess(props).v, regenerate = true))

def close(): Unit = Store.calSettings := None

/** 所有者は削除、ほかの人は一覧から外す. */
def remove(props: Props): Unit = cal(props).foreach { c =>
  val (msg, ok) =
    if (c.role == CalRole.Owner) (s"カレンダー「${c.name}」を削除しますか？\n予定もすべて削除され、元に戻せません。", "削除")
    else if (c.viaTeam) (s"カレンダー「${c.name}」を一覧から外しますか？（「他のカレンダー」の＋から追加し直せます）", "外す")
    else (s"カレンダー「${c.name}」を一覧から外しますか？（共有した人がもう一度共有すると戻ります）", "外す")
  Dialog.confirm(msg, ok)(Toasts.report(Api.deleteCalendar(c.id)) { _ => close(); reload() })
}

val roles: List[(String, String)] = List(CalRole.View -> CalRole.label(CalRole.View), CalRole.Edit -> CalRole.label(CalRole.Edit))
val accesses: List[(String, String)] = TeamAccess.all.map(a => a -> TeamAccess.label(a))
val linkAccesses: List[(String, String)] = List(CalRole.Busy, CalRole.View).map(a => a -> TeamAccess.label(a))
</script>

<template>
  <Modal :onClose="_ => close()" :wide="true">
    <div class="row hd"><h2 class="grow">カレンダーの設定</h2><button class="ghost" @click="close()">×</button></div>
    <template v-if="isOwner(props)">
      <form @submit.prevent="save(s, props)">
        <label class="field"><span>名前</span><input type="text" v-model="s.name" required></label>
        <label class="field"><span>説明</span><textarea v-model="s.desc" rows="2"></textarea></label>
        <div class="field"><span>色</span>
          <ColorSwatches :value="s.color.v" :onPick="c => s.color := c" />
        </div>
        <label class="field"><span>チームへの公開</span>
          <select v-model="s.access"><option v-for="a in accesses" :value="a._1">{{ a._2 }}</option></select>
        </label>
        <p class="muted small">公開すると、ログインできる人はだれでも「他のカレンダー」の＋からこのカレンダーを追加できます。
          「予定あり・なしのみ」では予定のある時間だけが見え、題名・場所・説明・ゲストは見えません。</p>
        <p><button type="submit" class="primary">保存</button></p>
      </form>
      <template v-if="isSub(props)">
        <h3>購読</h3>
        <p class="muted small">iCalendar の URL を購読したカレンダーです。予定は読み取りだけで、1時間ごとに読み直します。
          購読をやめるには「カレンダーを削除」を押します（元のカレンダーの予定は消えません）。</p>
        <div class="field"><span>iCalendar の URL</span>
          <div class="row"><input type="text" class="grow" readonly :value="subUrl(props)">
            <button @click='Dom.copy(subUrl(props).v, "URL をコピーしました")'>コピー</button></div>
        </div>
        <p class="small">最後に読んだ時刻: {{ syncedText(props) }}</p>
        <p v-if="hasSyncError(props)" class="error small">読めませんでした: {{ syncError(props) }}</p>
        <p><button @click="syncNow(s, props)" :disabled="s.syncing.v">{{ if (s.syncing.v) "読み直しています…" else "今すぐ読み直す" }}</button></p>
      </template>
      <template v-else>
        <h3>ファイルから取り込む</h3>
        <p class="muted small">ほかのカレンダーから書き出した iCalendar（.ics）のファイルの予定を、このカレンダーに写します。
          同じファイルを取り込み直すと、重ならずに変わった予定だけを上書きします。</p>
        <label class="btn">iCalendar（.ics）のファイルを選ぶ<input type="file" accept=".ics,text/calendar" class="hidden-file" @change="importFile(props, event)"></label>
      </template>
      <h3>共有</h3>
      <p class="muted small">共有した人は、サイドバーの「他のカレンダー」でこのカレンダーを見られます。</p>
      <div class="row add">
        <div class="grow"><UserPicker :search="UserSearch.search" :hint="UserSearch.hint" :onPick="p => UserSearch(p).foreach(share(s, props, _))" :exclude="exclude(s).v.map(_.toString)" placeholder="共有する人を名前・メールアドレスで探す" /></div>
        <select v-model="s.role"><option v-for="r in roles" :value="r._1">{{ r._2 }}</option></select>
      </div>
      <div v-for="m in s.members" :key="m.user.id" class="member">
        <Avatar :user="Some(m.user.person)" :size="28" />
        <div class="grow"><div>{{ m.user.name }}</div><div class="muted small">{{ m.user.email.getOrElse("") }}</div></div>
        <span v-if='m.role == "owner"' class="muted small">所有者</span>
        <template v-else>
          <select @change="changeRole(s, props, m, Dom.value(event))"><option v-for="r in roles" :value="r._1" :selected="r._1 == m.role">{{ r._2 }}</option></select>
          <button class="ghost small" title="共有をやめる" @click="unshare(s, props, m)">×</button>
        </template>
      </div>
      <h3>リンクで公開（ログインしない人にも）</h3>
      <p class="muted small">リンクを知っている人は、ログインせずにブラウザでこのカレンダーを見られます（閲覧だけ）。同じリンクの iCalendar の URL を
        iCalendar の購読に対応したカレンダーアプリに URL で追加すると、予定を読み取れます。
        リンクを知っている人はだれでも見られるので、渡す相手に注意してください。ゲストの一覧は公開されません。</p>
      <template v-if="hasLink(props)">
        <label class="field"><span>公開の範囲</span>
          <select :value="linkAccess(props)" @change="setLink(props, Dom.value(event))"><option v-for="a in linkAccesses" :value="a._1">{{ a._2 }}</option></select>
        </label>
        <div class="field"><span>閲覧のページ</span>
          <div class="row"><input type="text" class="grow" readonly :value="pageUrl(props)">
            <button @click='Dom.copy(pageUrl(props).v, "リンクをコピーしました")'>コピー</button></div>
        </div>
        <div class="field"><span>iCalendar（購読用）</span>
          <div class="row"><input type="text" class="grow" readonly :value="icsUrl(props)">
            <button @click='Dom.copy(icsUrl(props).v, "URL をコピーしました")'>コピー</button></div>
        </div>
        <div class="row"><button @click="regenerate(props)">リンクを作り直す</button><button class="danger" @click="unpublish(props)">公開をやめる</button></div>
      </template>
      <div v-else class="row">
        <select v-model="s.linkAccess"><option v-for="a in linkAccesses" :value="a._1">{{ a._2 }}</option></select>
        <button @click="publish(s, props)">公開リンクを作る</button>
      </div>
    </template>
    <template v-else>
      <p>{{ ownerText(props) }}</p>
      <div class="field"><span>自分の画面での色</span>
        <ColorSwatches :value="myColor(props).v" :onPick="c => setMyColor(props, c)" />
      </div>
      <h3 v-if="hasMembers(s)">共有している人</h3>
      <div v-for="m in s.members" :key="m.user.id" class="member">
        <Avatar :user="Some(m.user.person)" :size="28" /><span class="grow">{{ m.user.name }}</span><span class="muted small">{{ CalRole.label(m.role) }}</span>
      </div>
    </template>
    <CalendarHooks v-if="canHook(props)" :calId="props.calId" />
    <div class="actions">
      <button v-if="canDelete(props)" class="danger" @click="remove(props)">カレンダーを削除</button>
      <button v-if="notOwner(props)" class="danger" @click="remove(props)">{{ leaveLabel(props) }}</button>
      <span class="grow"></span>
      <button @click="close()">閉じる</button>
    </div>
  </Modal>
</template>

<style scoped>
.hd h2 { margin: 0; }
h3 { margin-top: 1.4rem; }
.add { margin-bottom: .6rem; flex-wrap: nowrap; }
.member { display: flex; align-items: center; gap: .6rem; padding: .4rem 0; border-bottom: 1px solid var(--line); }
.actions .grow { flex: 1; }
</style>
