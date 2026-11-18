<script lang="scala">
import skuaka.web.*
import skuaka.shared.*
import org.scalajs.dom

import scala.scalajs.js

// 連携しているサービス（OAuth の認可で発行したトークン）の確認・取り消し、API トークンの作成・失効と、MCP（AI アシスタントからの利用）のつなぎ方
case class State(tokens: RV[List[ApiTokenDto]] = RV(Nil), name: RV[String] = RV(""), scope: RV[String] = RV(TokenScope.Write),
                 days: RV[String] = RV("90"), created: RV[Option[String]] = RV(None))

def initState() = State()

def mounted(s: State): Unit = load(s)

def load(s: State): Unit = Store.report(Api.tokens())(ts => s.tokens := ts)

def create(s: State): Unit = {
  val name = s.name.v.trim
  if (name.isEmpty) Store.toast("名前を入れてください", error = true)
  else Store.report(Api.createToken(ApiTokenReq(name, s.scope.v, s.days.v.toIntOption))) { r =>
    s.created := Some(r.token)
    s.name := ""
    load(s)
  }
}

def disconnect(s: State, t: ApiTokenDto): Unit =
  Dialog.confirm(s"「${t.oauthClient.getOrElse(t.name)}」との連携をやめますか？（このサービスはあなたの予定を読み書きできなくなります）", "連携をやめる") {
    Store.report(Api.revokeToken(t.id))(_ => { Store.toast("連携をやめました"); load(s) })
  }

def revoke(s: State, t: ApiTokenDto): Unit =
  Dialog.confirm(s"API トークン「${t.name}」を失効させますか？（このトークンを使っているスクリプトや AI アシスタントは使えなくなります）", "失効") {
    Store.report(Api.revokeToken(t.id))(_ => load(s))
  }

def when(iso: String): String =
  new js.Date(iso).asInstanceOf[js.Dynamic].toLocaleString("ja-JP", js.Dynamic.literal(dateStyle = "medium", timeStyle = "short")).asInstanceOf[String]
def expired(t: ApiTokenDto): Boolean = t.expiresAt.exists(e => new js.Date(e).getTime() < js.Date.now())
def info(t: ApiTokenDto): String = List(
  Some(TokenScope.label(t.scope)),
  Some(s"作成 ${when(t.createdAt)}"),
  Some(t.expiresAt.fold("期限なし")(e => (if (expired(t)) "期限切れ " else "期限 ") + when(e))),
  Some(t.lastUsedAt.fold("未使用")(u => s"最後に使用 ${when(u)}"))
).flatten.mkString("・")

// 連携のトークンは1時間ごとに更新されるので、期限ではなく許可した日時と最後に使った日時を出す
def linkInfo(t: ApiTokenDto): String = List(
  TokenScope.label(t.scope),
  s"許可 ${when(t.createdAt)}",
  t.lastUsedAt.fold("未使用")(u => s"最後に使用 ${when(u)}")
).mkString("・")
def links(s: State) = s.tokens.mapC(_.filter(_.oauthClient.isDefined))
def manual(s: State) = s.tokens.mapC(_.filter(_.oauthClient.isEmpty))
def noLinks(s: State) = links(s).mapC(_.isEmpty)

def origin: String = dom.window.location.origin
def mcpUrl: String = origin + "/mcp"
def secret(s: State) = s.created.mapC(_.getOrElse("<API トークン>"))
def noTokens(s: State) = manual(s).mapC(_.isEmpty)
def hasCreated(s: State) = s.created.mapC(_.isDefined)
def createdText(s: State) = s.created.mapC(_.getOrElse(""))
def jsonConf(s: State) = secret(s).mapC(t =>
  s"""{\n  "mcpServers": {\n    "skuaka": {\n      "type": "http",\n      "url": "$mcpUrl",\n      "headers": { "Authorization": "Bearer $t" }\n    }\n  }\n}""")
def curlCmd(s: State) = secret(s).mapC(t => s"""curl -H "Authorization: Bearer $t" "$origin/api/events?from=2026-11-01&to=2026-12-01"""")

val scopes: List[(String, String)] = List(TokenScope.Write -> "読み書き（予定の作成・変更・削除もできる）", TokenScope.Read -> "参照のみ")
val periods: List[(String, String)] = List("30" -> "30日", "90" -> "90日", "365" -> "1年", "" -> "期限なし")
</script>

<template>
  <div class="wrap">
  <section class="panel">
    <h2>連携しているサービス</h2>
    <p class="small">あなたが許可して、あなたの権限で予定を使っているサービス（AI エージェントの skakapo など）です。</p>
    <div v-for="t in links(s)" :key="t.id" class="token">
      <div class="grow">
        <div>{{ t.name }}</div>
        <div class="muted small">{{ linkInfo(t) }}</div>
      </div>
      <button class="danger small" @click="disconnect(s, t)">連携をやめる</button>
    </div>
    <p v-if="noLinks(s)" class="muted small">連携しているサービスはありません。</p>
  </section>

  <section class="panel">
    <h2>API トークンと MCP</h2>
    <p class="small">API トークンを使うと、スクリプトから REST API を呼んだり、MCP に対応した AI アシスタントから予定を読み書きしたりできます。
      権限はあなたと同じです（参照のみのトークンは読むだけ）。トークンはパスワードと同じように扱ってください。</p>
    <form class="row new" @submit.prevent="create(s)">
      <input type="text" class="grow" v-model="s.name" maxlength="100" placeholder="名前（例: AI アシスタント、集計スクリプト）">
      <select v-model="s.scope"><option v-for="x in scopes" :value="x._1">{{ x._2 }}</option></select>
      <select v-model="s.days"><option v-for="x in periods" :value="x._1">{{ x._2 }}</option></select>
      <button type="submit" class="primary">作成</button>
    </form>
    <div v-if="hasCreated(s)" class="created">
      <p class="small"><b>トークンを作成しました。</b>この値は今しか表示されません。コピーして保管してください。</p>
      <div class="row">
        <input type="text" class="grow mono" readonly :value="createdText(s)">
        <button @click='Dom.copy(createdText(s).v, "トークンをコピーしました")'>コピー</button>
        <button class="ghost" @click="s.created := None">閉じる</button>
      </div>
    </div>
    <div v-for="t in manual(s)" :key="t.id" class="token">
      <div class="grow">
        <div>{{ t.name }} <code class="muted">{{ t.prefix }}…</code></div>
        <div class="muted small">{{ info(t) }}</div>
      </div>
      <button class="danger small" @click="revoke(s, t)">失効</button>
    </div>
    <p v-if="noTokens(s)" class="muted small">API トークンはまだありません。</p>

    <h3>MCP でつなぐ</h3>
    <p class="small">MCP サーバーの URL: <code>{{ mcpUrl }}</code>（Streamable HTTP。認証は <code>Authorization: Bearer</code> の API トークン）</p>
    <p class="small muted">MCP クライアントの設定の例:</p>
    <div class="row code"><pre class="grow">{{ jsonConf(s) }}</pre><button class="small" @click="Dom.copy(jsonConf(s).v)">コピー</button></div>
    <p class="small muted">使えるツール: 予定の一覧・検索・詳細・作成・変更・削除、招待への返事、空き時間の検索、カレンダーの一覧、任意の REST API の呼び出し。</p>

    <h3>REST API</h3>
    <p class="small">画面と同じ API（<a href="/api/endpoints" target="_blank">一覧</a>）を、<code>Authorization: Bearer</code> を付けて呼べます。</p>
    <div class="row code"><pre class="grow">{{ curlCmd(s) }}</pre><button class="small" @click="Dom.copy(curlCmd(s).v)">コピー</button></div>
  </section>
  </div>
</template>

<style scoped>
.wrap { display: contents; }
h3 { margin-top: 1.4rem; }
.new { flex-wrap: wrap; gap: .4rem; margin: .8rem 0; }
.new input { min-width: 12rem; }
.created { border: 1px solid var(--accent); border-radius: 8px; padding: .6rem .8rem; margin-bottom: .8rem; }
.created p { margin: 0 0 .4rem; }
.mono, pre, code { font-family: ui-monospace, SFMono-Regular, Menlo, monospace; }
.token { display: flex; align-items: center; gap: .6rem; padding: .5rem 0; border-bottom: 1px solid var(--line); }
.code { align-items: flex-start; gap: .4rem; flex-wrap: nowrap; }
pre { margin: 0; padding: .5rem .7rem; background: var(--side); border-radius: 6px; font-size: .78rem;
      white-space: pre-wrap; word-break: break-all; min-width: 0; }
</style>
