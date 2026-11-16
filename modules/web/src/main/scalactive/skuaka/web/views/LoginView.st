<script lang="scala">
import skuaka.web.*
import skuaka.shared.SafePath
import org.scalajs.dom

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.util.{Failure, Success}

// ログイン. 本番は OIDC の IdP へ. 開発用のログイン（DEV_LOGIN=true で IdP がないとき）は名前を入れるだけ.
case class State(name: RV[String] = RV(""), email: RV[String] = RV(""), error: RV[String] = RV(""))

def initState() = State()

def param(k: String): Option[String] = Option(new dom.URL(dom.window.location.href).searchParams.get(k))
def returnTo(): String = SafePath.of(param("returnTo"))
def loggedOut: Boolean = param("loggedOut").isDefined

def login(s: State): Unit =
  Api.devLogin(s.name.v.trim, s.email.v.trim, returnTo()).onComplete {
    case Success(_) => dom.window.location.href = returnTo()
    case Failure(e) => s.error := e.getMessage
  }

def oidcHref: String = "/auth/login?returnTo=" + Api.enc(returnTo())
def dev = Store.config.mapC(_.exists(_.devLogin))
def oidc = Store.config.mapC(_.exists(_.oidc))

def mounted(): Unit = pageTitle("ログイン")
</script>

<template>
  <div class="page narrow">
    <div class="card login">
      <img src="/logo.svg" alt="" width="88" height="88" class="logo">
      <h1>skuaka</h1>
      <p v-if="loggedOut" class="ok">ログアウトしました。</p>
      <template v-if="oidc">
        <p class="muted">組織のアカウント（OpenID Connect）でログインします。</p>
        <a class="btn primary big" :href="oidcHref">ログイン</a>
      </template>
      <form v-if="dev" @submit.prevent="login(s)">
        <p class="muted small">開発用のログインです（IdP を設定していない環境だけで使えます）。</p>
        <label class="field"><span>名前</span><input type="text" v-model="s.name" placeholder="山田 太郎" autofocus></label>
        <label class="field"><span>メールアドレス（任意. 共有・招待の相手を探すのに使います）</span><input type="email" v-model="s.email"></label>
        <p v-if="s.error.mapC(_.nonEmpty)" class="error">{{ s.error }}</p>
        <p><button type="submit" class="primary">ログイン</button></p>
      </form>
    </div>
  </div>
</template>

<style scoped>
.login { max-width: 420px; margin: 8vh auto 0; padding: 1.8rem; text-align: center; }
.login form { text-align: left; }
.logo { display: block; margin: 0 auto .4rem; }
.big { font-size: 1rem; padding: .6rem 2.4rem; border-radius: 999px; }
</style>
