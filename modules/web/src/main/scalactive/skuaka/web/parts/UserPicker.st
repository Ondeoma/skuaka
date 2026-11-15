<script lang="scala">
import skuaka.web.*
import skuaka.shared.*
import org.scalajs.dom

import java.util.UUID
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// ユーザーを名前・メールアドレスで探して選ぶ（ゲスト・共有の相手）. 選んだら onPick.
case class Props(onPick: UserBrief => Unit, exclude: List[UUID] = Nil, placeholder: String = "名前・メールアドレスで探す")

case class State(q: RV[String] = RV(""), found: RV[List[UserBrief]] = RV(Nil), open: RV[Boolean] = RV(false))

def initState() = State()

private var seq = 0

def typed(s: State, v: String): Unit = {
  s.q := v
  seq += 1
  val my = seq
  if (v.trim.isEmpty) s.found := Nil
  else dom.window.setTimeout(() => if (my == seq) Api.users(v.trim).foreach { us => if (my == seq) { s.found := us; s.open := true } }, 200)
}

def candidates(s: State, props: Props) = s.found.mapC(_.filterNot(u => props.exclude.contains(u.id)))

def pick(s: State, props: Props, u: UserBrief): Unit = {
  props.onPick(u)
  s.q := ""
  s.found := Nil
  s.open := false
}

def close(s: State): Unit = dom.window.setTimeout(() => s.open := false, 150)
</script>

<template>
  <div class="picker">
    <input type="text" :value="s.q" :placeholder="props.placeholder" autocomplete="off"
      @input="typed(s, Dom.value(event))" @focus="s.open := true" @blur="close(s)">
    <div v-if="s.open" class="list">
      <a v-for="u in candidates(s, props)" :key="u.id" href="#" class="item" @mousedown.prevent="pick(s, props, u)">
        <Avatar :user="Some(u)" :size="22" /><span class="grow">{{ u.name }}</span><span class="muted small">{{ u.email.getOrElse("") }}</span>
      </a>
    </div>
  </div>
</template>

<style scoped>
.picker { position: relative; }
.picker input { width: 100%; }
.list { position: absolute; left: 0; right: 0; top: calc(100% + 2px); background: var(--surface); border: 1px solid var(--line); border-radius: 8px;
  box-shadow: var(--shadow); z-index: 20; max-height: 240px; overflow-y: auto; }
.list:empty { display: none; }
.item { display: flex; gap: .5rem; align-items: center; padding: .4rem .6rem; color: var(--fg); }
.item:hover { background: var(--hover); text-decoration: none; }
</style>
