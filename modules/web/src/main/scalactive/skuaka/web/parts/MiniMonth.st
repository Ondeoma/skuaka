<script lang="scala">
import skuaka.web.*

// サイドバーのミニカレンダー（日付をクリックするとその日へ）
case class State(month: RV[String] = RV(Cal.monthStart(Store.date.v)))

def initState() = State()

def mounted(s: State): Unit = watch(Store.date)((_, d) => s.month := Cal.monthStart(d))

def days(s: State) = computed { Store.monthDays(s.month.v) }
def heads = Store.weekStart.mapC(w => (0 until 7).toList.map(i => Cal.WeekNames((w + i) % 7)))
def label(s: State) = s.month.mapC(m => s"${Cal.year(m)}年 ${Cal.month(m)}月")

def cls(s: State, d: String) = computed {
  val sel = Store.date.v
  val (f, t) = Store.range(Store.view.v, sel)
  List(
    Some("d"),
    Option.when(Cal.month(d) != Cal.month(s.month.v))("out"),
    Option.when(d == Cal.today())("today"),
    Option.when(d == sel)("sel"),
    Option.when(d >= f && d < t && Store.view.v != "day" && Store.view.v != "agenda")("inrange"),
    Option.when(Holidays.red(d))("hol")
  ).flatten.mkString(" ")
}

def pick(d: String): Unit = Store.date := d
</script>

<template>
  <div class="mm">
    <div class="mh">
      <span class="ml">{{ label(s) }}</span>
      <button class="ghost small" @click="s.month := Cal.addMonths(s.month.v, -1)">‹</button>
      <button class="ghost small" @click="s.month := Cal.addMonths(s.month.v, 1)">›</button>
    </div>
    <div class="grid">
      <span v-for="h in heads" class="wh">{{ h }}</span>
      <button v-for="d in days(s)" :key="d" :class="cls(s, d)" :title='Holidays.name(d).getOrElse("")' @click="pick(d)">{{ Cal.day(d) }}</button>
    </div>
  </div>
</template>

<style scoped>
.mm { padding: 0 .2rem; }
.mh { display: flex; align-items: center; gap: .1rem; padding: 0 .2rem .3rem; }
.ml { flex: 1; font-size: .88rem; font-weight: 600; }
.grid { display: grid; grid-template-columns: repeat(7, 1fr); text-align: center; row-gap: 2px; }
.wh { font-size: .68rem; color: var(--muted); padding: .15rem 0; }
.d { border: none; background: transparent; border-radius: 999px; font-size: .74rem; padding: 0; width: 28px; height: 28px; margin: 0 auto;
  justify-content: center; }
.d.out { color: var(--muted); opacity: .7; }
.d.hol { color: var(--danger); }
.d.inrange { background: var(--accent-weak); }
.d.sel { background: color-mix(in srgb, var(--accent) 25%, transparent); font-weight: 700; }
.d.today { background: var(--accent); color: #fff; font-weight: 700; }
</style>
