<script lang="scala">
import skuaka.web.*
import skuaka.shared.*

// スケジュールの表示: 31日分の予定を日ごとに並べる（複数日の予定は日ごとに出す）
final case class DayGroup(day: String, items: List[OccurrenceDto])

def groups = computed {
  val os = Store.shown.v
  val (from, to) = Store.range("agenda", Store.date.v)
  Cal.range(from, Cal.diffDays(from, to)).flatMap { d =>
    val items = os.filter(Store.onDay(_, d)).sortBy(o => (!o.allDay, o.start))
    Option.when(items.nonEmpty)(DayGroup(d, items))
  }
}

def empty = groups.mapC(_.isEmpty)

/** その日の分の時刻（複数日の予定は、始まる日は「22:00 –」、終わる日は「– 03:00」、間の日は「終日」）. */
def timeOn(o: OccurrenceDto, d: String): String =
  if (o.allDay) "終日"
  else {
    val (sd, ed) = (Cal.dateOf(o.start), Cal.dateOf(o.end))
    val endsMidnight = Cal.minutesOf(o.end) == 0
    if (sd == d && (ed == d || (endsMidnight && ed == Cal.addDays(d, 1)))) s"${Cal.time(o.start)} – ${Cal.time(o.end)}"
    else if (sd == d) s"${Cal.time(o.start)} –"
    else if (ed == d) s"– ${Cal.time(o.end)}"
    else "終日"
  }

def dayClass(d: String): String = Holidays.dayClass("date", d)
def itemClass(o: OccurrenceDto): String = Store.chipClass(o, "item")
</script>

<template>
  <div class="page narrow agenda">
    <div v-for="g in groups" :key="g.day" class="day">
      <button :class="dayClass(g.day)" @click="Store.openDay(g.day)">
        <span class="dn">{{ Cal.day(g.day) }}</span><span class="dl">{{ Cal.month(g.day) }}月（{{ Cal.WeekNames(Cal.dow(g.day)) }}）</span>
      </button>
      <div class="items">
        <div v-if="Holidays.name(g.day).isDefined" class="holiday small">{{ Holidays.name(g.day).getOrElse("") }}</div>
        <div v-for="o in g.items" :class="itemClass(o)" @click="Store.detail := Some(o)">
          <span class="dot" :style="Store.dotStyle(o)"></span>
          <span class="tm">{{ timeOn(o, g.day) }}</span>
          <span class="ti">{{ o.title }}<span v-if="o.location.nonEmpty" class="muted small">　{{ o.location }}</span></span>
        </div>
      </div>
    </div>
    <p v-if="empty" class="empty">この31日間に予定はありません</p>
  </div>
</template>

<style scoped>
.agenda { padding-top: .6rem; }
.day { display: grid; grid-template-columns: 130px 1fr; border-bottom: 1px solid var(--line); padding: .5rem 0; }
.date { border: none; background: transparent; align-self: flex-start; gap: .5rem; padding: .2rem .4rem; }
.dn { font-size: 1.4rem; width: 2.4rem; height: 2.4rem; display: flex; align-items: center; justify-content: center; border-radius: 999px; }
.dl { font-size: .74rem; color: var(--muted); font-weight: 600; }
.date.today .dn { background: var(--accent); color: #fff; }
.date.today .dl { color: var(--accent); }
.date.hol .dn, .date.hol .dl { color: var(--danger); }
.date.today.hol .dn { color: #fff; }
.items { display: flex; flex-direction: column; justify-content: center; }
.item { display: flex; gap: .8rem; align-items: center; padding: .45rem .6rem; border-radius: 8px; cursor: pointer; }
.item:hover { background: var(--hover); }
.item.declined .ti { text-decoration: line-through; opacity: .6; }
.tm { width: 140px; flex: none; font-size: .84rem; color: var(--muted); }
.ti { min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
@media (max-width: 720px) { .day { grid-template-columns: 76px 1fr; } .dl { display: none; } .tm { width: 92px; font-size: .76rem; } }
</style>
