<script lang="scala">
import skuaka.web.*
import skuaka.shared.*
import org.scalajs.dom


// 月の表示: 6週間の表. 予定はドラッグで別の日へ動かせる. 空いたところをクリックすると、その日の終日の予定を作る.

val MaxChips = 3

final case class Cell(day: String, items: List[OccurrenceDto], more: Int)

/** ドラッグ中の予定と、つかんだ日. */
private var dragging: Option[(OccurrenceDto, String)] = None

case class State(over: RV[String] = RV(""))

def initState() = State()

def heads = Store.weekStart.mapC(w => (0 until 7).toList.map(i => (w + i) % 7))

/** その日に重なる予定（終日・複数日 → 時刻の順）. */
def on(os: List[OccurrenceDto], d: String): List[OccurrenceDto] =
  os.filter(Store.onDay(_, d)).sortBy(o => (!o.allDay && Cal.dateOf(o.start) == Cal.dateOf(o.end), o.start))

def weeks = computed {
  val os = Store.shown.v
  Store.monthDays(Store.date.v).map { d =>
    val all = on(os, d)
    Cell(d, all.take(MaxChips), all.size - MaxChips)
  }.grouped(7).toList
}

def isBlock(o: OccurrenceDto): Boolean = o.allDay || Cal.dateOf(o.start) != Cal.dateOf(o.end)

def chipStyle(o: OccurrenceDto): String =
  if (isBlock(o)) s"background: ${Store.color(o)}" else s"--c: ${Store.color(o)}"

def chipClass(o: OccurrenceDto): String = Store.chipClass(o, if (isBlock(o)) "ev mchip block" else "ev mchip line")

def cellClass(s: State, d: String) = s.over.mapC { ov =>
  List(Some("cell"), Option.when(Cal.month(d) != Cal.month(Store.date.v))("out"), Option.when(ov == d)("over")).flatten.mkString(" ")
}

def numClass(d: String): String =
  Holidays.dayClass("num", d)

def label(d: String): String = if (Cal.day(d) == 1) s"${Cal.month(d)}月${Cal.day(d)}日" else Cal.day(d).toString

def start(o: OccurrenceDto, d: String, e: dom.Event): Unit = {
  val de = e.asInstanceOf[dom.DragEvent]
  if (!o.canEdit) de.preventDefault()
  else {
    dragging = Some((o, d))
    de.dataTransfer.setData("text/plain", o.title)
    de.dataTransfer.effectAllowed = dom.DataTransferEffectAllowedKind.move
  }
}

def over(s: State, d: String, e: dom.Event): Unit = if (dragging.isDefined) {
  e.preventDefault()
  if (s.over.v != d) s.over := d
}

/** 落とした日へ、つかんだ日からの日数だけずらす（時刻はそのまま）. */
def drop(s: State, d: String, e: dom.Event): Unit = {
  e.preventDefault()
  s.over := ""
  dragging.foreach { (o, from) =>
    val n = Cal.diffDays(from, d)
    if (n != 0) {
      val (a, b) =
        if (o.allDay) (Cal.addDays(o.start, n), Cal.addDays(o.end, n))
        else (Cal.instant(Cal.addDays(Cal.dateOf(o.start), n), Cal.minutesOf(o.start)),
              Cal.instant(Cal.addDays(Cal.dateOf(o.end), n), Cal.minutesOf(o.end)))
      // 保存を待たずに動かして見せる
      Store.occurrences := Store.occurrences.v.map(x => if (x.eventId == o.eventId && x.occurrence == o.occurrence) x.copy(start = a, end = b) else x)
      Store.move(o, a, b)(Store.load())
    }
  }
  dragging = None
}

def end(s: State): Unit = { dragging = None; s.over := "" }

def open(o: OccurrenceDto, e: dom.Event): Unit = { e.stopPropagation(); Store.detail := Some(o) }

def chipText(o: OccurrenceDto): String = if (isBlock(o)) o.title else s"${Cal.time(o.start)} ${o.title}"
</script>

<template>
  <div class="mg">
    <div class="wh"><span v-for="w in heads" :class='if (w == 0) "hol" else ""'>{{ Cal.WeekNames(w) }}</span></div>
    <div class="weeks">
      <div v-for="wk in weeks" class="week">
        <div v-for="c in wk" :key="c.day" :class="cellClass(s, c.day)" @click.self="Store.create(c.day, allDay = true)"
          @dragover="over(s, c.day, event)" @drop="drop(s, c.day, event)">
          <div class="top" @click.self="Store.create(c.day, allDay = true)">
            <button :class="numClass(c.day)" @click="Store.openDay(c.day)">{{ label(c.day) }}</button>
            <span class="hname">{{ Holidays.name(c.day).getOrElse("") }}</span>
          </div>
          <div v-for="o in c.items" :class="chipClass(o)" :style="chipStyle(o)" :draggable="o.canEdit.toString"
            @dragstart="start(o, c.day, event)" @dragend="end(s)" @click="open(o, event)" :title="o.title">{{ chipText(o) }}</div>
          <button v-if="c.more > 0" class="more" @click="Store.openDay(c.day)">他 {{ c.more }} 件</button>
        </div>
      </div>
    </div>
  </div>
</template>

<style scoped>
.mg { display: flex; flex-direction: column; flex: 1; min-height: 0; }
.wh { display: grid; grid-template-columns: repeat(7, 1fr); text-align: center; font-size: .72rem; font-weight: 600; color: var(--muted);
  border-bottom: 1px solid var(--line); padding: .3rem 0; }
.wh .hol { color: var(--danger); }
.weeks { flex: 1; display: grid; grid-template-rows: repeat(6, minmax(96px, 1fr)); min-height: 0; overflow-y: auto; }
.week { display: grid; grid-template-columns: repeat(7, minmax(0, 1fr)); border-bottom: 1px solid var(--line); }
.cell { border-left: 1px solid var(--line); padding: 2px 3px 4px; min-width: 0; overflow: hidden; cursor: pointer; display: flex; flex-direction: column; gap: 2px; }
.cell:first-child { border-left: none; }
.cell.out { background: color-mix(in srgb, var(--hover) 50%, transparent); }
.cell.out .num { color: var(--muted); }
.cell.over { background: var(--accent-weak); }
.top { display: flex; align-items: center; gap: .3rem; min-width: 0; }
.num { border: none; background: transparent; font-size: .76rem; padding: 0 .4rem; height: 24px; min-width: 24px; border-radius: 999px; justify-content: center; }
.num:hover { background: var(--hover); }
.num.hol { color: var(--danger); }
.num.today { background: var(--accent); color: #fff; font-weight: 700; }
.hname { font-size: .64rem; color: var(--danger); white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.mchip { padding: 1px 6px; white-space: nowrap; text-overflow: ellipsis; flex: none; }
.mchip.line { color: var(--fg); background: transparent; display: block; }
.mchip.line::before { content: ""; display: inline-block; width: 8px; height: 8px; border-radius: 50%; background: var(--c); margin-right: 5px; }
.mchip.line:hover { background: var(--hover); }
.more { border: none; background: transparent; font-size: .72rem; font-weight: 600; padding: 0 6px; justify-content: flex-start; color: var(--muted); }
.more:hover { background: var(--hover); color: var(--fg); }
</style>
