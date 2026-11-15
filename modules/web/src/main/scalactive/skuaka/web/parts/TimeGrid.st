<script lang="scala">
import skuaka.web.*
import skuaka.shared.*
import org.scalajs.dom

import scala.scalajs.js

// 日・週の表示: 時間の表. 空いたところをドラッグして予定を作り、予定をドラッグで移動、下の端で伸縮する.
// 終日・24時間以上の予定は上の「終日」の欄に帯で出す.

val HourPx = 48
val Snap = 15

/** 時間の表の予定（その日の中の位置. 重なる予定は横に並べる）. */
final case class Seg(o: OccurrenceDto, top: Double, height: Double, col: Int, cols: Int, span: Int, cutTop: Boolean, cutBottom: Boolean)
/** 終日の欄の帯（何日目から何日分、何段目）. */
final case class Bar(o: OccurrenceDto, from: Int, span: Int, lane: Int, cutL: Boolean, cutR: Boolean)
/** 列（日）. */
final case class Col(day: String, segs: List[Seg])
/** ドラッグ中（kind: new 作成 / move 移動 / resize 伸縮）. day0・min0 は押した位置、day・min は今の位置. */
final case class Drag(kind: String, o: Option[OccurrenceDto], day0: String, min0: Int, day: String, min: Int, moved: Boolean)

case class State(drag: RV[Option[Drag]] = RV(None), now: RV[Double] = RV(js.Date.now()),
                 body: ElementRef[dom.HTMLElement] = ElementRef(), cols: ElementRef[dom.HTMLElement] = ElementRef())

def initState() = State()

private var timer: Option[Int] = None

def mounted(s: State): Unit = {
  // 最初は朝8時あたりを見せる（今日を含むなら今の時刻の少し前）
  dom.window.setTimeout(() => s.body.foreach { b =>
    val nowMin = Cal.minutesOf(new js.Date().toISOString())
    val m = if (dayList.contains(Cal.today())) math.max(0, nowMin - 120) else 8 * 60
    b.scrollTop = m / 60.0 * HourPx
  }, 0)
  timer = Some(dom.window.setInterval(() => s.now := js.Date.now(), 60000))
}

def unmounted(): Unit = timer.foreach(dom.window.clearInterval)

def dayList: List[String] = if (Store.view.v == "day") List(Store.date.v) else Store.weekDays(Store.date.v)

def days = computed(dayList)
def gridCols = computed(s"grid-template-columns: 56px repeat(${dayList.size}, minmax(0, 1fr))")
def dayCols = computed(s"grid-template-columns: repeat(${dayList.size}, minmax(0, 1fr))")
def hours: List[Int] = (0 until 24).toList

def ms(iso: String): Double = new js.Date(iso).getTime()

/** 終日の欄に出す予定か. */
def isLong(o: OccurrenceDto): Boolean = o.allDay || ms(o.end) - ms(o.start) >= 86400000.0

def same(a: OccurrenceDto, b: OccurrenceDto): Boolean = a.eventId == b.eventId && a.occurrence == b.occurrence

/** ドラッグを反映した開始・終わり（ISO）. */
def moved(o: OccurrenceDto, d: Drag): (String, String) = {
  val dd = Cal.diffDays(d.day0, d.day)
  val dm = d.min - d.min0
  d.kind match {
    case "move" =>
      val st = Cal.instant(Cal.addDays(Cal.dateOf(o.start), dd), Cal.minutesOf(o.start) + dm)
      (st, new js.Date(ms(st) + ms(o.end) - ms(o.start)).toISOString())
    case _ =>
      val en = Cal.instant(Cal.addDays(Cal.dateOf(o.end), dd), Cal.minutesOf(o.end) + dm)
      (o.start, if (ms(en) - ms(o.start) < Snap * 60000.0) new js.Date(ms(o.start) + Snap * 60000.0).toISOString() else en)
  }
}

/** 表示する予定（ドラッグ中の予定は動かした位置に）. */
def current(s: State): List[OccurrenceDto] = {
  val d = s.drag.v
  Store.shown.v.map { o =>
    d match {
      case Some(x) if x.moved && x.kind != "new" && x.o.exists(same(_, o)) => val (a, b) = moved(o, x); o.copy(start = a, end = b)
      case _ => o
    }
  }
}

/** 列に並べる前の予定（その日の0時からの分. col は入れた列）. */
final case class Item(o: OccurrenceDto, top: Double, bottom: Double, cutTop: Boolean, cutBottom: Boolean, col: Int = 0) {
  def overlaps(x: Item): Boolean = top < x.bottom && x.top < bottom
}

/** 重なる予定のまとまりを列に並べる. 右の列が空いていれば広げる. */
def layout(cluster: List[Item]): List[Seg] = {
  val n = if (cluster.isEmpty) 1 else cluster.map(_.col).max + 1
  cluster.map { c =>
    val span = Iterator.range(c.col + 1, n).takeWhile(j => !cluster.exists(x => x.col == j && x.overlaps(c))).size + 1
    Seg(c.o, c.top / 60 * HourPx, (c.bottom - c.top) / 60 * HourPx, c.col, n, span, c.cutTop, c.cutBottom)
  }
}

/** 列ごとの予定の位置. 重なる予定のまとまりごとに、空いている列へ順に入れる. */
def columns(s: State) = computed {
  val timed = current(s).filterNot(isLong)
  dayList.map { d =>
    val d0 = Cal.parse(d).getTime()
    val d1 = Cal.parse(Cal.addDays(d, 1)).getTime()
    val items = timed.filter(o => ms(o.start) < d1 && ms(o.end) > d0).map { o =>
      val a = math.max(ms(o.start), d0); val b = math.min(ms(o.end), d1)
      // 短い予定も読めるよう、20分ぶんの高さは取る
      Item(o, (a - d0) / 60000.0, (a - d0 + math.max(b - a, 20 * 60000.0)) / 60000.0, ms(o.start) < d0, ms(o.end) > d1)
    }.sortBy(x => (x.top, -x.bottom))
    val (done, last, _) = items.foldLeft((List.empty[Seg], List.empty[Item], -1.0)) { case ((out, cluster, end), it) =>
      val (out2, cl) = if (it.top >= end) (out ++ layout(cluster), Nil) else (out, cluster)
      val used = cl.filter(_.bottom > it.top).map(_.col).toSet
      val placed = it.copy(col = Iterator.from(0).find(c => !used.contains(c)).get)
      (out2, cl :+ placed, if (cl.isEmpty) placed.bottom else math.max(end, placed.bottom))
    }
    Col(d, done ++ layout(last))
  }
}

/** 終日の欄の帯. 前から順に、空いている段へ入れる. */
def bars(s: State) = computed {
  val ds = dayList
  val longs = current(s).filter(isLong).flatMap { o =>
    val (sd, ed) = (Store.firstDay(o), Store.lastDay(o))
    val idx = ds.zipWithIndex.filter((d, _) => d >= sd && d <= ed).map(_._2)
    Option.when(idx.nonEmpty)((o, idx.min, idx.max - idx.min + 1, sd < ds.head, ed > ds.last))
  }.sortBy(x => (x._2, -x._3))
  var lanes = Vector.empty[Int] // 段ごとの、使っている最後の列
  longs.map { (o, from, span, cl, cr) =>
    val lane = lanes.indexWhere(_ < from) match {
      case -1 => lanes = lanes :+ (from + span - 1); lanes.size - 1
      case i  => lanes = lanes.updated(i, from + span - 1); i
    }
    Bar(o, from, span, lane, cl, cr)
  }
}

def alldayStyle(s: State) = bars(s).mapC(bs => s"height: ${math.max(1, bs.map(_.lane + 1).maxOption.getOrElse(0)) * 24 + 6}px")

def barStyle(b: Bar): String = {
  val n = dayList.size
  s"left: calc(${b.from * 100.0 / n}% + 2px); width: calc(${b.span * 100.0 / n}% - 5px); top: ${b.lane * 24 + 3}px; background: ${Store.color(b.o)}"
}

def segStyle(g: Seg): String = {
  val w = 100.0 / g.cols
  s"top: ${g.top}px; height: ${math.max(g.height - 2, 14)}px; left: calc(${g.col * w}% + 1px); width: calc(${w * g.span}% - 4px); background: ${Store.color(g.o)}"
}

def segClass(s: State, g: Seg) = s.drag.mapC { d =>
  Store.chipClass(g.o, "ev seg") + (if (d.exists(x => x.moved && x.o.exists(same(_, g.o)))) " dragging" else "") +
    (if (g.height < 34) " short" else "")
}

def headClass(d: String): String =
  Holidays.dayClass("dh", d)

def colClass(d: String): String = if (Holidays.off(d)) "col off" else "col"

/** 今の時刻の線（今日の列だけ）. */
def nowStyle(s: State, d: String) = s.now.mapC { _ =>
  if (d != Cal.today()) "display: none" else s"top: ${Cal.minutesOf(new js.Date().toISOString()) / 60.0 * HourPx}px"
}

/** 作成のドラッグ中の範囲. */
def selStyle(s: State, d: String) = s.drag.mapC {
  case Some(x) if x.kind == "new" && x.day0 == d =>
    val (a, b) = (math.min(x.min0, x.min), math.max(x.min0, x.min) + Snap)
    s"display: block; top: ${a / 60.0 * HourPx}px; height: ${(b - a) / 60.0 * HourPx}px"
  case _ => "display: none"
}
def selText(s: State) = s.drag.mapC {
  case Some(x) if x.kind == "new" => { val (a, b) = (math.min(x.min0, x.min), math.max(x.min0, x.min) + Snap); s"${Cal.hm(a)} – ${Cal.hm(b)}" }
  case _ => ""
}

def segText(g: Seg): String = Store.timeText(g.o)

// ---------------------------------------------------------------- ドラッグ

/** 横の位置の日（日の列が並んだ要素の中で）. */
def dayAt(r: dom.DOMRect, clientX: Double): String = {
  val ds = dayList
  ds(math.max(0, math.min(ds.size - 1, ((clientX - r.left) / (r.width / ds.size)).toInt)))
}

/** ポインタの位置の日と分（15分単位）. */
def at(s: State, e: dom.MouseEvent): Option[(String, Int)] = s.cols.get.map { el =>
  val r = el.getBoundingClientRect()
  (dayAt(r, e.clientX), math.max(0, math.min(24 * 60 - Snap, (((e.clientY - r.top) / HourPx * 60) / Snap).toInt * Snap)))
}

private var moveL: js.Function1[dom.PointerEvent, Unit] = null
private var upL: js.Function1[dom.PointerEvent, Unit] = null

private def listen(s: State): Unit = {
  moveL = (e: dom.PointerEvent) => s.drag.v.foreach { d =>
    at(s, e).foreach { (day, min) =>
      if (day != d.day || min != d.min) s.drag := Some(d.copy(day = day, min = min, moved = true))
    }
  }
  upL = (_: dom.PointerEvent) => {
    dom.window.removeEventListener("pointermove", moveL)
    dom.window.removeEventListener("pointerup", upL)
    finish(s)
  }
  dom.window.addEventListener("pointermove", moveL)
  dom.window.addEventListener("pointerup", upL)
}

/** 空いたところを押す: 作成の範囲を選び始める. */
def downCol(s: State, e: dom.Event): Unit = {
  val pe = e.asInstanceOf[dom.PointerEvent]
  if (pe.button == 0 && Store.defaultCalendar.isDefined) at(s, pe).foreach { (day, min) =>
    pe.preventDefault()
    s.drag := Some(Drag("new", None, day, min, day, min, moved = false))
    listen(s)
  }
}

/** 予定を押す: 移動（下の端なら伸縮）. 動かさずに離したら詳細を開く. */
def downSeg(s: State, g: Seg, kind: String, e: dom.Event): Unit = {
  val pe = e.asInstanceOf[dom.PointerEvent]
  if (pe.button == 0) {
    pe.stopPropagation()
    pe.preventDefault()
    at(s, pe).foreach { (day, min) =>
      s.drag := Some(Drag(if (g.o.canEdit) kind else "view", Some(g.o), day, min, day, min, moved = false))
      listen(s)
    }
  }
}

private def finish(s: State): Unit = s.drag.v.foreach { d =>
  (d.kind, d.o) match {
    case ("new", _) =>
      s.drag := None
      val (a, b) = (math.min(d.min0, d.min), math.max(d.min0, d.min) + Snap)
      Store.create(d.day0, a, if (d.moved) b else a + 60)
    case (_, Some(o)) if !d.moved || d.kind == "view" || (d.day == d.day0 && d.min == d.min0) =>
      s.drag := None
      Store.detail := Some(o)
    case (_, Some(o)) =>
      val (a, b) = moved(o, d)
      // 保存が終わるまでは動かした位置に見せておく（取り消したら元に戻す）
      Store.move(o, a, b)(s.drag := None)
      dom.window.setTimeout(() => s.drag := None, 400)
    case _ => s.drag := None
  }
}

def openBar(b: Bar): Unit = Store.detail := Some(b.o)

/** 終日の欄の空いたところ: その日の終日の予定を作る. */
def clickAllday(s: State, e: dom.Event): Unit = {
  val r = e.currentTarget.asInstanceOf[dom.HTMLElement].getBoundingClientRect()
  Store.create(dayAt(r, e.asInstanceOf[dom.MouseEvent].clientX), allDay = true)
}

def gmt: String = {
  val off = -new js.Date().getTimezoneOffset().toInt
  val h = off / 60; val m = math.abs(off % 60)
  "GMT" + (if (off >= 0) "+" else "-") + math.abs(h) + (if (m > 0) f":$m%02d" else "")
}
</script>

<template>
  <div class="tg">
    <div class="head" :style="gridCols">
      <div class="gutter tz">{{ gmt }}</div>
      <button v-for="d in days" :key="d" :class="headClass(d)" @click="Store.openDay(d)" :title='Holidays.name(d).getOrElse("")'>
        <span class="dw">{{ Cal.WeekNames(Cal.dow(d)) }}</span>
        <span class="dn">{{ Cal.day(d) }}</span>
        <span class="hn">{{ Holidays.name(d).getOrElse("") }}</span>
      </button>
    </div>
    <div class="allday-row">
      <div class="gutter small muted">終日</div>
      <div class="allday" :style="alldayStyle(s)" @click.self="clickAllday(s, event)">
        <div v-for="b in bars(s)" :class='Store.chipClass(b.o, "ev bar")' :style="barStyle(b)" @click="openBar(b)" :title="b.o.title">
          <span v-if="b.cutL">◂ </span>{{ b.o.title }}<span v-if="b.cutR"> ▸</span>
        </div>
      </div>
    </div>
    <div class="body" ref="s.body">
      <div class="grid">
        <div class="hours">
          <div v-for="h in hours" class="hour"><span v-if="h > 0">{{ Cal.hm(h * 60) }}</span></div>
        </div>
        <div class="cols" ref="s.cols" :style="dayCols">
          <div v-for="c in columns(s)" :key="c.day" :class="colClass(c.day)" @pointerdown="downCol(s, event)">
            <div v-for="g in c.segs" :class="segClass(s, g)" :style="segStyle(g)" @pointerdown='downSeg(s, g, "move", event)'>
              <div class="t">{{ g.o.title }}</div>
              <div class="tm">{{ segText(g) }}<template v-if="g.o.location.nonEmpty">、{{ g.o.location }}</template></div>
              <span v-if="g.o.canEdit" class="grip" @pointerdown='downSeg(s, g, "resize", event)'></span>
            </div>
            <div class="sel" :style="selStyle(s, c.day)">{{ selText(s) }}</div>
            <div class="now" :style="nowStyle(s, c.day)"></div>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<style scoped>
.tg { display: flex; flex-direction: column; flex: 1; min-height: 0; }
.head, .allday-row { display: grid; border-bottom: 1px solid var(--line); }
.allday-row { grid-template-columns: 56px 1fr; }
.gutter { display: flex; align-items: flex-end; justify-content: flex-end; padding: 0 .45rem .3rem 0; font-size: .66rem; color: var(--muted); }
.tz { align-items: flex-end; }
.dh { border: none; border-radius: 0; background: transparent; display: flex; flex-direction: column; align-items: center; gap: 0; padding: .4rem 0 .3rem;
  min-width: 0; }
.dh:hover { background: var(--hover); }
.dw { font-size: .72rem; color: var(--muted); font-weight: 600; }
.dn { font-size: 1.5rem; width: 2.6rem; height: 2.6rem; border-radius: 999px; display: flex; align-items: center; justify-content: center; line-height: 1; }
.hn { font-size: .62rem; color: var(--danger); height: .9rem; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; max-width: 100%; }
.dh.hol .dw, .dh.hol .dn { color: var(--danger); }
.dh.today .dw { color: var(--accent); }
.dh.today .dn { background: var(--accent); color: #fff; }
.allday { position: relative; border-left: 1px solid var(--line); min-height: 28px; cursor: pointer; }
.bar { position: absolute; height: 21px; padding: 2px .5rem; white-space: nowrap; text-overflow: ellipsis; }
.body { flex: 1; min-height: 0; overflow-y: auto; position: relative; }
.grid { display: grid; grid-template-columns: 56px 1fr; }
.hours { position: relative; }
.hour { height: 48px; position: relative; }
.hour span { position: absolute; top: -.55rem; right: .45rem; font-size: .66rem; color: var(--muted); }
.cols { display: grid; position: relative; }
.col { position: relative; height: calc(48px * 24); border-left: 1px solid var(--line); touch-action: pan-y;
  background-image: linear-gradient(to bottom, var(--line) 1px, transparent 1px); background-size: 100% 48px; }
.col.off { background-color: color-mix(in srgb, var(--hover) 45%, transparent); }
.seg { position: absolute; padding: 2px 6px; border: 1px solid var(--bg); touch-action: none; z-index: 1; }
.seg:hover { z-index: 3; box-shadow: var(--shadow); }
.seg .t { font-weight: 600; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.seg .tm { font-size: .7rem; opacity: .92; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.seg.short { display: flex; gap: .4rem; align-items: center; padding-top: 0; padding-bottom: 0; }
.seg.short .t { flex: none; max-width: 60%; }
.seg.dragging { opacity: .85; box-shadow: var(--shadow); z-index: 5; cursor: grabbing; }
.grip { position: absolute; left: 0; right: 0; bottom: 0; height: 7px; cursor: ns-resize; }
.sel { position: absolute; left: 2px; right: 4px; background: var(--accent); color: #fff; border-radius: 5px; font-size: .74rem; padding: 2px 6px;
  z-index: 4; opacity: .85; pointer-events: none; box-shadow: var(--shadow); }
.now { position: absolute; left: -1px; right: 0; height: 2px; background: #ea4335; z-index: 6; pointer-events: none; }
.now::before { content: ""; position: absolute; left: -6px; top: -5px; width: 12px; height: 12px; border-radius: 50%; background: #ea4335; }
</style>
