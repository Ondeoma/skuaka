<script lang="scala">
import skuaka.shared.*

// 色の選択（Palette の色. auto を渡すと先頭に「既定の色」（値は ""）を出す）
case class Props(value: String, onPick: String => Unit, auto: Option[String] = None, label: String = "既定の色")

def cls(props: Props, c: String): String = if (props.value == c) "sw on" else "sw"
def bg(c: String): String = s"background: $c"
def autoBg(props: Props): String = bg(props.auto.getOrElse(Palette.Default))
def hasAuto(props: Props): Boolean = props.auto.isDefined
</script>

<template>
  <div class="swatches">
    <button v-if="hasAuto(props)" type="button" :class='cls(props, "")' :style="autoBg(props)" :title="props.label" @click='props.onPick("")'></button>
    <button v-for="c in Palette.colors" type="button" :class="cls(props, c._1)" :style="bg(c._1)" :title="c._2" @click="props.onPick(c._1)"></button>
  </div>
</template>

<style scoped>
.swatches { display: flex; flex-wrap: wrap; gap: .35rem; }
.sw { width: 26px; height: 26px; border-radius: 50%; padding: 0; border: 2px solid transparent; }
.sw.on { border-color: var(--fg); box-shadow: 0 0 0 2px var(--bg) inset; }
</style>
