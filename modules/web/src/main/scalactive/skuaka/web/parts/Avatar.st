<script lang="scala">
import skuaka.shared.UserBrief

// ユーザーのアイコン（画像がなければ名前の1文字目）
case class Props(user: Option[UserBrief], size: Int = 24)

def initial(props: Props): String = props.user.map(_.name.trim.take(1)).getOrElse("?")
def title(props: Props): String = props.user.map(_.name).getOrElse("")
def style(props: Props): String = props.user.fold("background: var(--line)") { u =>
  s"background: hsl(${math.abs(u.id.hashCode) % 360} 55% 46%)"
} + s"; width: ${props.size}px; height: ${props.size}px; font-size: ${props.size * 0.45}px"
def picture(props: Props): Option[String] = props.user.flatMap(_.avatarUrl)
</script>

<template>
  <span class="avatar" :title="title(props)" :style="style(props)">
    <img v-if="picture(props).isDefined" :src='picture(props).getOrElse("")' alt="" referrerpolicy="no-referrer">
    <template v-else>{{ initial(props) }}</template>
  </span>
</template>
