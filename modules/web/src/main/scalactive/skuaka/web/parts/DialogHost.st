<script lang="scala">
import skuaka.web.*

// 画面内のダイアログ（確認・入力・選択）. Dialog.confirm などで開く.
def okClass(d: DialogReq): String = if (d.danger) "danger-fill" else "primary"
def list = Dialog.current.mapC(_.toList)
def checked(v: RV[String], value: String) = v.mapC(_ == value)
</script>

<template>
  <Modal v-for="d in list" :onClose="_ => Dialog.close(false)">
    <form class="dlg" @submit.prevent="Dialog.close(true)">
      <p class="msg">{{ d.message }}</p>
      <template v-for="v in d.input.toList">
        <div v-if="d.choices.nonEmpty" class="choices">
          <label v-for="c in d.choices" class="choice"><input type="radio" name="dlg-choice" :checked="checked(v, c._1)" @change="v := c._1"> {{ c._2 }}</label>
        </div>
        <input v-else type="text" v-model="v" class="full" autofocus>
      </template>
      <div class="actions">
        <button type="button" @click="Dialog.close(false)">キャンセル</button>
        <button type="submit" :class="okClass(d)" autofocus>{{ d.okLabel }}</button>
      </div>
    </form>
  </Modal>
</template>

<style scoped>
.msg { white-space: pre-wrap; margin: 0 0 .8rem; line-height: 1.6; font-weight: 600; }
.full { width: 100%; }
.choices { display: flex; flex-direction: column; gap: .5rem; margin: .2rem 0 .4rem; }
.choice { display: flex; gap: .5rem; align-items: center; font-size: .95rem; cursor: pointer; }
.danger-fill { background: var(--danger); border-color: var(--danger); color: #fff; }
</style>
