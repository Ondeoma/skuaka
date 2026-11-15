<script lang="scala">
import skuaka.web.*
import skuaka.web.parts.*

// カレンダー: 表示の種類（日・週・月・スケジュール）ごとの画面
def mounted(): Unit = pageTitle(Store.title)

def isGrid = Store.view.mapC(v => v == "day" || v == "week")
def isMonth = Store.view.mapC(_ == "month")
def isAgenda = Store.view.mapC(_ == "agenda")
</script>

<template>
  <TimeGrid v-if="isGrid" />
  <MonthGrid v-else-if="isMonth" />
  <Agenda v-else-if="isAgenda" />
</template>
