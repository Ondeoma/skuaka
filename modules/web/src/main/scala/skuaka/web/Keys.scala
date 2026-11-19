package skuaka.web

import io.github.ondeoma.scalactive.components.ComponentManager.*
import org.scalajs.dom

/** キーボードの操作（入力欄の外で）: t 今日・j/k 次/前・d/w/m/a 表示・c 作成・/ 検索・? 一覧. */
object Keys {
  val help: RV[Boolean] = RV(false)

  val list: List[(String, String)] = List(
    "t" -> "今日",
    "j / n" -> "次の期間",
    "k / p" -> "前の期間",
    "d" -> "日",
    "w" -> "週",
    "m" -> "月",
    "a" -> "スケジュール",
    "c" -> "予定を作成",
    "/" -> "検索",
    "?" -> "この一覧"
  )

  def install(): Unit = dom.document.addEventListener(
    "keydown",
    (e: dom.KeyboardEvent) => {
      val tag = Option(e.target.asInstanceOf[dom.Element]).map(_.tagName.toLowerCase).getOrElse("")
      val busy = Set("input", "textarea", "select").contains(tag) || e.ctrlKey || e.metaKey || e.altKey ||
        Store.editor.v.isDefined || Store.detail.v.isDefined || Store.calSettings.v.isDefined || Store.teamPicker.v ||
        Store.addCalendar.v || Dialog.current.v.isDefined
      if (!busy && dom.window.location.pathname == Store.home) {
        val handled = e.key match {
          case "t" => Store.date := Cal.today(); true
          case "j" | "n" => Store.step(1); true
          case "k" | "p" => Store.step(-1); true
          case "d" => Store.setView("day"); true
          case "w" => Store.setView("week"); true
          case "m" => Store.setView("month"); true
          case "a" => Store.setView("agenda"); true
          case "c" => Store.create(Store.date.v); true
          case "/" =>
            Option(dom.document.querySelector("header .search input")).foreach(_.asInstanceOf[dom.HTMLElement].focus());
            true
          case "?" => help := !help.v; true
          case _ => false
        }
        if (handled) e.preventDefault()
      }
    }
  )
}
