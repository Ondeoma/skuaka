package skuaka.web

import org.scalajs.dom
import skuaka.shared.*

import java.util.UUID

/** iCalendar（.ics）のファイルの取り込み（カレンダーを追加する画面と、カレンダーの設定で共通）. */
object IcsFile {

  /** ファイルの選択（input type=file の change）で選んだファイルを、カレンダーに取り込む. */
  def importFrom(e: dom.Event, calId: UUID)(done: => Unit): Unit = {
    Dom.takeFiles(e).headOption.foreach { f =>
      if (f.size > IcsImport.MaxBytes)
        Toasts.show(s"ファイルが大きすぎます（${IcsImport.MaxBytes / 1024 / 1024}MB まで）", error = true)
      else
        Toasts.report(Api.importIcs(calId, f)) { r =>
          Toasts.show(s"「${f.name}」を取り込みました（${r.summary}）")
          Store.load()
          done
        }
    }
  }
}
