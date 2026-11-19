package skuaka.web

import org.scalajs.dom
import skuaka.shared.*

import java.util.UUID

/** iCalendar（.ics）のファイルの取り込み（カレンダーを追加する画面と、カレンダーの設定で共通）. */
object IcsFile {

  /** ファイルの選択（input type=file の change）で選んだファイルを、カレンダーに取り込む. */
  def importFrom(
    e: dom.Event,
    calId: UUID
  )(
    done: => Unit
  ): Unit = {
    val input = e.target.asInstanceOf[dom.HTMLInputElement]
    Option(input.files).filter(_.length > 0).map(_(0)).foreach { f =>
      if (f.size > IcsImport.MaxBytes)
        Store.toast(s"ファイルが大きすぎます（${IcsImport.MaxBytes / 1024 / 1024}MB まで）", error = true)
      else
        Store.report(Api.importIcs(calId, f)) { r =>
          Store.toast(s"「${f.name}」を取り込みました（${r.summary}）")
          Store.load()
          done
        }
    }
    input.value = ""
  }
}
