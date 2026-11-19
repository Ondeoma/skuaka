package skuaka.web

import io.github.ondeoma.scalactive.components.ComponentManager.*

object Dialog {
  val current: RV[Option[DialogReq]] = RV(None)

  def confirm(message: String, ok: String = "OK", danger: Boolean = true)(f: => Unit): Unit =
    current := Some(DialogReq(message, ok, danger, None, Nil, _ => f, () => ()))

  def prompt(message: String, default: String = "", ok: String = "OK")(f: String => Unit): Unit =
    current := Some(DialogReq(message, ok, danger = false, Some(RV(default)), Nil, f, () => ()))

  /** 選択肢（値 → 表示名）から1つ選ぶ. */
  def choose(message: String,
             choices: List[(String, String)],
             ok: String = "OK",
             danger: Boolean = false,
             onCancel: () => Unit = () => ())(f: String => Unit): Unit =
    current :=
      Some(DialogReq(message, ok, danger, Some(RV(choices.headOption.map(_._1).getOrElse(""))), choices, f, onCancel))

  def close(ok: Boolean): Unit = current.v.foreach { r =>
    current := None
    if (ok) r.onOk(r.input.map(_.v).getOrElse("")) else r.onCancel()
  }
}
