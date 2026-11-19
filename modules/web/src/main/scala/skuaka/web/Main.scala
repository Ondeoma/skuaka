package skuaka.web

import io.github.ondeoma.scalactive.ScalactiveConfig
import io.github.ondeoma.scalactive.components.ComponentManager.*
import org.scalajs.dom

/**
 * 画面の入口.
 * `@main def main()` にすると生成される `main` クラスと `Main` が、大文字小文字を区別しないファイルシステム（macOS）で
 * 同じファイル名になり差分コンパイルが壊れるため、通常の main(args) にする.
 */
object Main {

  def main(args: Array[String]): Unit = {
    ScalactiveConfig.titleTemplate = "%s | skuaka"
    ScalactiveConfig.defaultTitle = Some("skuaka")
    AppNames.init("skuaka")
    Theme.apply()
    // sbirds-lib の部品（Pwa・Observe）の要求にも CSRF 対策の X-Skuaka を付ける
    sbirds.web.Fetch.extraHeaders = Map("X-Skuaka" -> "1")
    // skorimako のブラウザの観測（設定があるときだけ）と、PWA（ホーム画面に追加・オフラインで開く・プッシュ通知）
    Observe.start()
    Pwa.register()
    App()(dom.document.body, AddMethod.appendS(IdSelector("app"))).init()
  }

}
