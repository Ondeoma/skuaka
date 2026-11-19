package skuaka.api

import cats.data.OptionT
import cats.effect.*
import org.http4s.*
import org.typelevel.ci.CIString

/**
 * すべての応答に付けるセキュリティのヘッダー. CSP は応答が自分で付けていなければ、何も読み込ませないもの
 * （API・エラーの画面向け. 画面の配信は画面に合わせた CSP を付ける）. hsts は HTTPS で公開しているとき.
 */
object SecurityHeaders {
  val StrictCsp =
    "default-src 'none'; style-src 'unsafe-inline'; frame-ancestors 'none'; base-uri 'none'; form-action 'none'"

  def apply(
    routes: HttpRoutes[IO],
    hsts: Boolean
  ): HttpRoutes[IO] = {
    val fixed = List[Header.ToRaw](
      "X-Content-Type-Options" -> "nosniff",
      "X-Frame-Options" -> "DENY",
      "Referrer-Policy" -> "strict-origin-when-cross-origin"
    ) ++
      Option.when(hsts)("Strict-Transport-Security" -> "max-age=31536000").toList.map(h => h: Header.ToRaw)
    routes.map { r =>
      val withCsp = if r.headers.get(CIString("Content-Security-Policy")).isDefined then r
      else r.putHeaders("Content-Security-Policy" -> StrictCsp)
      withCsp.putHeaders(fixed*)
    }
  }
}
