package skuaka.devidp

import cats.effect.{IO, IOApp}
import sbirds.devidp.DevIdp

/** 開発・テスト用の簡易 OIDC プロバイダ（sbirds-lib の DevIdp. 本番では使わない）. */
object DevIdpMain extends IOApp.Simple {
  def run: IO[Unit] = DevIdp.serve(9095, "skuaka", "skuaka-dev-secret-0123456789")
}
