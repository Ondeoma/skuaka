package skuaka.api

import cats.effect.*

import java.net.{Inet4Address, Inet6Address, InetAddress, URI}

/**
 * 購読の URL を読みに行ってよいかの確認（SSRF の対策）. 既定は外部に公開されたアドレスだけを許す
 * （社内のネットワーク・localhost・クラウドのメタデータ（169.254.169.254）などをサーバーから読ませない）.
 */
trait UrlGuard {
  def check(uri: URI): IO[Either[String, Unit]]
}

object UrlGuard {
  val NotPublic = "社内のネットワークなど、外部に公開されていないアドレスは購読できません"

  /** 外部に公開されたアドレスだけを許す（ホスト名を解決し、どれか1つでも内部のアドレスなら断る）. */
  val publicOnly: UrlGuard = uri =>
    IO.blocking(InetAddress.getAllByName(uri.getHost).toList).attempt.map {
      case Left(_) => Left("ホスト名を解決できません")
      case Right(as) if as.exists(blocked) => Left(NotPublic)
      case Right(_) => Right(())
    }

  /** どこでも許す（開発環境・テスト用）. */
  val allowAll: UrlGuard = _ => IO.pure(Right(()))

  /** 内部・特別な用途のアドレスか. */
  def blocked(a: InetAddress): Boolean = {
    val b = a.getAddress.map(_ & 0xff)
    a.isLoopbackAddress || a.isAnyLocalAddress || a.isLinkLocalAddress || a.isSiteLocalAddress ||
    a.isMulticastAddress ||
    (a match {
      case _: Inet4Address => b(0) == 0 ||
        (b(0) == 100 && (b(1) & 0xc0) == 64) ||
        (b(0) == 192 && b(1) == 0 && b(2) == 0) || b(0) >= 240
      case v6: Inet6Address =>
        (b(0) & 0xfe) == 0xfc || // ユニークローカル（fc00::/7）
        (b.take(10).forall(_ == 0) && b(10) == 0xff && b(11) == 0xff &&
          blocked(InetAddress.getByAddress(a.getAddress.drop(12)))) // ::ffff:10.0.0.1
      case _ => false
    })
  }
}
