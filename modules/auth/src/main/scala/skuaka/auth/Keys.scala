package skuaka.auth

/** skuaka の API トークン（skuaka_ で始まる）. */
object ApiKeys extends sbirds.auth.ApiKeys("skuaka_")

/** skuaka の秘密の値の暗号化（鍵の salt は skuaka 固有. 変えると保存済みの値を復号できなくなる）. */
object SecretBox {
  def apply(
    secret: String
  ): sbirds.auth.SecretBox = new sbirds.auth.SecretBox(secret, "skuaka-secret-box:")
}

type SecretBox = sbirds.auth.SecretBox
