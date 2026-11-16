package skuaka.auth

import java.nio.charset.StandardCharsets
import java.security.{MessageDigest, SecureRandom}
import java.util.Base64
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

/** 乱数のトークン・ハッシュ・署名。 */
object Crypto:
  private val rnd = new SecureRandom()
  private val b64 = Base64.getUrlEncoder.withoutPadding()

  def randomToken(bytes: Int = 32): String =
    val a = new Array[Byte](bytes)
    rnd.nextBytes(a)
    b64.encodeToString(a)

  def sha256(s: String): Array[Byte] = MessageDigest.getInstance("SHA-256").digest(s.getBytes(StandardCharsets.UTF_8))

  def hex(b: Array[Byte]): String = b.map(x => f"${x & 0xff}%02x").mkString

  def pkceChallenge(verifier: String): String = b64.encodeToString(sha256(verifier))

  def hmacSha256(key: String, data: Array[Byte]): Array[Byte] =
    val mac = Mac.getInstance("HmacSHA256")
    mac.init(new SecretKeySpec(key.getBytes(StandardCharsets.UTF_8), "HmacSHA256"))
    mac.doFinal(data)

  def hmacHex(key: String, data: Array[Byte]): String = hex(hmacSha256(key, data))

  /** 時間に依存しない比較（署名の検証用）。 */
  def constantTimeEquals(a: String, b: String): Boolean =
    MessageDigest.isEqual(a.getBytes(StandardCharsets.UTF_8), b.getBytes(StandardCharsets.UTF_8))

/** API トークン. 見分けやすいよう skuaka_ で始める. */
object ApiTokens:
  val Prefix = "skuaka_"

  /** (トークン, 画面に出す先頭部分) */
  def generate(): (String, String) =
    val t = Prefix + Crypto.randomToken(30)
    (t, t.take(Prefix.length + 6))

  def looksLike(s: String): Boolean = s.startsWith(Prefix)

/** 秘密の値（外部サービスの OAuth トークン）の暗号化. APP_SECRET から鍵を作る（AES-256-GCM）. */
final class SecretBox(secret: String):
  private val key = new javax.crypto.spec.SecretKeySpec(
    MessageDigest.getInstance("SHA-256").digest(("skuaka-secret-box:" + secret).getBytes(StandardCharsets.UTF_8)), "AES")
  private val rnd = new SecureRandom()

  /** "v1:" + base64(iv + 暗号文) */
  def encrypt(plain: String): String =
    val iv = new Array[Byte](12)
    rnd.nextBytes(iv)
    val c = javax.crypto.Cipher.getInstance("AES/GCM/NoPadding")
    c.init(javax.crypto.Cipher.ENCRYPT_MODE, key, new javax.crypto.spec.GCMParameterSpec(128, iv))
    "v1:" + Base64.getEncoder.encodeToString(iv ++ c.doFinal(plain.getBytes(StandardCharsets.UTF_8)))

  /** 鍵が違う（APP_SECRET を変えた）・壊れていれば None. */
  def decrypt(enc: String): Option[String] =
    scala.util.Try {
      val raw = Base64.getDecoder.decode(enc.stripPrefix("v1:"))
      val c   = javax.crypto.Cipher.getInstance("AES/GCM/NoPadding")
      c.init(javax.crypto.Cipher.DECRYPT_MODE, key, new javax.crypto.spec.GCMParameterSpec(128, raw.take(12)))
      new String(c.doFinal(raw.drop(12)), StandardCharsets.UTF_8)
    }.toOption
