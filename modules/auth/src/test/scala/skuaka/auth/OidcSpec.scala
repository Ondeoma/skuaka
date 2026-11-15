package skuaka.auth

import io.circe.Json
import io.circe.syntax.*

import java.nio.charset.StandardCharsets
import java.security.{KeyPairGenerator, Signature}
import java.security.interfaces.{ECPublicKey, RSAPublicKey}
import java.time.Instant
import java.util.Base64

class OidcSpec extends munit.FunSuite:

  private val enc = Base64.getUrlEncoder.withoutPadding()
  private def b64(b: Array[Byte]) = enc.encodeToString(b)
  private def unsigned(n: java.math.BigInteger) = { val b = n.toByteArray; if b.length > 1 && b(0) == 0 then b.drop(1) else b }
  private def pad(b: Array[Byte], n: Int) = Array.fill[Byte](math.max(0, n - b.length))(0) ++ b

  private val cfg = OidcConfig("https://idp.example.com", "skuaka", "client-secret-0123456789", "https://cal.example.com/auth/callback")
  private val now = Instant.parse("2026-11-15T00:00:00Z")
  private def claims(nonce: String = "n1", aud: Json = "skuaka".asJson, exp: Long = now.getEpochSecond + 300) = Json.obj(
    "iss" -> cfg.issuer.asJson, "sub" -> "u1".asJson, "aud" -> aud, "exp" -> exp.asJson, "nonce" -> nonce.asJson,
    "name" -> "山田".asJson, "email" -> "y@example.com".asJson, "groups" -> List("staff").asJson)

  private def jwt(alg: String, kid: String, payload: Json)(sign: Array[Byte] => Array[Byte]): String =
    val input = b64(Json.obj("alg" -> alg.asJson, "kid" -> kid.asJson).noSpaces.getBytes(StandardCharsets.UTF_8)) + "." +
      b64(payload.noSpaces.getBytes(StandardCharsets.UTF_8))
    input + "." + b64(sign(input.getBytes(StandardCharsets.US_ASCII)))

  private val rsa = { val g = KeyPairGenerator.getInstance("RSA"); g.initialize(2048); g.generateKeyPair() }
  private val rsaJwk = {
    val p = rsa.getPublic.asInstanceOf[RSAPublicKey]
    Json.obj("kty" -> "RSA".asJson, "kid" -> "r1".asJson, "use" -> "sig".asJson, "n" -> b64(unsigned(p.getModulus)).asJson,
      "e" -> b64(unsigned(p.getPublicExponent)).asJson)
  }
  private def rs256(payload: Json, kid: String = "r1") = jwt("RS256", kid, payload) { in =>
    val s = Signature.getInstance("SHA256withRSA"); s.initSign(rsa.getPrivate); s.update(in); s.sign()
  }

  test("RS256 の署名を検証する（改ざん・別の鍵・alg=none は拒否）") {
    val t = rs256(claims())
    assertEquals(Jws.verify(t, List(rsaJwk), cfg.clientSecret), Right(()))
    val parts = t.split('.')
    val forged = parts(0) + "." + b64(claims().deepMerge(Json.obj("sub" -> "admin".asJson)).noSpaces.getBytes(StandardCharsets.UTF_8)) + "." + parts(2)
    assert(Jws.verify(forged, List(rsaJwk), cfg.clientSecret).isLeft)
    assert(Jws.verify(rs256(claims(), kid = "other"), List(rsaJwk), cfg.clientSecret).isLeft)
    val none = b64("""{"alg":"none"}""".getBytes) + "." + parts(1) + "."
    assert(Jws.verify(none, List(rsaJwk), cfg.clientSecret).isLeft)
  }

  test("ES256・HS256 の署名を検証する") {
    val g = KeyPairGenerator.getInstance("EC"); g.initialize(new java.security.spec.ECGenParameterSpec("secp256r1"))
    val ec = g.generateKeyPair()
    val p = ec.getPublic.asInstanceOf[ECPublicKey]
    val jwk = Json.obj("kty" -> "EC".asJson, "kid" -> "e1".asJson, "crv" -> "P-256".asJson,
      "x" -> b64(pad(unsigned(p.getW.getAffineX), 32)).asJson, "y" -> b64(pad(unsigned(p.getW.getAffineY), 32)).asJson)
    val es = jwt("ES256", "e1", claims()) { in =>
      val s = Signature.getInstance("SHA256withECDSAinP1363Format"); s.initSign(ec.getPrivate); s.update(in); s.sign()
    }
    assertEquals(Jws.verify(es, List(jwk), cfg.clientSecret), Right(()))
    val hs = jwt("HS256", "", claims())(in => Crypto.hmacSha256(cfg.clientSecret, in))
    assertEquals(Jws.verify(hs, Nil, cfg.clientSecret), Right(()))
    assert(Jws.verify(hs, Nil, "another-secret").isLeft)
  }

  test("ID トークンの iss・aud・azp・exp・nonce を確かめる") {
    assertEquals(IdToken.validate(claims(), cfg.issuer, cfg, "n1", now).map(i => (i.sub, i.name, i.groups)), Right(("u1", "山田", List("staff"))))
    assert(IdToken.validate(claims(nonce = "x"), cfg.issuer, cfg, "n1", now).isLeft)
    assert(IdToken.validate(claims(aud = "other".asJson), cfg.issuer, cfg, "n1", now).isLeft)
    assert(IdToken.validate(claims(exp = now.getEpochSecond - 120), cfg.issuer, cfg, "n1", now).isLeft)
    assert(IdToken.validate(claims(), "https://evil.example.com", cfg, "n1", now).isLeft)
    // aud が複数なら azp が必要
    assert(IdToken.validate(claims(aud = List("skuaka", "x").asJson), cfg.issuer, cfg, "n1", now).isLeft)
    assert(IdToken.validate(claims(aud = List("skuaka", "x").asJson).deepMerge(Json.obj("azp" -> "skuaka".asJson)), cfg.issuer, cfg, "n1", now).isRight)
  }
