package skuaka.devidp

import cats.effect.*
import cats.syntax.all.*
import com.comcast.ip4s.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityEncoder.*
import org.http4s.dsl.io.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.headers.{`Content-Type`, Authorization, Location}
import skuaka.auth.Crypto

import java.nio.charset.StandardCharsets
import java.security.interfaces.RSAPublicKey
import java.security.{KeyPairGenerator, Signature}
import java.time.Instant
import java.util.Base64

/** 発行した認可コード（60秒・1回だけ）. */
final case class Grant(clientId: String, redirectUri: String, challenge: String, nonce: Option[String], sub: String, name: String,
    email: String, groups: List[String], issuedAt: Instant)

/**
 * 開発・テスト用の簡易 OIDC プロバイダ（本番では使わないこと）.
 * Authorization Code + PKCE（S256）で、名前・メールアドレス・グループを入力した人として ID トークン（RS256）を発行する.
 * discovery・JWKS・end_session_endpoint もあるので、本物の IdP と同じ手順でログイン・ログアウトを試せる.
 */
final class DevIdp(issuer: String, clientId: String, clientSecret: String, codes: Ref[IO, Map[String, Grant]]):

  private val enc = Base64.getUrlEncoder.withoutPadding()
  private val kid = "dev-" + Crypto.randomToken(6)
  private val keys = {
    val g = KeyPairGenerator.getInstance("RSA")
    g.initialize(2048)
    g.generateKeyPair()
  }

  private def b64(bytes: Array[Byte]): String = enc.encodeToString(bytes)
  private def unsigned(n: java.math.BigInteger): Array[Byte] =
    val b = n.toByteArray
    if b.length > 1 && b(0) == 0 then b.drop(1) else b

  def jwks: Json =
    val pub = keys.getPublic.asInstanceOf[RSAPublicKey]
    Json.obj("keys" -> Json.arr(Json.obj("kty" -> "RSA".asJson, "use" -> "sig".asJson, "alg" -> "RS256".asJson, "kid" -> kid.asJson,
      "n" -> b64(unsigned(pub.getModulus)).asJson, "e" -> b64(unsigned(pub.getPublicExponent)).asJson)))

  def sign(claims: Json): String =
    val header = Json.obj("alg" -> "RS256".asJson, "typ" -> "JWT".asJson, "kid" -> kid.asJson)
    val input  = b64(header.noSpaces.getBytes(StandardCharsets.UTF_8)) + "." + b64(claims.noSpaces.getBytes(StandardCharsets.UTF_8))
    val s      = Signature.getInstance("SHA256withRSA")
    s.initSign(keys.getPrivate)
    s.update(input.getBytes(StandardCharsets.US_ASCII))
    input + "." + b64(s.sign())

  private def html(body: String): IO[Response[IO]] =
    IO.pure(Response[IO](Status.Ok).withEntity(s"""<!doctype html><html lang="ja"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width">
           |<title>開発用 IdP</title><style>body{font-family:system-ui,sans-serif;max-width:420px;margin:10vh auto;padding:0 1rem}
           |label{display:block;margin:.6rem 0}input{width:100%;padding:.4rem;font:inherit;box-sizing:border-box}
           |button{padding:.5rem 1.2rem;font:inherit}</style></head><body>$body</body></html>""".stripMargin)(using EntityEncoder.stringEncoder)
      .withContentType(`Content-Type`(MediaType.text.html, Charset.`UTF-8`)))

  private def esc(s: String): String = s.flatMap {
    case '<' => "&lt;"; case '>' => "&gt;"; case '&' => "&amp;"; case '"' => "&quot;"; case c => c.toString
  }

  private def bad(msg: String): IO[Response[IO]] = BadRequest(Json.obj("error" -> "invalid_request".asJson, "error_description" -> msg.asJson))

  private def tokenError(code: String, status: Status = Status.BadRequest): IO[Response[IO]] =
    IO.pure(Response[IO](status).withEntity(Json.obj("error" -> code.asJson)))

  private def checkClient(id: Option[String], redirect: Option[String]): Either[String, (String, String)] =
    (id, redirect) match
      case (Some(c), Some(r)) if c == clientId && (r.startsWith("http://localhost") || r.startsWith("http://127.0.0.1") || r.startsWith(issuer)) =>
        Right((c, r))
      case (Some(c), _) if c != clientId => Left(s"unknown client_id: $c")
      case _                             => Left("client_id と redirect_uri が必要です")

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / ".well-known" / "openid-configuration" =>
      Ok(Json.obj(
        "issuer" -> issuer.asJson,
        "authorization_endpoint" -> s"$issuer/authorize".asJson,
        "token_endpoint" -> s"$issuer/token".asJson,
        "jwks_uri" -> s"$issuer/jwks".asJson,
        "end_session_endpoint" -> s"$issuer/logout".asJson,
        "response_types_supported" -> List("code").asJson,
        "subject_types_supported" -> List("public").asJson,
        "id_token_signing_alg_values_supported" -> List("RS256").asJson,
        "code_challenge_methods_supported" -> List("S256").asJson,
        "scopes_supported" -> List("openid", "profile", "email").asJson
      ))

    case GET -> Root / "jwks" => Ok(jwks)

    // ログインの画面（名前・メール・グループを入れるだけ）
    case req @ GET -> Root / "authorize" =>
      val p = req.params
      checkClient(p.get("client_id"), p.get("redirect_uri")) match
        case Left(e) => bad(e)
        case Right(_) if !p.get("code_challenge_method").contains("S256") || p.get("code_challenge").forall(_.isEmpty) =>
          bad("PKCE（code_challenge_method=S256）が必要です")
        case Right(_) =>
          val hidden = List("client_id", "redirect_uri", "state", "nonce", "code_challenge").flatMap(k => p.get(k).map(v =>
            s"""<input type="hidden" name="$k" value="${esc(v)}">""")).mkString
          html(s"""<h1>開発用 IdP</h1><p>本番では使わないでください。入力した人としてログインします。</p>
                  |<form method="post" action="/authorize">$hidden
                  |<label>名前<input name="name" value="山田 太郎" autofocus></label>
                  |<label>メールアドレス<input name="email" value="yamada@example.com"></label>
                  |<label>グループ（カンマ区切り）<input name="groups" value="staff"></label>
                  |<button type="submit">ログイン</button></form>""".stripMargin)

    case req @ POST -> Root / "authorize" =>
      req.as[UrlForm].flatMap { f =>
        def v(k: String) = f.getFirst(k).map(_.trim).filter(_.nonEmpty)
        checkClient(v("client_id"), v("redirect_uri")) match
          case Left(e) => bad(e)
          case Right((c, r)) =>
            val email = v("email").getOrElse("user@example.com")
            val code  = Crypto.randomToken()
            val g = Grant(c, r, v("code_challenge").getOrElse(""), v("nonce"), "dev|" + email.toLowerCase, v("name").getOrElse(email), email,
              v("groups").toList.flatMap(_.split(',').map(_.trim).filter(_.nonEmpty)), Instant.now())
            val back = Uri.unsafeFromString(r).withQueryParam("code", code).withQueryParams(v("state").map("state" -> _).toMap)
            codes.update(_.updated(code, g)) *> IO.pure(Response[IO](Status.Found).putHeaders(Location(back)))
      }

    case req @ POST -> Root / "token" =>
      req.as[UrlForm].flatMap { f =>
        def v(k: String) = f.getFirst(k).map(_.trim).filter(_.nonEmpty)
        val basic = req.headers.get[Authorization].collect { case Authorization(BasicCredentials(u, p)) => (u, p) }
        val client = basic.orElse((v("client_id"), v("client_secret")).tupled)
        if !client.contains((clientId, clientSecret)) then tokenError("invalid_client", Status.Unauthorized)
        else if !v("grant_type").contains("authorization_code") then tokenError("unsupported_grant_type")
        else
          codes.modify(m => v("code").fold((m, None))(c => (m - c, m.get(c)))).flatMap {
            case Some(g) if g.issuedAt.plusSeconds(60).isAfter(Instant.now()) && v("redirect_uri").contains(g.redirectUri) &&
                v("code_verifier").exists(cv => Crypto.pkceChallenge(cv) == g.challenge) =>
              val now = Instant.now().getEpochSecond
              val claims = Json.obj(
                "iss" -> issuer.asJson, "sub" -> g.sub.asJson, "aud" -> clientId.asJson, "iat" -> now.asJson, "exp" -> (now + 300).asJson,
                "name" -> g.name.asJson, "email" -> g.email.asJson, "email_verified" -> true.asJson, "groups" -> g.groups.asJson
              ).deepMerge(g.nonce.fold(Json.obj())(n => Json.obj("nonce" -> n.asJson)))
              Ok(Json.obj("access_token" -> Crypto.randomToken().asJson, "token_type" -> "Bearer".asJson, "expires_in" -> 300.asJson,
                "id_token" -> sign(claims).asJson)).map(_.putHeaders("Cache-Control" -> "no-store"))
            case _ => tokenError("invalid_grant")
          }
      }

    case req @ GET -> Root / "logout" =>
      req.params.get("post_logout_redirect_uri").filter(u => u.startsWith("http://localhost") || u.startsWith("http://127.0.0.1")) match
        case Some(u) => IO.pure(Response[IO](Status.Found).putHeaders(Location(Uri.unsafeFromString(u))))
        case None    => html("<h1>ログアウトしました</h1>")
  }

object DevIdp extends IOApp.Simple:

  private given org.typelevel.log4cats.LoggerFactory[IO] = org.typelevel.log4cats.noop.NoOpFactory[IO]

  def create(issuer: String, clientId: String, clientSecret: String): IO[DevIdp] =
    Ref.of[IO, Map[String, Grant]](Map.empty).map(new DevIdp(issuer.stripSuffix("/"), clientId, clientSecret, _))

  def run: IO[Unit] =
    val port   = sys.env.get("DEVIDP_PORT").flatMap(_.toIntOption).getOrElse(9095)
    val issuer = sys.env.getOrElse("DEVIDP_ISSUER", s"http://localhost:$port")
    for
      idp <- create(issuer, sys.env.getOrElse("DEVIDP_CLIENT_ID", "skuaka"), sys.env.getOrElse("DEVIDP_CLIENT_SECRET", "skuaka-dev-secret-0123456789"))
      _ <- EmberServerBuilder.default[IO].withHost(ipv4"127.0.0.1").withPort(Port.fromInt(port).getOrElse(port"9095"))
             .withHttpApp(idp.routes.orNotFound).build
             .evalTap(_ => IO.println(s"[devidp] 開発用の OIDC プロバイダ: $issuer（本番では使わないこと）")).useForever
    yield ()
