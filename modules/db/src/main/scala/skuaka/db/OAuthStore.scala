package skuaka.db

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*

import java.time.Instant
import java.util.UUID

/** scope は API トークンのスコープ（read か write）. */
final case class OAuthCode(
  clientId: String,
  redirectUri: String,
  userId: UUID,
  codeChallenge: String,
  scope: String)

/** 認可で発行するトークン（API トークンの値と、更新用の refresh token）. 値はハッシュだけを保存する. */
final case class OAuthGrant(
  prefix: String,
  tokenHash: Array[Byte],
  refreshHash: Array[Byte],
  expires: Instant)

/** OAuth の認可コードと、認可で発行した API トークン. */
final class OAuthStore(
  xa: Transactor[IO]) extends TxStore(xa) {

  def createCode(
    hash: Array[Byte],
    c: OAuthCode,
    expires: Instant
  ): IO[Unit] = tx(
    sql"""insert into oauth_code (code_hash, client_id, redirect_uri, user_id, code_challenge, scope, expires_at)
          values ($hash, ${c.clientId}, ${c.redirectUri}, ${c.userId}, ${c.codeChallenge}, ${c.scope}, $expires)""".update.run *>
      sql"delete from oauth_code where expires_at < now() - interval '1 hour'".update.run.void
  )

  /** 認可コードを使う（有効期限内で、まだ使っていないものだけ. 使ったら使用済みにする）. */
  def consumeCode(
    hash: Array[Byte]
  ): IO[Option[OAuthCode]] = tx(
    sql"""update oauth_code set used_at = now() where code_hash = $hash and used_at is null and expires_at > now()
          returning client_id, redirect_uri, user_id, code_challenge, scope""".query[OAuthCode].option
  )

  /** そのユーザーにそのクライアントが以前に発行したトークンを失効させ、新しいトークンを登録する. */
  def replaceToken(
    userId: UUID,
    clientId: String,
    name: String,
    scope: String,
    g: OAuthGrant
  ): IO[UUID] = tx {
    val id = UUID.randomUUID()
    sql"update api_token set revoked_at = now() where user_id = $userId and oauth_client = $clientId and revoked_at is null".update.run *>
      sql"""insert into api_token (id, user_id, name, prefix, token_hash, scope, expires_at, oauth_client, refresh_hash)
            values ($id, $userId, $name, ${g.prefix}, ${g.tokenHash}, $scope, ${g.expires}, $clientId, ${g.refreshHash})""".update.run.as(
        id
      )
  }

  /**
   * refresh token で、トークンと refresh token を新しくする（前のものは使えなくなる）. 返すのはスコープ.
   * 失効したもの・別のクライアントのもの・持ち主が無効にされたものは None.
   */
  def rotate(
    refreshHash: Array[Byte],
    clientId: String,
    g: OAuthGrant
  ): IO[Option[String]] = tx(
    sql"""update api_token t set prefix = ${g.prefix}, token_hash = ${g.tokenHash}, refresh_hash = ${g.refreshHash}, expires_at = ${g.expires}
          from app_user u
          where t.refresh_hash = $refreshHash and t.oauth_client = $clientId and t.revoked_at is null and u.id = t.user_id and not u.disabled
          returning t.scope""".query[String].option
  )

  /** そのクライアントが発行したトークンを失効させる（トークンでも refresh token でもよい. なければ何もしない）. */
  def revoke(
    hash: Array[Byte],
    clientId: String
  ): IO[Unit] = tx(
    sql"""update api_token set revoked_at = now()
          where (token_hash = $hash or refresh_hash = $hash) and oauth_client = $clientId and revoked_at is null""".update.run.void
  )
}
