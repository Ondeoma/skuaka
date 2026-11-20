package skuaka.db

import sbirds.oauth.OAuthCodeStore
import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*

import java.time.Instant
import java.util.UUID

/** 認可で発行するトークン（API トークンの値と、更新用の refresh token）. 値はハッシュだけを保存する. */
final case class OAuthGrant(prefix: String, tokenHash: Array[Byte], refreshHash: Array[Byte], expires: Instant)

/** 認可で発行した API トークン. 認可コード（oauth_code. scope は API トークンのスコープ〈read か write〉）は sbirds.oauth.OAuthCodeStore. */
final class OAuthStore(xa: Transactor[IO]) extends OAuthCodeStore(xa) {

  /** そのユーザーにそのクライアントが以前に発行したトークンを失効させ、新しいトークンを登録する. */
  def replaceToken(userId: UUID, clientId: String, name: String, scope: String, g: OAuthGrant): IO[UUID] = tx {
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
  def rotate(refreshHash: Array[Byte], clientId: String, g: OAuthGrant): IO[Option[String]] = tx(
    sql"""update api_token t set prefix = ${g.prefix}, token_hash = ${g.tokenHash}, refresh_hash = ${g.refreshHash}, expires_at = ${g.expires}
          from app_user u
          where t.refresh_hash = $refreshHash and t.oauth_client = $clientId and t.revoked_at is null and u.id = t.user_id and not u.disabled
          returning t.scope""".query[String].option
  )

  /** そのクライアントが発行したトークンを失効させる（トークンでも refresh token でもよい. なければ何もしない）. */
  def revoke(hash: Array[Byte], clientId: String): IO[Unit] = tx(
    sql"""update api_token set revoked_at = now()
          where (token_hash = $hash or refresh_hash = $hash) and oauth_client = $clientId and revoked_at is null""".update.run.void
  )
}
