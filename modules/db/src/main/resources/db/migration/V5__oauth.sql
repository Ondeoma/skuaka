-- ------------------------------------------------------------------ 兄弟のサービス（skakapo など）への認可（OAuth）

-- 認可で発行した API トークン: クライアント（許可し直したら前のトークンを失効させる）と、更新用の refresh token の SHA-256
alter table api_token add column oauth_client text;
alter table api_token add column refresh_hash bytea unique;
create index api_token_oauth_idx on api_token (user_id, oauth_client) where oauth_client is not null;

-- 認可コード: 60秒で失効、一度だけ使える。ハッシュだけを保存する
create table oauth_code (
  code_hash      bytea primary key,
  client_id      text not null,
  redirect_uri   text not null,
  user_id        uuid not null references app_user(id) on delete cascade,
  code_challenge text not null,
  scope          text not null,
  expires_at     timestamptz not null,
  used_at        timestamptz
);
create index oauth_code_expires_idx on oauth_code (expires_at);
