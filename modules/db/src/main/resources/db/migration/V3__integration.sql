-- 外部サービス（skaka: Web 会議）との連携. 各ユーザーが OAuth 2.0（認可コード + PKCE）で許可し、受け取ったトークンを暗号化して保存する.

-- 許可の途中の状態（10分・1回だけ）
create table integration_state (
  state         text primary key,
  user_id       uuid not null references app_user(id) on delete cascade,
  provider      text not null,
  code_verifier text not null,
  return_to     text not null default '/settings',
  expires_at    timestamptz not null
);

-- ユーザーごとのトークン（APP_SECRET の鍵で暗号化）
create table integration_token (
  user_id    uuid not null references app_user(id) on delete cascade,
  provider   text not null,
  token_enc  text not null,
  created_at timestamptz not null default now(),
  primary key (user_id, provider)
);
