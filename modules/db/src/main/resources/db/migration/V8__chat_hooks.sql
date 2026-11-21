-- stui（チャット）との連携と、カレンダーの通知先.

-- stui と連携したとき、リマインダー・招待を stui の DM でも受け取るか（個人の設定）
alter table app_user
  add column chat_reminders boolean not null default true,
  add column chat_invites   boolean not null default true;

-- カレンダーの通知先. 予定のリマインダー・追加・変更・削除を送る.
--  kind = stui:      owner_id の人が stui と連携したトークンで、channel_id のチャンネルに stui のボット「skuaka」として投稿する
--  kind = stui_hook: url（stui の受信 Webhook）に送る
--  kind = url:       url に、署名（secret の HMAC-SHA256）付きの JSON を送る
create table calendar_hook (
  id           uuid primary key,
  calendar_id  uuid not null references calendar(id) on delete cascade,
  kind         text not null check (kind in ('stui', 'stui_hook', 'url')),
  name         text not null,
  url          text not null default '',
  secret       text not null default '',
  events       text[] not null default '{}',
  enabled      boolean not null default true,
  owner_id     uuid references app_user(id) on delete set null,
  channel_id   text,
  channel_name text,
  created_by   uuid references app_user(id) on delete set null,
  created_at   timestamptz not null default now()
);
create index calendar_hook_calendar_idx on calendar_hook (calendar_id);

-- 送信待ち（失敗したら間を空けて送り直す. 8回で諦める. 30日で消す）
create table calendar_hook_delivery (
  id          uuid primary key,
  hook_id     uuid not null references calendar_hook(id) on delete cascade,
  event       text not null,
  payload     jsonb not null,
  attempts    int not null default 0,
  not_before  timestamptz not null default now(),
  done_at     timestamptz,
  failed      boolean not null default false,
  last_error  text,
  status_code int,
  created_at  timestamptz not null default now()
);
create index calendar_hook_delivery_pending_idx on calendar_hook_delivery (not_before) where done_at is null and not failed;
create index calendar_hook_delivery_hook_idx on calendar_hook_delivery (hook_id, created_at desc);
