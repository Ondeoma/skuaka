-- skuaka のスキーマ（適用済みのファイルは書き換えず、変更は新しい番号の V<n>__*.sql を追加する）

create table app_user (
  id            uuid primary key,
  sub           text not null unique,            -- OIDC の sub（開発用ログインは dev:名前）
  name          text not null,
  email         text,
  avatar_url    text,
  timezone      text not null default 'Asia/Tokyo',
  disabled      boolean not null default false,
  created_at    timestamptz not null default now(),
  last_login_at timestamptz
);
create index app_user_email_idx on app_user (lower(email));

create table session (
  token_hash bytea primary key,                  -- Cookie の値の SHA-256（値そのものは保存しない）
  user_id    uuid not null references app_user(id) on delete cascade,
  id_token   text,                               -- IdP からログアウトするときの id_token_hint
  created_at timestamptz not null default now(),
  expires_at timestamptz not null
);
create index session_expires_idx on session (expires_at);

-- OIDC のログイン途中の状態（複数ノードのどこに戻ってきてもよいよう DB に置く）
create table login_state (
  state      text primary key,
  verifier   text not null,
  nonce      text not null,
  return_to  text not null default '/',
  created_at timestamptz not null default now()
);

create table calendar (
  id          uuid primary key,
  owner_id    uuid not null references app_user(id) on delete cascade,
  name        text not null,
  description text not null default '',
  color       text not null,
  is_primary  boolean not null default false,    -- ログインしたときに作る、その人のカレンダー
  -- チームへの公開: none（しない）/ busy（予定あり・なしのみ）/ view（予定の詳細）. メインのカレンダーは busy で作る
  team_access   text not null default 'none' check (team_access in ('none', 'busy', 'view')),
  -- ログインしない人への公開リンクの秘密の値（閲覧のページ /p/{token} と iCalendar の配信 /ics/{token}.ics. null なら公開しない）
  public_token  text unique,
  public_access text not null default 'view' check (public_access in ('busy', 'view')),
  created_at  timestamptz not null default now()
);
create unique index calendar_primary_idx on calendar (owner_id) where is_primary;
create index calendar_team_idx on calendar (owner_id) where team_access <> 'none';

-- カレンダーを見られる人（所有者も owner として入る）と、その人の表示の設定.
-- team はチームに公開されたカレンダーを自分で一覧に加えたもの（権限は calendar.team_access に従う）
create table calendar_member (
  calendar_id uuid not null references calendar(id) on delete cascade,
  user_id     uuid not null references app_user(id) on delete cascade,
  role        text not null check (role in ('team', 'view', 'edit', 'owner')),
  color       text,                              -- 自分だけの表示の色
  visible     boolean not null default true,
  primary key (calendar_id, user_id)
);
create index calendar_member_user_idx on calendar_member (user_id);

create table event (
  id               uuid primary key,
  calendar_id      uuid not null references calendar(id) on delete cascade,
  organizer_id     uuid references app_user(id) on delete set null,
  title            text not null,
  description      text not null default '',
  location         text not null default '',
  all_day          boolean not null,
  start_at         timestamptz,                  -- 時刻のある予定
  end_at           timestamptz,
  start_date       date,                         -- 終日の予定（end_date はその日を含む）
  end_date         date,
  timezone         text not null,                -- 繰り返しを展開する基準
  color            text,
  rrule            text,                         -- FREQ=WEEKLY;BYDAY=MO
  exdates          text[] not null default '{}', -- 繰り返しから除いた回（その回の元の開始）
  reminders        int[] not null default '{}',  -- 通知（何分前の一覧. 空なら通知しない）
  range_start      timestamptz not null,         -- 検索用: 最初の回の始まり
  range_end        timestamptz,                  -- 検索用: 最後の回の終わり（終わりのない繰り返しは null）
  version          int not null default 1,       -- 楽観ロック
  created_at       timestamptz not null default now(),
  updated_at       timestamptz not null default now(),
  check ((all_day and start_date is not null and end_date is not null) or (not all_day and start_at is not null and end_at is not null))
);
create index event_calendar_range_idx on event (calendar_id, range_start);

-- ゲスト（招待した人）
create table event_attendee (
  event_id uuid not null references event(id) on delete cascade,
  user_id  uuid not null references app_user(id) on delete cascade,
  response text not null default 'needs_action' check (response in ('needs_action', 'accepted', 'declined', 'tentative')),
  primary key (event_id, user_id)
);
create index event_attendee_user_idx on event_attendee (user_id);

-- API トークン（REST API と MCP を Bearer で呼ぶため）. 値は SHA-256 だけを保存する.
create table api_token (
  id           uuid primary key,
  user_id      uuid not null references app_user(id) on delete cascade,
  name         text not null,
  prefix       text not null,                 -- 画面に出す先頭部分（skuaka_xxxxxx）
  token_hash   bytea not null unique,
  scope        text not null check (scope in ('read', 'write')),
  created_at   timestamptz not null default now(),
  expires_at   timestamptz,
  last_used_at timestamptz,
  revoked_at   timestamptz
);
create index api_token_user_idx on api_token (user_id);
