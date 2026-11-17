-- プッシュ通知（PWA）. VAPID の鍵（全ノードで同じものを使う）と、ブラウザ・端末ごとの購読
create table push_config (
  id            int primary key check (id = 1),
  vapid_public  text not null,
  vapid_private text not null,
  created_at    timestamptz not null default now()
);

create table push_subscription (
  id           uuid primary key,
  user_id      uuid not null references app_user(id) on delete cascade,
  endpoint     text not null unique,
  p256dh       text not null,
  auth         text not null,
  user_agent   text,
  created_at   timestamptz not null default now(),
  last_used_at timestamptz
);
create index push_subscription_user_idx on push_subscription (user_id);

-- 予定のリマインダー（何分前のお知らせ）をプッシュ通知で送った記録. 複数ノードで同じ回を2度送らないよう、先に入れたノードだけが送る
create table reminder_sent (
  event_id   uuid not null references event(id) on delete cascade,
  occurrence text not null,                      -- 繰り返しの回（その回の元の開始）
  minutes    int not null,                       -- 何分前の通知か
  sent_at    timestamptz not null default now(),
  primary key (event_id, occurrence, minutes)
);
create index reminder_sent_at_idx on reminder_sent (sent_at);
