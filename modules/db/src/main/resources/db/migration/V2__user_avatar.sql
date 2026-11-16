-- ユーザーが保存したアイコン（画面で 256×256 にした画像）. あれば IdP の avatar_url より優先する.
-- avatar_version は保存するたびに増やし、画像の URL に付ける（ブラウザのキャッシュを効かせたまま、変えたらすぐ切り替わるように）.
alter table app_user add column avatar_version int;

create table user_avatar (
  user_id      uuid primary key references app_user(id) on delete cascade,
  content_type text not null check (content_type in ('image/png', 'image/jpeg', 'image/gif', 'image/webp')),
  data         bytea not null,
  updated_at   timestamptz not null default now()
);
