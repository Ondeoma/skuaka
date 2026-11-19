-- 外部のカレンダーの取り込み.
-- source_url のあるカレンダーは iCalendar の URL を購読したもので、定期的に読み直す（予定は読み取りだけ）.
alter table calendar
  add column source_url       text,          -- 購読する iCalendar の URL（webcal:// は https:// に直したもの）
  add column source_synced_at timestamptz,   -- 最後に読みに行った時刻（失敗しても進める）
  add column source_error     text;          -- 最後に読んだときの失敗の理由（成功したら null）
create index calendar_source_idx on calendar (source_synced_at) where source_url is not null;

-- 取り込んだ予定の元の UID（取り込み直したら上書きし、購読では元から消えた予定を消す）
alter table event add column source_uid text;
create unique index event_source_uid_idx on event (calendar_id, source_uid) where source_uid is not null;
