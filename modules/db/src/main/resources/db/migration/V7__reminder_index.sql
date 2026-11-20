-- リマインダーの確認（30秒ごと）は、カレンダーを絞らずに「通知の付いた時刻のある予定」を期間で探す.
-- event_calendar_range_idx（calendar_id, range_start）は使えず event 全体を読んでいたので、その予定だけの索引を足す
create index event_reminder_idx on event (range_start) where not all_day and cardinality(reminders) > 0;
