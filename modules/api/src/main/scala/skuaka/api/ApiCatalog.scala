package skuaka.api

/** REST API の一覧（MCP の list_api_endpoints と GET /api/endpoints）. */
object ApiCatalog {
  final case class Endpoint(
    method: String,
    path: String,
    description: String)
  val all: List[Endpoint] = List(
    Endpoint("GET", "/api/me", "自分の情報（名前・メール・タイムゾーン）"),
    Endpoint("PATCH", "/api/me", "自分の名前・タイムゾーンを変える {name?, timezone?}"),
    Endpoint("PUT", "/api/me/avatar", "自分のアイコンを保存する（本文は画像そのもの. PNG・JPEG・GIF・WebP、1MB まで）"),
    Endpoint("DELETE", "/api/me/avatar", "保存したアイコンを消す（IdP の画像に戻る）"),
    Endpoint("GET", "/api/users?q=", "ユーザーを名前・メールで探す"),
    Endpoint("GET", "/api/calendars", "見られるカレンダーの一覧（自分の権限 busy|view|edit|owner. busy は予定あり・なしのみ）"),
    Endpoint("POST", "/api/calendars", "カレンダーを作る {name, color: #rrggbb, description, teamAccess?: none|busy|view}"),
    Endpoint(
      "PUT",
      "/api/calendars/{calendarId}",
      "カレンダーを変える（所有者） {name, color, description, teamAccess?: none|busy|view（チームへの公開）}"
    ),
    Endpoint("PATCH", "/api/calendars/{calendarId}/prefs", "自分の表示の設定 {color?, visible?}"),
    Endpoint("DELETE", "/api/calendars/{calendarId}", "カレンダーを削除する（所有者）。共有されたカレンダー・チームのカレンダーなら一覧から外す"),
    Endpoint("GET", "/api/team/calendars?q=", "チームの人が公開しているカレンダーを探す（q は持ち主の名前・メール、カレンダーの名前）"),
    Endpoint("POST", "/api/team/calendars/{calendarId}", "チームに公開されたカレンダーを自分の一覧に加える"),
    Endpoint("GET", "/api/calendars/{calendarId}/members", "共有の相手と権限"),
    Endpoint("POST", "/api/calendars/{calendarId}/members", "共有する {user: メール または ID, role: view|edit}"),
    Endpoint("DELETE", "/api/calendars/{calendarId}/members/{userId}", "共有をやめる"),
    Endpoint(
      "POST",
      "/api/calendars/{calendarId}/public",
      "ログインしない人への公開リンク（閲覧ページと iCalendar の配信）を作る・範囲を変える（所有者） {access: busy|view, regenerate?: true で作り直す}"
    ),
    Endpoint("DELETE", "/api/calendars/{calendarId}/public", "公開リンクをやめる"),
    Endpoint(
      "POST",
      "/api/calendars/subscribe",
      "iCalendar の URL（https:// か webcal://）を購読する {url, name?, color?}。読み取りだけのカレンダーができ、定期的に読み直す。やめるときはカレンダーを削除する"
    ),
    Endpoint("POST", "/api/calendars/{calendarId}/sync", "購読したカレンダーを今すぐ読み直す（所有者）"),
    Endpoint(
      "POST",
      "/api/calendars/{calendarId}/import",
      "iCalendar のファイルの予定を取り込む（本文は iCalendar の文字列そのもの. Content-Type: text/calendar. 10MB まで. REST API だけ）。同じ UID の予定は上書きする"
    ),
    Endpoint("GET", "/api/events?from=yyyy-MM-dd&to=yyyy-MM-dd&tz=", "期間の予定（繰り返しは1回ずつに展開。to は含まない。400日まで）"),
    Endpoint("GET", "/api/events/search?q=", "予定を探す（タイトル・場所・説明）"),
    Endpoint("GET", "/api/events/{eventId}", "予定の詳細（繰り返しの定義・ゲスト・version）"),
    Endpoint(
      "POST",
      "/api/events",
      "予定を作る {calendarId, title, description, location, allDay, start, end, timezone, color?, recurrence?, attendees: [userId], reminders: [何分前], withMeeting?: true で Web 会議（skaka）の会議室を付ける}"
    ),
    Endpoint(
      "PUT",
      "/api/events/{eventId}?scope=this|following|all&occurrence=",
      "予定を変える（本文は作成と同じ + version。繰り返しは scope と occurrence を付ける）"
    ),
    Endpoint("DELETE", "/api/events/{eventId}?scope=this|following|all&occurrence=", "予定を削除する"),
    Endpoint("POST", "/api/events/{eventId}/rsvp", "招待に返事をする {response: accepted|declined|tentative}"),
    Endpoint("DELETE", "/api/events/{eventId}/rsvp", "招待された予定を自分のカレンダーから外す")
  )
}
