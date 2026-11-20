# skuaka の設計

## 認証（OIDC）

- Authorization Code + PKCE（S256）+ nonce + state。ログイン途中の状態（state・code_verifier・nonce・戻り先）は DB の `login_state` に置き、
  15分・1回だけ使える（複数ノードのどこに戻ってきてもよい）。
- ID トークンは、トークンエンドポイントから受け取ったうえで **署名も検証する**（JWKS の RS256/384/512・ES256/384/512、HS256/384/512 はクライアントシークレット）。
  `alg=none` は拒否。鍵が見つからなければ JWKS を取り直す（IdP の鍵の入れ替え）。そのうえで `iss`・`aud`（複数なら `azp`）・`exp`・`nonce` を確かめる。
- セッションはランダムな値の Cookie（HttpOnly・SameSite=Lax・https なら Secure）。DB にはその SHA-256 だけを保存する。ID トークンは
  ログアウトのときの `id_token_hint` のために保存する。
- CSRF: GET 以外の API は `X-Skuaka` ヘッダーが必要（ほかのサイトのフォームからは付けられない）。
- ログインや連携の後の戻り先は `SafePath`（サーバーと画面で共通）で同じサイトのパスだけに絞る。`//`・`\`・タブや改行を含むものは `/` にする。
- 応答には `X-Content-Type-Options`・`X-Frame-Options: DENY`・`Referrer-Policy` と CSP を付ける（`SecurityHeaders`）。API とエラーの画面は
  何も読み込ませない CSP、画面の配信はスクリプトを自分のサイトと `index.html` の直書き（SHA-256 のハッシュ）だけに絞る CSP。https で公開
  しているときは HSTS も付ける。リクエストの本文は 2MB まで（`ApiRoutes.MaxBody`）。
- ログインしなくても使える入口（ログインの `/auth/…`、OAuth の `/oauth/authorize`・`/oauth/token`・`/oauth/revoke`、公開リンクの `/api/public/…`・`/ics/…`）は、IP ごとに1分あたりの回数を制限する
  （`RateLimit`。超えたら 429 と `Retry-After`）。利用者の IP は、`TRUSTED_PROXIES` のプロキシから来た要求だけ `X-Forwarded-For` を右から
  たどって決める（左側はクライアントが書けるので使わない）。IPv6 は /64 ごとにまとめる。数はノードごとのメモリで数える。
- 開発用のログイン（`DEV_LOGIN`）は名前だけでだれにでもなれるので、`PUBLIC_URL` が localhost のときしか起動しない。
- ログインの失敗や外部サービスのエラーの詳細（検証に失敗した項目・中継先の URL など）はログにだけ出し、画面には出さない。
- ログアウトは、IdP の `end_session_endpoint` があればそこへ（`id_token_hint`・`post_logout_redirect_uri` 付き）。なければ skuaka のログイン画面へ。
- ユーザーは `sub` で識別し、ログインのたびに名前・メール・アイコンを IdP の値に合わせる。初めてのログインで自分のカレンダーを作る。
- 自分で保存したアイコンは `user_avatar` に画像のまま置き、IdP のアイコン（`avatar_url`）より優先する。画面で 256×256 にしてから送り、
  サーバーは先頭のバイトで PNG・JPEG・GIF・WebP だけを受け付ける（SVG はスクリプトを含められるので断る. 1MB まで）。
  表示する URL は `UserCols.avatar`（SQL）で決め、保存した画像は `/api/users/{id}/avatar?v={avatar_version}`。版を URL に付けるので、
  長くキャッシュしても変えればすぐ切り替わる。画像はログインした人だけが読める（`nosniff`・`default-src 'none'` 付き）。

## API トークンと MCP

- API トークンは `skuaka_` + 30バイトの乱数。DB（`api_token`）には SHA-256 と先頭の数文字（画面の表示用）だけを保存する。
  スコープは `read`（GET だけ）と `write`。有効期限は任意。最後に使った日時は1分に1回だけ書く。
- `Authorization: Bearer` の要求は CSRF の確認（`X-Skuaka`）をしない（ブラウザが自動では付けないため）。トークンが正しくなければ 401（`WWW-Authenticate: Bearer`）。
- トークンの作成・一覧・失効は Cookie のセッションからだけ（トークンでトークンを増やせないように）。
- MCP は `POST /mcp` の JSON-RPC（Streamable HTTP。応答は JSON で SSE は使わない。通知には 202）。認証は API トークンだけ。
  各ツールは同じトークンで REST API を内部から呼ぶので、権限・入力の確認は REST API と同じになる（MCP 専用の権限の確認を持たない）。
  参照のみのトークンには、変更するツールを `tools/list` に出さず、呼ばれても断る。
- MCP のツールは日時をトークンの持ち主のタイムゾーンで受け渡し、REST API の UTC の文字列に直す。`update_event` は今の予定に指定した項目だけを重ね、
  `version` を付けて送る（ほかの人の変更とぶつかれば 409 がそのまま返る）。

## 予定と繰り返し

- 時刻のある予定は `start_at`・`end_at`（timestamptz、終わりを含まない）、終日の予定は `start_date`・`end_date`（date、終わりの日を含む）。
- 繰り返しは iCalendar の RRULE の一部（FREQ・INTERVAL・BYDAY・BYMONTHDAY・BYMONTH・BYSETPOS・UNTIL・COUNT）を文字列で保存し、**表示のたびにサーバーで展開する**
  （RFC 5545 のとおり、期間（日・週・月・年）ごとに候補の日を作り、BYMONTH・BYMONTHDAY・BYDAY で広げるか絞り、最後に BYSETPOS で選ぶ. `Expand.periodDates`）
  （予定の `timezone` を基準にするので、夏時間をまたいでも現地の時刻を保つ）。除いた回は `exdates`（その回の元の開始）。
- 期間で探すため、最初の回の始まり `range_start` と最後の回の終わり `range_end`（終わりのない繰り返しは null）を保存時に計算する。
  展開は表示する期間の少し前の周期から始める（何年も続く毎日の予定でも、昔の回を数えない。回数の指定があるときだけは最初から数える）。
- 見える予定は「メンバーのカレンダーの予定」と「ゲストとして招待された予定」を union で先に絞ってから読む（それぞれ索引が使える）。
- 繰り返しの変更・削除:
  - この予定: 元の予定の `exdates` にその回を入れ、（変更なら）新しい1回だけの予定を作る
  - これ以降: 元の予定の繰り返しをその回の前日で終わらせ（UNTIL）、（変更なら）その回から始まる新しい予定を作る
  - すべて: 元の予定を変える。ある回を動かしたときは、繰り返しの最初の回を同じ日数だけずらす
- 楽観ロック: 「すべて」の変更は `version` が違えば 409。

## 権限

- カレンダーの `calendar_member`: owner（所有者。共有・配信・削除）/ edit（予定の作成・変更・削除）/ view（閲覧）/
  team（チームに公開されたカレンダーを自分で加えた印。権限は `calendar.team_access` に従う）。
- チームへの公開 `calendar.team_access`: none / busy（予定あり・なしのみ）/ view（詳細）。team の行の実際の権限は
  `Member.role`（SQL の case 式）で求め、公開をやめた（none）カレンダーの team の行は何も見せない（行は残すので、また公開すれば戻る）。
- busy の予定は `VisibleEvent.of` で題名・説明・場所・色・通知・ゲストの人数を伏せ、詳細ではゲストと繰り返しの決まりも返さない。
  ゲストとして招待されていれば伏せない。題名などで探すときは busy のカレンダーを経路から外す（伏せた内容に当たらないように）。
- 予定が見えるのは、そのカレンダーのメンバーか、ゲスト（`event_attendee`）。ゲストは返事だけができる。
- 公開リンク（`calendar.public_token` と `public_access`）: 推測できない値を知っている人だけが、ログインせずに読める（所有者が作り直し・停止できる）。
  閲覧のページ `/p/{token}`（画面はログインしたときと同じ部品を閲覧だけで使う。`Store.guest`）と、`/api/public/{token}`・`/ics/{token}.ics`。
  予定は `VisibleEvent.of(row, Some(public_access), None, 0)` で、チームへの公開と同じく busy なら伏せる。ゲストの一覧と主催者は返さない。

## 外部のカレンダーの取り込み

- iCalendar（RFC 5545）を `IcsParse`（core）で読む。Google カレンダー・Outlook・Apple のカレンダーが書き出すものを想定し、
  折り返し・エスケープ・TZID（IANA の名前と、Outlook の Windows の名前の主なもの）・浮動の時刻（利用者のタイムゾーンで読む）・終日（DTEND は含まない）・
  DURATION・RRULE（UNTIL は予定のタイムゾーンの日付に直す）・EXDATE・RECURRENCE-ID・STATUS:CANCELLED を扱う。
  繰り返しの一部の回の変更（RECURRENCE-ID）は、元の予定の `exdates` に入れ、別の予定（UID は「元の UID#その回のキー」）にする。
  skuaka で表せない RRULE の指定（BYYEARDAY・BYWEEKNO・BYHOUR など）は省いて近似し、件数を返す。VALARM（通知）・ATTENDEE（ゲスト）は取り込まない。
- 取り込んだ予定は `event.source_uid`（カレンダーごとに一意）に元の UID を持ち、取り込み直すと上書きする（変わった行だけ. 重ならない）。
- ファイルの取り込み（`POST /api/calendars/{id}/import`）: 予定を入れられるカレンダーに一度だけ写す。写した予定はふつうの予定として変更できる。
  本文の上限はほかの API（2MB）と分け、`IcsImport.MaxBytes`（10MB）にする。
- URL の購読（`calendar.source_url`）: 読めたらカレンダーを作る。予定は読み取りだけ（`Member.eventRole` で所有者でも view にし、`CalendarRow.writable` で予定を入れさせない）。
  `CalendarImporter.loop` が5分ごとに `ICS_SYNC_MINUTES`（既定60分）より前に読んだ購読を `claimDue`（`for update skip locked`. 複数ノードでも二重に読まない）で取り、
  読み直して、元から消えた予定を消す。失敗の理由は `source_error` に残し、予定はそのまま。
- SSRF の対策: 購読の URL は http(s) だけで、ホスト名を解決して内部のアドレス（localhost・プライベート・リンクローカル（クラウドのメタデータ）・CGNAT・ユニークローカルなど）なら読みに行かない（`UrlGuard`）。
  転送（3xx）は3回まで追い、転送先も同じく確かめる。大きさは10MB・60秒まで。開発環境では `ICS_ALLOW_PRIVATE_URLS=true` で手元の URL も読める。

## 画面

- Scalactive（Vue 風の `.st` 単一ファイルコンポーネント）。画面の日時はブラウザのタイムゾーンで、API とは ISO 8601 の UTC の文字列でやり取りする。
- 表示中の期間の予定（展開済み）をまとめて読み、表示するカレンダーの絞り込みは画面で行う（すぐ切り替わる）。
- 日本の祝日は画面で計算する（祝日法。春分・秋分は近似式）。

## Web 会議（skaka）との連携

- 各ユーザーが設定の画面で連携する: `/api/integrations/skaka/connect` で state と PKCE の verifier を DB（`integration_state`. 10分・1回だけ）に置いて
  skaka の許可の画面へ。`/callback` で認可コードをトークン（`skaka_…`）に替え、`APP_SECRET` の鍵（AES-256-GCM）で暗号化して `integration_token` に保存する。
  連携をやめると skaka の `/oauth/revoke` も呼ぶ。
- 会議室は予定の API（`EventReq.withMeeting`）で作る（MCP の `create_event`・`update_event` も同じ）。予定を保存してから skaka の `POST /api/rooms` を呼び、
  場所に URL を足す（空なら URL だけ、あれば「{場所} / {URL}」. `Meeting`）。作れなくても予定は残し、理由を `EventDto.meetingError` で返す。
  場所に既に skaka の URL があれば作らない。
- 「すべて」の変更で題名・日時が変わったら、場所の URL のコードで `PUT /api/rooms/{code}/event` を呼ぶ（失敗しても保存は止めない）。
- skaka が 401 を返したらトークンの失効なので、保存したトークンを消して「連携が切れた」と扱う。
