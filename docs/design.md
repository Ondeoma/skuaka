# skuaka の設計

## 認証（OIDC）

- Authorization Code + PKCE（S256）+ nonce + state。ログイン途中の状態（state・code_verifier・nonce・戻り先）は DB の `login_state` に置き、
  15分・1回だけ使える（複数ノードのどこに戻ってきてもよい）。
- ID トークンは、トークンエンドポイントから受け取ったうえで **署名も検証する**（JWKS の RS256/384/512・ES256/384/512、HS256/384/512 はクライアントシークレット）。
  `alg=none` は拒否。鍵が見つからなければ JWKS を取り直す（IdP の鍵の入れ替え）。そのうえで `iss`・`aud`（複数なら `azp`）・`exp`・`nonce` を確かめる。
- セッションはランダムな値の Cookie（HttpOnly・SameSite=Lax・https なら Secure）。DB にはその SHA-256 だけを保存する。ID トークンは
  ログアウトのときの `id_token_hint` のために保存する。
- CSRF: GET 以外の API は `X-Skuaka` ヘッダーが必要（ほかのサイトのフォームからは付けられない）。
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
- 繰り返しは iCalendar の RRULE の一部（FREQ・INTERVAL・BYDAY・UNTIL・COUNT）を文字列で保存し、**表示のたびにサーバーで展開する**
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

## 画面

- Scalactive（Vue 風の `.st` 単一ファイルコンポーネント）。画面の日時はブラウザのタイムゾーンで、API とは ISO 8601 の UTC の文字列でやり取りする。
- 表示中の期間の予定（展開済み）をまとめて読み、表示するカレンダーの絞り込みは画面で行う（すぐ切り替わる）。
- 日本の祝日は画面で計算する（祝日法。春分・秋分は近似式）。
