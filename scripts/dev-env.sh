# 開発用の環境変数（source scripts/dev-env.sh）。docker-compose.dev.yml の DB を使う
export DATABASE_URL=jdbc:postgresql://localhost:54349/skuaka DATABASE_USER=skuaka DATABASE_PASSWORD=skuaka
# 画面は Vite の開発サーバー（:5175. /api・/auth・/ics・/mcp を :8095 へ中継する）から開くので、公開 URL もそちらにする
export PUBLIC_URL=${PUBLIC_URL:-http://localhost:5175} HTTP_PORT=8095
# 認証: OIDC=dev なら開発用の IdP（scripts/dev-idp.sh、:9095）で OIDC のログインを試す。それ以外は名前だけの開発用ログイン
if [ "${OIDC:-}" = "dev" ]; then
  export OIDC_ISSUER=http://localhost:9095 OIDC_CLIENT_ID=skuaka OIDC_CLIENT_SECRET=skuaka-dev-secret-0123456789
  unset DEV_LOGIN
else
  export DEV_LOGIN=true
  unset OIDC_ISSUER OIDC_CLIENT_ID OIDC_CLIENT_SECRET
fi
# 秘密の値の暗号化の鍵（開発用. 本番では十分に長い乱数にする）
export APP_SECRET=${APP_SECRET:-skuaka-dev-app-secret-0123456789}
# Web 会議（skaka）との連携: skaka の開発環境（画面 :5177. /oauth・/api を skaka のサーバーへ中継）に登録済みの値
export SKAKA_URL=${SKAKA_URL:-http://localhost:5177} SKAKA_CLIENT_ID=${SKAKA_CLIENT_ID:-skuaka} SKAKA_CLIENT_SECRET=${SKAKA_CLIENT_SECRET:-skuaka-dev-secret-0123456789}
# skuaka が認可を出す側（skakapo が利用者の権限で REST API・MCP を使う）。skakapo の開発環境（:5199）に登録する値
export OAUTH_CLIENTS='[{"id":"skakapo","secret":"skakapo-dev-secret-0123456789","redirectUri":"http://localhost:5199/api/connections/callback","name":"skakapo","scopes":["read","write"]}]'
