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
