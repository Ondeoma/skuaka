#!/usr/bin/env bash
# 開発用の OIDC プロバイダ（:9095）を起動する。本番では使わないこと（ログは $LOG、既定 /tmp/skuaka-idp.log）
cd "$(dirname "$0")/.."
. scripts/lib.sh
LOG=${LOG:-/tmp/skuaka-idp.log}
free_port 9095
run_sbt devidp/run "$LOG" '\[devidp\] 開発用' 120
