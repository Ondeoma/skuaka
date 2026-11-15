# scripts/dev-*.sh の共通部分（source して使う）

# ポートで待ち受けているプロセスを止め、ポートが空くまで待つ
free_port() {
  local pid
  pid=$(lsof -tiTCP:"$1" -sTCP:LISTEN || true)
  if [ -n "$pid" ]; then kill $pid; while lsof -tiTCP:"$1" -sTCP:LISTEN >/dev/null; do sleep 0.5; done; fi
}

# sbt のタスクを裏で起動し、ログに準備完了の文言が出るまで待つ: run_sbt <タスク> <ログ> <準備完了の正規表現> <待つ秒数>
run_sbt() {
  nohup sbt -batch -no-colors "$1" > "$2" 2>&1 &
  for _ in $(seq 1 "$4"); do
    grep -qE "$3" "$2" && { echo "started"; return 0; }
    grep -qE 'nonzero exit|\[error\]' "$2" && { tail -30 "$2"; return 1; }
    sleep 1
  done
  echo "timeout"; return 1
}
