#!/usr/bin/env bash
set -euo pipefail

CREDS=creds-boku.json

HOST=$(jq -r '.["dashboards-boku"].host' "$CREDS")
USER=$(jq -r '.["dashboards-boku"].user' "$CREDS")
PASS=$(jq -r '.["dashboards-boku"].pass' "$CREDS")
PORT=$(jq -r '.["dashboards-boku"].port // 22' "$CREDS")
REMOTE_DIR=$(jq -r '.["dashboards-boku"].remote_dir // "."' "$CREDS")

echo "COPYING TO: "
echo $REMOTE_DIR

lftp -u "$USER,$PASS" -p "$PORT" sftp://"$HOST" <<EOF
set sftp:auto-confirm yes
mirror --reverse --delete --verbose ../web/dist "$REMOTE_DIR"
bye
EOF

