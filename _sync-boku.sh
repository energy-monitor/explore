#!/usr/bin/env bash
set -euo pipefail

REMOTE_DIR=www/mirror/

echo "COPYING TO: "
echo $REMOTE_DIR

# Uses the "dashboards-boku" host alias in ~/.ssh/config (key-based auth).
lftp sftp://dashboards-boku <<EOF
set sftp:auto-confirm yes
mirror --reverse --delete --verbose ../web/dist "$REMOTE_DIR"
bye
EOF

