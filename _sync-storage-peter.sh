#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"

echo $SCRIPT_DIR

SFTP_HOST="server.abteil.org"
SFTP_PORT="33123"
SFTP_USER="energy"
SFTP_KEYFILE="$HOME/.ssh/id_ed25519_energy_sync"
SFTP_KNOWN_HOSTS="$HOME/.ssh/known_hosts"
SFTP_REMOTE_DIR="data"
LOCAL_DIR="$SCRIPT_DIR/data/storage"

echo "Downloading newer files from ${SFTP_USER}@${SFTP_HOST}:${SFTP_REMOTE_DIR} to ${LOCAL_DIR}"
if ! lftp -u "${SFTP_USER}," "sftp://${SFTP_HOST}" <<EOF
set cmd:fail-exit yes
set net:max-retries 2
set net:timeout 30
set net:reconnect-interval-base 5
set sftp:connect-program "ssh -a -x -p $SFTP_PORT -i $SFTP_KEYFILE -o IdentitiesOnly=yes -o BatchMode=yes -o ConnectTimeout=30 -o UserKnownHostsFile=$SFTP_KNOWN_HOSTS"
mirror --only-newer --verbose "$SFTP_REMOTE_DIR" "$LOCAL_DIR"
bye
EOF
then
    echo "SFTP download failed" >&2
    exit 1
fi