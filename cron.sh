#!/usr/bin/env bash

# The nightly load of all data sources on the server, e.g. in the crontab
#   0 1 * * * /path/to/explore/cron.sh >> /path/to/cron.log 2>&1
# Runs load/_run-all.sh in the pixi environment of this repository, arguments
# are passed on (`no-climate` skips the grid downloads, `all` runs the weekly
# scripts as well).

# cron has a minimal PATH, pixi is installed per user in ~/.pixi/bin (the one
# in /usr/local/bin is too old for the lock file)
PATH=$HOME/.pixi/bin:$PATH

cd "$(dirname -- "$0")" || exit 1

pixi run bash load/_run-all.sh "$@"
