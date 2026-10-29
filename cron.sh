#!/usr/bin/env bash

# The nightly load of all data sources on the server, e.g. in the crontab
#   0 1 * * * /path/to/explore/cron.sh >> /path/to/cron.log 2>&1
# Runs load/_run-all.sh in the pixi environment of this repository, arguments
# are passed on (e.g. `cron.sh no-climate` skips the grid downloads).

# cron has a minimal PATH, pixi is in /usr/local/bin
PATH=$PATH:/usr/local/bin

cd "$(dirname -- "$0")" || exit 1

pixi run bash load/_run-all.sh "$@"
