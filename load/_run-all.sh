#!/usr/bin/env bash

# Runs all loaders, e.g. nightly on the server via cron.sh
#
# Arguments (any order):
# - `no-climate`: skip the downloads of the climate grids (ERA5, GeoSphere)
# - `all`: run the weekly scripts as well
#
# The output of a script goes to `data/tmp/log/<script>.log` (the last run
# only), the log of this script has one line per script and, for a failed
# script, the end of its output. A failed script does not stop the others,
# they are listed at the end (and the exit status is set).
#
# Sources that change monthly or yearly are only loaded on Mondays (`weekly`),
# the others every day (`daily`).

BASE_FOLDER=`dirname -- "$0"`/..;
cd $BASE_FOLDER

climate=1
all=0
for arg in "$@"; do
    case $arg in
        no-climate) climate=0 ;;
        all) all=1 ;;
        *) echo "unknown argument: $arg"; exit 1 ;;
    esac
done

LOG_FOLDER=data/tmp/log
mkdir -p $LOG_FOLDER

failed=()
skipped=()

# run <daily|weekly> <command...>
run() {
    local schedule=$1; shift
    if [ "$schedule" == "weekly" ] && [ "$(date +%u)" != "1" ] && [ $all == 0 ]; then
        skipped+=("$2")
        return
    fi
    local log=$LOG_FOLDER/$(echo "$2" | tr '/' '_').log
    local start=$(date +%s)
    printf '%s  %-48s ' "$(date '+%F %T')" "$2"
    if "$@" > "$log" 2>&1; then
        printf 'ok     %4d s\n' $(( $(date +%s) - start ))
    else
        printf 'FAILED %4d s\n' $(( $(date +%s) - start ))
        failed+=("$2")
        tail -n 20 "$log" | sed 's/^/    | /'
    fi
}

echo "$(date '+%F %T')  start$([ $climate == 0 ] && echo ', no climate')$([ $all == 1 ] && echo ', all')"

# - TEMP/HEATING DAYS
[ $climate == 1 ] && run daily python3 load/era5/downloadExtractFull.py
run daily Rscript calc/hdd.r

# - PRECIPITATION/SNOW/CLIMATE BY DISTRICT
[ $climate == 1 ] && run daily python3 load/geosphere/districts.py
run daily Rscript calc/climate-regions.r

# - GAS
run daily Rscript load/econtrol-gas-consumption.r
run daily Rscript load/aggm/gas-consumption.r
run daily Rscript load/gie/detailed.r
run daily python3 load/cismo/1-gas-price.py
run daily Rscript load/cismo/2-gas-price.r

# - ELECTRICITY
run daily Rscript load/entsoe/load.r
run daily Rscript load/entsoe/load-hourly.r
run daily Rscript load/entsoe/generation.r
run daily Rscript load/entsoe/generation-hourly.r
run daily Rscript load/entsoe/price.r
# run daily Rscript load/entsoe/netPosition.r
run daily Rscript load/entsoe/physicalFlows.r
run daily Rscript load/entsoe/hydro-storage.r
run weekly Rscript load/apg/installed-power-capacity-at.R

# - WOOD
run weekly Rscript load/agrarforschung/pellets.r
run weekly Rscript load/agrarforschung/brennholz.r

# - OTHERS
run weekly Rscript load/ec-gas-oil.r
run weekly Rscript load/stat-economic-activity.r
run weekly Rscript load/stat-car-registrations.r
run weekly Rscript load/eurostat/passenger-cars.r
run weekly Rscript load/asfinag/traffic.r
run weekly Rscript load/eurostat/rail.r
run weekly Rscript load/eurostat/prc_hicp_minr.r
run weekly Rscript load/eurostat/nrg_cb_gasm.r
run weekly Rscript load/eurostat/nrg_cb_oilm.r
run weekly Rscript load/eurostat/nrg_cb_sffm.r
run weekly Rscript load/eurostat/emissions.r
run weekly Rscript load/uba-thg-crt.r
run weekly Rscript load/unfccc-crt.r
run weekly Rscript calc/emissions.r

echo "$(date '+%F %T')  done, ${#failed[@]} failed, ${#skipped[@]} weekly scripts skipped"
if [ ${#failed[@]} -gt 0 ]; then
    printf -- '- %s\n' "${failed[@]}"
    exit 1
fi
