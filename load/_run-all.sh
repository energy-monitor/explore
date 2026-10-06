#!/usr/bin/env bash

BASE_FOLDER=`dirname -- "$0"`/..;
cd $BASE_FOLDER

# An error in one script does not stop the others, the failed ones are listed
# at the end (and the exit status is set)
failed=()
run() {
    echo "- $*"
    "$@" || failed+=("$*")
}

# - TEMP/HEATING DAYS
if [ "$1" == "no-climate" ]; then
    echo "skipping climate data download"
else
    run python3 load/era5/downloadExtractFull.py
fi
run Rscript calc/hdd.r

# - GAS
run Rscript load/econtrol-gas-consumption.r
run Rscript load/aggm/gas-consumption.r
run Rscript load/gie/detailed.r
run python3 load/cismo/1-gas-price.py
run Rscript load/cismo/2-gas-price.r

# - ELECTRICITY
run Rscript load/entsoe/load.r
run Rscript load/entsoe/load-hourly.r
run Rscript load/entsoe/generation.r
run Rscript load/entsoe/generation-hourly.r
run Rscript load/entsoe/price.r
# run Rscript load/entsoe/netPosition.r
run Rscript load/entsoe/physicalFlows.r
run Rscript load/entsoe/hydro-storage.r
run Rscript load/apg/installed-power-capacity-at.R

# - OTHERS
run Rscript load/ec-gas-oil.r
run Rscript load/stat-economic-activity.r
run Rscript load/stat-car-registrations.r
run Rscript load/eurostat/passenger-cars.r
run Rscript load/asfinag/traffic.r
run Rscript load/eurostat/rail.r
run Rscript load/eurostat/prc_hicp_minr.r
run Rscript load/eurostat/nrg_cb_gasm.r
run Rscript load/eurostat/nrg_cb_oilm.r
run Rscript load/eurostat/nrg_cb_sffm.r
run Rscript load/eurostat/emissions.r
run Rscript load/uba-thg-crt.r
run Rscript calc/emissions.r


if [ ${#failed[@]} -gt 0 ]; then
    echo "failed scripts:"
    printf -- '- %s\n' "${failed[@]}"
    exit 1
fi
