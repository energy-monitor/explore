#!/usr/bin/env bash

BASE_FOLDER=`dirname -- "$0"`/..;
cd $BASE_FOLDER

# - TEMP/HEATING DAYS

if [ "$1" == "no-climate" ]; then
    echo "skipping climate data download"
else
    echo "downloading climate data"
    python3 load/era5/downloadExtractFull.py
fi
Rscript calc/hdd.r

echo "gas data"

# - GAS
Rscript load/econtrol-gas-consumption.r
Rscript load/aggm/gas-consumption.r
Rscript load/gie/detailed.r
python3 load/cismo/1-gas-price.py
Rscript load/cismo/2-gas-price.r

echo "entso-e"

# - ELECTRICITY
Rscript load/entsoe/load.r
Rscript load/entsoe/load-hourly.r
Rscript load/entsoe/generation.r
Rscript load/entsoe/generation-hourly.r
Rscript load/entsoe/price.r
# Rscript load/entsoe/netPosition.r
Rscript load/entsoe/physicalFlows.r
Rscript load/entsoe/hydro-storage.r
Rscript load/apg/installed-power-capacity-at.R

echo "other"

# - OTHERS
Rscript load/ec-gas-oil.r
Rscript load/stat-economic-activity.r
Rscript load/stat-car-registrations.r
Rscript load/eurostat/passenger-cars.r
Rscript load/eurostat/prc_hicp_minr.r
Rscript load/eurostat/nrg_cb_gasm.r
Rscript load/eurostat/nrg_cb_oilm.r
Rscript load/eurostat/nrg_cb_sffm.r
Rscript load/eurostat/emissions.r
Rscript load/uba-thg-crt.r
