#!/usr/bin/env bash

BASE_FOLDER=`dirname -- "$0"`/..;
cd $BASE_FOLDER

echo "downloading climate data"

# - TEMP/HEATING DAYS
python3 load/era5/downloadExtractFull.py
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
Rscript load/apg/download-capacity-at.R

echo "other"

# - OTHERS
Rscript load/ec-gas-oil.r
Rscript load/stat-economic-activity.r
