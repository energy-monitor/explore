#!/usr/bin/env bash

BASE_FOLDER=`dirname -- "$0"`/..;
cd $BASE_FOLDER

# - TEMP/HEATING DAYS
python3 load/era5/downloadExtractFull.py
Rscript calc/hdd.r
# Rscript calc/prediction-gas-consumption/linearModel1.r

# - GAS
Rscript load/econtrol-gas-consumption.r
Rscript load/aggm/gas-consumption.r
Rscript load/gie/detailed.r

# - ELECTRICITY
Rscript load/entsoe/load.r
Rscript load/entsoe/load-hourly.r
Rscript load/entsoe/generation.r
Rscript load/entsoe/generation-hourly.r
Rscript load/entsoe/price.r
# Rscript load/entsoe/netPosition.r
Rscript load/entsoe/physicalFlows.r


Rscript load/entsoe/load-hourly-res.r
Rscript load/entsoe/generation-hourly-res.r
Rscript load/entsoe/price-hourly-res.r


# - OTHERS
Rscript load/ec-gas-oil.r
Rscript load/stat-economic-activity.r
