#!/usr/bin/env bash

run_climate_download="$1"

# run download scripts
load/_run-all.sh "$run_climate_download"

# export data and analyses to website
Rscript export/data/_run-all.r
Rscript export/analysis/_run.r value-renewables
Rscript export/analysis/_run.r gas-savings

### build website
cd ../web
npm run build



