#!/usr/bin/env bash

# run download scripts
if [ -n "$1" ] && [ "$1" == "0" ]; then
    echo "Parameter is 0, skipping downloads"
else
    load/_run-all.sh    
fi

# export data and analyses to website
Rscript export/data/_run-all.r
Rscript export/analysis/_run.r value-renewables
Rscript export/analysis/_run.r gas-savings

### build website
cd ../web
npm run build



