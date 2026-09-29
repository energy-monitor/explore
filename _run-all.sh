#!/usr/bin/env bash

# run download scripts
if [ -n "$1" ] && [ "$1" == "0" ]; then
    echo "Parameter is 0, skipping downloads"
else
    load/_run-all.sh "$1"
fi

# sync storage from sftp server
bash _sync-storage-peter.sh || exit 1

# export data and analyses to website
Rscript export/data/_run-all.r
Rscript export/analysis/_run.r value-renewables
Rscript export/analysis/_run.r gas-savings

### build website
cd ../web
npm run build



