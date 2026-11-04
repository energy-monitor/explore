#!/bin/bash
# The bidding zones of Europe as TopoJSON for the web (web/assets/geo/
# europe-bidding-zones.json, the object `europe`), the ids are the AreaMapCode
# of the day-ahead prices of ENTSO-E (load/entsoe/price.r), e.g. AT, DE_LU,
# NO1, IT-NORTH. Run once from explore/, it is not part of the nightly export:
#
#     export/geo/bidding-zones.sh
#
# - the zones of entsoe-py (https://github.com/EnergieID/entsoe-py, MIT, the
#   folder entsoe/geo/geojson at the commit below): Natural Earth for the
#   countries, NVE for Norway, natomraden.se for Sweden, the regions of the
#   zones of GME for Italy (the zones since 2021)
# - the other countries of web/assets/geo/europe.json, e.g. Albania, North
#   Macedonia or the United Kingdom without a price, Ireland as the zone of the
#   island (IE_SEM, Northern Ireland stays part of the United Kingdom), Kosovo
#   (XK) is Serbia of europe.json without the one of entsoe-py
# - simplified to about the size of europe.json (40 kB)
#
# Needs node (mapshaper is run by npx) and curl.
set -euo pipefail

commit=29202229b7ce1218c37c96bc5450bd3672f4a6d0
zones="AT BE BG CH CZ DE_LU DK_1 DK_2 EE ES FI FR GR HR HU IT_CALA IT_CNOR IT_CSUD IT_NORD IT_SARD IT_SICI IT_SUD LT LV NL NO_1 NO_2 NO_3 NO_4 NO_5 PL PT RO RS SE_1 SE_2 SE_3 SE_4 SI SK"
wd=$(jq -r '.d.wd' config.local.json 2>/dev/null || true)
[ -n "$wd" ] && [ "$wd" != null ] || wd=$(jq -r '.d.wd' config.json)
europe="$wd/../assets/geo/europe.json"
out="$wd/../assets/geo/europe-bidding-zones.json"
ms="npx -y mapshaper@0.6"

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
mkdir "$tmp/src"
for z in $zones; do
    curl -sfL -o "$tmp/src/$z.geojson" "https://raw.githubusercontent.com/EnergieID/entsoe-py/$commit/entsoe/geo/geojson/$z.geojson"
done

# the zones of entsoe-py with the codes of the prices
$ms -i "$tmp"/src/*.geojson combine-files -merge-layers force \
    -each 'id = ({DK_1:"DK1",DK_2:"DK2",NO_1:"NO1",NO_2:"NO2",NO_3:"NO3",NO_4:"NO4",NO_5:"NO5",SE_1:"SE1",SE_2:"SE2",SE_3:"SE3",SE_4:"SE4",IT_NORD:"IT-NORTH",IT_CNOR:"IT-CNORTH",IT_CSUD:"IT-CSOUTH",IT_SUD:"IT-SOUTH",IT_CALA:"IT-Calabria",IT_SARD:"IT-Sardinia",IT_SICI:"IT-Sicily"})[zoneName] || zoneName' \
    -filter-fields id -o "$tmp/zones.geojson" force

# the countries of europe.json without a zone of entsoe-py (San Marino, the
# Vatican and Liechtenstein are part of the zones around them)
$ms -i "$europe" id-field=id \
    -filter '!["AT","BE","BG","CH","CZ","DE","LU","DK","EE","ES","FI","FR","GR","HR","HU","IT","LT","LV","NL","NO","PL","PT","RO","RS","SE","SI","SK","SM","VA","LI"].includes(id)' \
    -each 'id = id == "IE" ? "IE_SEM" : id' -filter-fields id -o "$tmp/rest.geojson" format=geojson force

# Kosovo: the part of more than 5000 km² (it has 10 900), not the slivers of
# the two borders
$ms -i "$europe" id-field=id -filter 'id == "RS"' -erase "$tmp/src/RS.geojson" \
    -explode -filter 'this.area > 5e9' \
    -each 'id = "XK"' -filter-fields id -o "$tmp/xk.geojson" format=geojson force

$ms -i "$tmp/zones.geojson" "$tmp/rest.geojson" "$tmp/xk.geojson" combine-files -merge-layers force \
    -clip bbox=-25,34,45,72 \
    -simplify 4% keep-shapes \
    -clean \
    -rename-layers europe \
    -o "$out" format=topojson quantization=10000 id-field=id force

echo "$out: $(jq '.objects.europe.geometries | length' "$out") areas, $(wc -c < "$out") bytes"
