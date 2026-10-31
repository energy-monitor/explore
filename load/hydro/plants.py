#!/usr/bin/env python
"""
The Austrian hydro power plants and the river basins upstream of each of them,
the base of the precipitation and snow indicators by catchment (see
`load/geosphere/catchments.py`):

- Plants: JRC Hydro-power plants database (CC BY 4.0,
  https://github.com/energy-modelling-toolkit/hydro-power-database), all plants
  in Austria with type (HROR run-of-river, HDAM reservoir, HPHS pumped storage),
  capacity, coordinates and partly the average annual generation. The shared
  plants on the Inn and the Danube at the German border are listed as Austrian.
- Basins: HydroBASINS level 12 (HydroSHEDS, https://www.hydrosheds.org/,
  free for any use with attribution), sub-basins of about 130 km² with the
  next basin downstream, so that all basins upstream of a plant can be
  collected. The catchments reach far beyond Austria, the Danube at Vienna
  drains about 100,000 km² (mostly Bavaria, the Inn also Switzerland).

A plant belongs to the level 12 basin its coordinates lie in. For a plant
close to a confluence that may be the wrong river, `UP_AREA_HINTS` picks the
basin with the closest upstream area within `HINT_RADIUS` instead.

Output in `data/hydro`:
- `plants.csv`: one row per plant with its basin and upstream area (km²)
- `plant-basins.csv`: the basins upstream of every plant (incl. its own)
- `basins.gpkg`: the polygons of all these basins (EPSG:4326)
"""

import os, sys, zipfile
from pathlib import Path

import numpy as np
import pandas as pd
import geopandas as gpd
import requests

ROOT_PATH = Path(__file__).resolve().parents[2]
os.chdir(ROOT_PATH)
sys.path.append(str(ROOT_PATH))

from _shared import getConfig
config = getConfig()

DATA_FOLDER = ROOT_PATH/config['d']['hydro']
SRC_FOLDER = DATA_FOLDER/'src'

PLANTS_URL = ('https://raw.githubusercontent.com/energy-modelling-toolkit/hydro-power-database/'
              'master/data/jrc-hydro-power-plant-database.csv')
BASINS_URL = 'https://data.hydrosheds.org/file/HydroBASINS/standard/hybas_eu_lev12_v1c.zip'

# Basins loaded (west, south, east, north): the Danube up to its source in the
# Black Forest, the Rhine above Lake Constance, the Drau from South Tyrol
BBOX = (7.0, 45.0, 18.0, 51.0)
# Metric CRS for the distances (the one of the GeoSphere grids)
CRS = 'EPSG:3416'

# Plants left out: the coordinates are far off and the plant is not Austrian
DROP = {
    'H487': 'KW Laufenburg, the Rhine plant at the Swiss-German border, placed in Upper Austria',
}

# Upstream area (km²) of the river a plant is on, if its coordinates fall into
# the basin of another river
HINT_RADIUS = 3000  # m
UP_AREA_HINTS = {
    'H822': 6080,  # St. Pantaleon on the Enns, 2 km above its mouth (not the Danube)
    'H1503': 4700,  # Dionysen on the Mur above the Mürz (not the Mürz)
}


def download(url, file):
    if not file.exists():
        print(f'  downloading {url}')
        res = requests.get(url, timeout=600)
        res.raise_for_status()
        file.write_bytes(res.content)
    return file


def load_plants():
    d = pd.read_csv(download(PLANTS_URL, SRC_FOLDER/'jrc-hydro-power-plant-database.csv'))
    d = d[(d.country_code == 'AT') & ~d.id.isin(DROP.keys())]
    d = d.rename(columns={'installed_capacity_MW': 'capacity', 'pumping_MW': 'pumping',
                          'avg_annual_generation_GWh': 'generation'})
    d = d[['id', 'name', 'type', 'capacity', 'pumping', 'generation', 'lat', 'lon']]
    return gpd.GeoDataFrame(d, geometry=gpd.points_from_xy(d.lon, d.lat), crs='EPSG:4326').to_crs(CRS)


def load_basins():
    zip_file = download(BASINS_URL, SRC_FOLDER/'hybas_eu_lev12_v1c.zip')
    shp = SRC_FOLDER/'hybas_eu_lev12_v1c.shp'
    if not shp.exists():
        with zipfile.ZipFile(zip_file) as z:
            z.extractall(SRC_FOLDER, [n for n in z.namelist() if not n.endswith(('.pdf', '.xml'))])
    b = gpd.read_file(shp, bbox=BBOX)
    return b[['HYBAS_ID', 'NEXT_DOWN', 'SUB_AREA', 'UP_AREA', 'geometry']].rename(columns=str.lower)


def plant_basins(plants, basins):
    """The basin of every plant, by location or by the hint of its upstream area"""
    b = basins.to_crs(CRS)
    j = gpd.sjoin(plants, b[['hybas_id', 'up_area', 'geometry']], how='left', predicate='within')
    j = j[~j.index.duplicated()]
    result = j.hybas_id.copy()

    for id, area in UP_AREA_HINTS.items():
        p = plants[plants.id == id]
        if p.empty:
            continue
        near = b[b.intersects(p.geometry.iloc[0].buffer(HINT_RADIUS))]
        best = near.iloc[np.argmin(np.abs(np.log(near.up_area / area)))]
        result[p.index[0]] = best.hybas_id

    missing = plants[result.isna()]
    if len(missing):
        raise RuntimeError(f'plants outside the basins: {missing.id.tolist()}')
    return result.astype('int64')


def upstream(basins):
    """Function giving all basins upstream of a basin (incl. itself)"""
    children = basins.groupby('next_down').hybas_id.apply(list).to_dict()
    cache = {}

    def collect(id):
        if id not in cache:
            ids = [id]
            stack = list(children.get(id, []))
            while stack:
                c = stack.pop()
                ids.append(c)
                stack.extend(children.get(c, []))
            cache[id] = ids
        return cache[id]

    return collect


# - MAIN -----------------------------------------------------------------------
if __name__ == '__main__':
    SRC_FOLDER.mkdir(parents=True, exist_ok=True)
    plants = load_plants()
    basins = load_basins()

    plants['hybas_id'] = plant_basins(plants, basins)
    collect = upstream(basins)
    sub_area = basins.set_index('hybas_id').sub_area
    up_area = basins.set_index('hybas_id').up_area

    links = pd.DataFrame(
        [(id, b) for id, h in zip(plants.id, plants.hybas_id) for b in collect(h)],
        columns=['plant', 'hybas_id']
    )

    # The basins collected must add up to the upstream area of HydroBASINS,
    # otherwise the box of the basins cuts the catchment
    plants['up_area'] = plants.hybas_id.map(up_area)
    collected = links.assign(area=links.hybas_id.map(sub_area)).groupby('plant').area.sum()
    gap = (1 - plants.id.map(collected) / plants.up_area).abs()
    if (gap > 0.01).any():
        raise RuntimeError(f'catchments cut by the box: {plants.id[gap > 0.01].tolist()}')

    DATA_FOLDER.mkdir(parents=True, exist_ok=True)
    plants.drop(columns='geometry').to_csv(DATA_FOLDER/'plants.csv', index=False)
    used = basins[basins.hybas_id.isin(links.hybas_id)]
    # Rewritten only if the catchments changed, the chunks of the grids are
    # aggregated again once they are newer than the cache
    links_file = DATA_FOLDER/'plant-basins.csv'
    if not links_file.exists() or not pd.read_csv(links_file).equals(links):
        used.to_file(DATA_FOLDER/'basins.gpkg', driver='GPKG')
        links.to_csv(links_file, index=False)

    print(f'  {len(plants)} plants, {len(used)} basins')
    print(plants.groupby('type').agg(n=('id', 'size'), capacity=('capacity', 'sum')).to_string())
