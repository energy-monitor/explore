#!/usr/bin/env python
"""
Daily climate grids of Austria from the GeoSphere Austria Data Hub
(https://data.hub.geosphere.at/, CC BY 4.0, no credentials needed), aggregated
to the political districts:

- SPARTACUS v3: precipitation, temperature (mean, min, max), sunshine duration
- SNOWGRID-CL v2: snow water equivalent, snow depth

Both are 1 km grids since 1961 (EPSG:3416, 329 x 584 cells), the grid covers
Austria and, for SPARTACUS, a buffer into the neighbouring countries. The grids
are fetched in chunks of a few days (the API allows 10 million values per
request) and kept below `d.tmp`, so that only this machine holds the raw data.
Each chunk is reduced to one row per day and district: the mean of every
variable plus the share of the cells with a hot, summer, frost, ice or snow
cover day. Vienna is one district, the municipal districts are dropped.

The data has a lag of about two days and the last seven days are revised, so
chunks are downloaded again until they were fetched seven days after their end.
The one-off backfill takes about 3400 requests, with the rate limit of the API
(240 per hour) roughly 15 hours. The script can be interrupted and resumed,
existing chunks are skipped.

Usage: districts.py [--from YYYY-MM-DD]   (start date, 1961-01-01 by default)
"""

import os, sys, time, json, fcntl, zipfile, argparse
from datetime import date, timedelta, datetime
from pathlib import Path

import numpy as np
import pandas as pd
import xarray as xr
import requests
import geopandas as gpd

ROOT_PATH = Path(__file__).resolve().parents[2]
os.chdir(ROOT_PATH)
sys.path.append(str(ROOT_PATH))

from _shared import getConfig
config = getConfig()

DATA_FOLDER = ROOT_PATH/config['d']['geosphere']
TMP_FOLDER = ROOT_PATH/config['d']['tmp']/'geosphere'
DISTRICTS_FOLDER = DATA_FOLDER/'districts'

DATE_START = date(1961, 1, 1)
REVISION_DAYS = 7

API_BASE = 'https://dataset.api.hub.geosphere.at/v1/grid/historical'
BBOX = '46.16,9.39,49.18,17.39'  # south, west, north, east: the whole grid
GRID_CRS = 'EPSG:3416'
STATE_NAMES = {'1': 'Burgenland', '2': 'Kärnten', '3': 'Niederösterreich', '4': 'Oberösterreich',
               '5': 'Salzburg', '6': 'Steiermark', '7': 'Tirol', '8': 'Vorarlberg', '9': 'Wien'}
# Seconds between two requests, the API allows 240 per hour
REQUEST_INTERVAL = 15.5
REQUEST_RETRIES = 5

DATASETS = {
    'spartacus-v3-1d-1km': {
        'params': ['RR', 'TM', 'TN', 'TX', 'SA'],
        # The API counts about 212k cells per time step (more than the 192k of
        # the grid), 5 params * 9 days stay below the 10 million values
        'days': 9,
    },
    'snowgrid_cl-v2-1d-1km': {
        'params': ['swe_tot', 'snow_depth'],
        'days': 23,
    },
}

# Political districts of Statistik Austria (CC BY 4.0), reference date 1 January
DISTRICTS_YEAR = 2026
DISTRICTS_URL = (
    'https://www.statistik.gv.at/gs-open/GEODATA/ows?service=WFS&version=1.0.0'
    f'&request=GetFeature&typeName=GEODATA:STATISTIK_AUSTRIA_POLBEZ_{DISTRICTS_YEAR}0101'
    '&outputFormat=SHAPE-ZIP&format_options=CHARSET:UTF-8'
)

# Daily indicators as the share of the cells of a district, from the
# variables of the grids (`sa` in seconds, `snow_depth` in metres)
INDICATORS = {
    'hot': ('TX', lambda v: v >= 30),
    'summer': ('TX', lambda v: v >= 25),
    'frost': ('TN', lambda v: v < 0),
    'ice': ('TX', lambda v: v < 0),
    'snow': ('snow_depth', lambda v: v >= 0.01),
}

# Names of the variables in the result
VARIABLES = {
    'RR': 'rr', 'TM': 'tm', 'TN': 'tn', 'TX': 'tx', 'SA': 'sa',
    'swe_tot': 'swe', 'snow_depth': 'snow_depth',
}


# - DISTRICTS ------------------------------------------------------------------
def load_districts():
    """The district polygons in the CRS of the grids, Vienna as one district"""
    DISTRICTS_FOLDER.mkdir(parents=True, exist_ok=True)
    shp = list(DISTRICTS_FOLDER.glob('*.shp'))
    if not shp:
        print('  downloading the district boundaries')
        res = requests.get(DISTRICTS_URL, timeout=300)
        res.raise_for_status()
        zip_file = DISTRICTS_FOLDER/'districts.zip'
        zip_file.write_bytes(res.content)
        with zipfile.ZipFile(zip_file) as z:
            z.extractall(DISTRICTS_FOLDER)
        shp = list(DISTRICTS_FOLDER.glob('*.shp'))

    d = gpd.read_file(shp[0], encoding='utf-8')
    d = d.rename(columns={'g_id': 'district', 'g_name': 'name'})
    # The municipal districts of Vienna (901-923) are part of Vienna (900)
    d = d[~(d.district.str.startswith('9') & (d.district != '900'))]
    d['state'] = d.district.str[0]
    return d[['district', 'name', 'state', 'geometry']].to_crs(GRID_CRS)


def build_mask(districts, x, y):
    """District of every grid cell (index into `districts`, -1 if none)"""
    xx, yy = np.meshgrid(x, y)
    cells = gpd.GeoDataFrame(
        {'cell': np.arange(xx.size)},
        geometry=gpd.points_from_xy(xx.ravel(), yy.ravel()), crs=GRID_CRS
    )
    joined = gpd.sjoin(cells, districts.reset_index(drop=True), how='inner', predicate='within')
    # A cell on a boundary may be in two districts, keep the first
    joined = joined[~joined.index.duplicated()]
    mask = np.full(xx.size, -1, dtype=np.int32)
    mask[joined.cell.values] = joined.index_right.values
    return mask.reshape(xx.shape)


def get_mask(districts, x, y):
    """Mask of the grid, cached as it takes a while to compute"""
    file = DATA_FOLDER/'district-mask.npz'
    if file.exists():
        c = np.load(file)
        if np.array_equal(c['x'], x) and np.array_equal(c['y'], y) and c['mask'].shape == (len(y), len(x)):
            return c['mask']
    print('  building the district mask of the grid')
    mask = build_mask(districts, x, y)
    np.savez_compressed(file, mask=mask, x=x, y=y)
    return mask


# - DOWNLOAD -------------------------------------------------------------------
last_request = 0

def request(url, file):
    """Downloads `url` to `file`, respecting the rate limit of the API"""
    global last_request
    for attempt in range(REQUEST_RETRIES):
        wait = REQUEST_INTERVAL - (time.monotonic() - last_request)
        if wait > 0:
            time.sleep(wait)
        last_request = time.monotonic()
        try:
            res = requests.get(url, timeout=600)
        except requests.RequestException as e:
            print(f'    {e}, retrying')
            time.sleep(60)
            continue
        if res.status_code == 200 and 'json' not in res.headers.get('content-type', ''):
            file.write_bytes(res.content)
            return
        # Only the rate limit and server errors are worth a retry
        if res.status_code != 429 and res.status_code < 500:
            raise RuntimeError(f'HTTP {res.status_code}: {res.text[:300]}\n{url}')
        print(f'    HTTP {res.status_code}: {res.text[:200]}, retrying')
        time.sleep(60 if res.status_code == 429 else 10)
    raise RuntimeError(f'download failed: {url}')


def chunks(dataset, date_from, today):
    """The (nominal) date ranges of the chunks of a data set"""
    days = DATASETS[dataset]['days']
    for year in range(date_from.year, today.year + 1):
        start = date(year, 1, 1)
        while start.year == year and start <= today:
            end = min(start + timedelta(days=days - 1), date(year, 12, 31))
            if end >= date_from:
                yield start, end
            start = end + timedelta(days=1)


def is_final(file, end):
    """A chunk is final if it was downloaded after its revision period"""
    fetched = datetime.fromtimestamp(file.stat().st_mtime).date()
    return fetched > end + timedelta(days=REVISION_DAYS)


def download_chunk(dataset, start, end, today):
    folder = TMP_FOLDER/dataset/str(start.year)
    folder.mkdir(parents=True, exist_ok=True)
    file = folder/f'{start}_{end}.nc'
    if file.exists() and is_final(file, end):
        return file

    params = ','.join(DATASETS[dataset]['params'])
    url = (
        f'{API_BASE}/{dataset}?parameters={params}&start={start}&end={min(end, today)}'
        f'&bbox={BBOX}&output_format=netcdf'
    )
    print(f'  downloading {dataset} {start} - {end}')
    # Downloaded to a temporary file that is renamed once it is readable, an
    # interrupted or invalid download would otherwise be skipped forever
    part = file.with_name(file.name + '.part')
    request(url, part)
    try:
        with xr.open_dataset(part, engine='netcdf4') as d:
            d.load()
    except Exception as e:
        part.unlink()
        raise RuntimeError(f'{url} returned no valid NetCDF file: {e}')
    part.replace(file)
    return file


# - AGGREGATE ------------------------------------------------------------------
def aggregate_chunk(file, districts, mask_file):
    """Daily means and indicator shares per district of a chunk, cached"""
    cache = file.with_suffix('.parquet')
    if cache.exists() and cache.stat().st_mtime > max(file.stat().st_mtime, mask_file.stat().st_mtime):
        return pd.read_parquet(cache)

    with xr.open_dataset(file) as d:
        mask = get_mask(districts, d.x.values, d.y.values).ravel()
        inside = mask >= 0
        idx = mask[inside]
        n_districts = len(districts)
        n_cells = np.bincount(idx, minlength=n_districts)

        rows = []
        for t in d.time.values:
            row = {'date': pd.Timestamp(t).date(), 'district': districts.district.values, 'n': n_cells}
            values = {}
            for param in d.data_vars:
                v = d[param].sel(time=t).values.ravel()[inside]
                values[param] = v
                valid = np.isfinite(v)
                s = np.bincount(idx[valid], weights=v[valid], minlength=n_districts)
                c = np.bincount(idx[valid], minlength=n_districts)
                with np.errstate(invalid='ignore', divide='ignore'):
                    row[VARIABLES[param]] = np.where(c > 0, s / c, np.nan)
            for name, (param, cond) in INDICATORS.items():
                if param in values:
                    v = values[param]
                    valid = np.isfinite(v)
                    s = np.bincount(idx[valid], weights=cond(v[valid]).astype(float), minlength=n_districts)
                    c = np.bincount(idx[valid], minlength=n_districts)
                    with np.errstate(invalid='ignore', divide='ignore'):
                        row[f'share_{name}'] = np.where(c > 0, s / c, np.nan)
            rows.append(pd.DataFrame(row))

    result = pd.concat(rows, ignore_index=True) if rows else pd.DataFrame()
    result.to_parquet(cache, index=False)
    return result


def write_topology(districts, file):
    """The districts and the states for the maps of the web page as one
    topology, simplified along their shared arcs: simplified one by one the
    borders of neighbours differ, their gaps show as lines, e.g. the borders
    of the states of geo:mesh"""
    import topojson
    states = districts.dissolve('state', as_index=False).assign(
        district=lambda x: x.state, name=lambda x: x.state.map(STATE_NAMES))
    layers = [d.to_crs('EPSG:4326').rename(columns={'district': 'id', 'name': 'NAME'})[['id', 'NAME', 'geometry']]
              for d in (districts, states)]
    # toposimplify is in degrees, 0.003 about 300 m
    topo = json.loads(topojson.Topology(layers, object_name=['districts', 'states'], prequantize=1e5,
                                        topology=True, toposimplify=0.003).to_json())
    # The id of a geometry is its row number, the maps join by the district
    for o in topo['objects'].values():
        for geometry in o['geometries']:
            geometry['id'] = geometry['properties']['id']
    file.write_text(json.dumps(topo, ensure_ascii=False))


# - MAIN -----------------------------------------------------------------------
if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--from', dest='date_from', type=date.fromisoformat, default=DATE_START)
    args = parser.parse_args()

    # Only one run at a time, e.g. the nightly run during the backfill: two runs
    # download the same chunks and break each other's files
    TMP_FOLDER.mkdir(parents=True, exist_ok=True)
    lock = open(TMP_FOLDER/'districts.lock', 'w')
    try:
        fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
    except BlockingIOError:
        sys.exit('another run of districts.py is still going, stopping')

    today = date.today()
    DATA_FOLDER.mkdir(parents=True, exist_ok=True)
    districts = load_districts()
    mask_file = DATA_FOLDER/'district-mask.npz'

    results = []
    for dataset in DATASETS:
        parts = []
        for start, end in chunks(dataset, args.date_from, today):
            file = download_chunk(dataset, start, end, today)
            parts.append(aggregate_chunk(file, districts, mask_file))
        d = pd.concat([p for p in parts if len(p)], ignore_index=True)
        # A day may be in two chunks if the data of a chunk was still missing
        # when it was fetched, keep the latest one
        d = d.drop_duplicates(['date', 'district'], keep='last')
        results.append(d.set_index(['date', 'district']))

    d = results[0].join([r.drop(columns='n') for r in results[1:]], how='outer').reset_index()
    d = d.sort_values(['date', 'district'])
    d['date'] = pd.to_datetime(d.date)

    # Units: sunshine in hours, snow depth in cm
    d['sa'] = d.sa / 3600
    d['snow_depth'] = d.snow_depth * 100

    d.to_parquet(DATA_FOLDER/'districts-daily.parquet', index=False)

    mask = np.load(mask_file)['mask'].ravel()
    districts.drop(columns='geometry').assign(
        cells=np.bincount(mask[mask >= 0], minlength=len(districts))
    ).to_csv(DATA_FOLDER/'districts.csv', index=False)

    geo_folder = ROOT_PATH/config['d']['wd']/'..'/'assets'/'geo'
    if geo_folder.exists():
        write_topology(districts, geo_folder/'austria-districts.json')
    print(f'  {len(d)} rows, {d.date.min().date()} - {d.date.max().date()}')
