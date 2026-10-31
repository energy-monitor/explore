#!/usr/bin/env python
"""
Daily precipitation, temperature and snow water equivalent of the GeoSphere
grids averaged over the catchment of every Austrian hydro power plant, as the
indicator of the inflow of the plants (see `load/hydro/plants.py` for the
plants and their basins).

The raw grids are the chunks `districts.py` downloads to `d.tmp`, this script
does not download anything and should run after it. A cell belongs to the
level 12 basin its centre lies in, the catchment of a plant is the union of
its upstream basins. Only the part within the grids counts: the precipitation
grid covers Austria plus a buffer, the snow grid Austria only, so the mean of
a plant on the Danube or the Inn misses most of its catchment. The cells of
each catchment are in `catchments.csv`, an area of 1 km² each.

Output in `data/geosphere`:
- `catchments-daily.parquet`: date, plant, rr (mm), tm (°C), swe (mm)
- `catchments.csv`: plant, cells with precipitation, cells with snow
"""

import os, sys
from datetime import date
from pathlib import Path

import numpy as np
import pandas as pd
import xarray as xr
import geopandas as gpd

from districts import config, ROOT_PATH, DATA_FOLDER, TMP_FOLDER, DATASETS, DATE_START, GRID_CRS, chunks

HYDRO_FOLDER = ROOT_PATH/config['d']['hydro']
MASK_FILE = DATA_FOLDER/'basin-mask.npz'

# Variables of the grids and their names in the result
VARIABLES = {
    'spartacus-v3-1d-1km': {'RR': 'rr', 'TM': 'tm'},
    'snowgrid_cl-v2-1d-1km': {'swe_tot': 'swe'},
}


def get_mask(basins, x, y):
    """Index of the basin (into `basins`) of every grid cell, -1 if none, cached"""
    if MASK_FILE.exists():
        c = np.load(MASK_FILE)
        if np.array_equal(c['x'], x) and np.array_equal(c['y'], y) and np.array_equal(c['ids'], basins.hybas_id):
            return c['mask']
    print('  building the basin mask of the grid')
    xx, yy = np.meshgrid(x, y)
    cells = gpd.GeoDataFrame(geometry=gpd.points_from_xy(xx.ravel(), yy.ravel()), crs=GRID_CRS)
    joined = gpd.sjoin(cells, basins.reset_index(drop=True), how='inner', predicate='within')
    joined = joined[~joined.index.duplicated()]
    mask = np.full(xx.size, -1, dtype=np.int32)
    mask[joined.index.values] = joined.index_right.values
    mask = mask.reshape(xx.shape)
    np.savez_compressed(MASK_FILE, mask=mask, x=x, y=y, ids=basins.hybas_id.values)
    return mask


def aggregate_chunk(file, dataset, basins, membership, inputs):
    """Daily means of the variables per catchment of a chunk, cached"""
    cache = file.with_name(file.stem + '.catchments.parquet')
    if cache.exists() and cache.stat().st_mtime > max(f.stat().st_mtime for f in [file, *inputs]):
        return pd.read_parquet(cache)

    n_basins = len(basins)
    with xr.open_dataset(file) as d:
        mask = get_mask(basins, d.x.values, d.y.values).ravel()
        inside = mask >= 0
        idx = mask[inside]
        dates = pd.to_datetime(d.time.values).date
        result = pd.DataFrame({
            'date': np.repeat(dates, membership.shape[1]),
            'plant': np.tile(membership.columns, len(dates)),
        })
        for param, name in VARIABLES[dataset].items():
            v = d[param].transpose('time', 'y', 'x').values.reshape(len(dates), -1)[:, inside]
            valid = np.isfinite(v)
            sums = np.stack([np.bincount(idx[ok], weights=row[ok], minlength=n_basins) for row, ok in zip(v, valid)])
            counts = np.stack([np.bincount(idx[ok], minlength=n_basins) for ok in valid])
            # From the basins to the catchments of the plants
            sums, counts = sums @ membership.values, counts @ membership.values
            with np.errstate(invalid='ignore', divide='ignore'):
                result[name] = np.where(counts > 0, sums / counts, np.nan).ravel()
            result[f'cells_{name}'] = counts.ravel()

    result.to_parquet(cache, index=False)
    return result


# - MAIN -----------------------------------------------------------------------
if __name__ == '__main__':
    basins = gpd.read_file(HYDRO_FOLDER/'basins.gpkg').to_crs(GRID_CRS)[['hybas_id', 'geometry']]
    links = pd.read_csv(HYDRO_FOLDER/'plant-basins.csv')
    # Basins (rows) by plants (columns), 1 if the basin drains to the plant
    membership = pd.crosstab(links.hybas_id, links.plant).reindex(basins.hybas_id, fill_value=0)
    inputs = [HYDRO_FOLDER/'plant-basins.csv']

    today = date.today()
    results = []
    for dataset in VARIABLES:
        parts = []
        for start, end in chunks(dataset, DATE_START, today):
            file = TMP_FOLDER/dataset/str(start.year)/f'{start}_{end}.nc'
            if file.exists():
                parts.append(aggregate_chunk(file, dataset, basins, membership, inputs))
        if not parts:
            sys.exit(f'no chunks of {dataset} in {TMP_FOLDER}, run districts.py first')
        d = pd.concat([p for p in parts if len(p)], ignore_index=True)
        # A day may be in two chunks, keep the latest one (as districts.py)
        results.append(d.drop_duplicates(['date', 'plant'], keep='last').set_index(['date', 'plant']))

    d = results[0].join(results[1:], how='outer').reset_index().sort_values(['date', 'plant'])
    d['date'] = pd.to_datetime(d.date)

    # The cells of a catchment as of the last day with data
    cells = [c for c in d.columns if c.startswith('cells_')]
    last = d.dropna(subset=['rr', 'swe']).date.max()
    d[d.date == last][['plant', *cells]].to_csv(DATA_FOLDER/'catchments.csv', index=False)

    d.drop(columns=cells).to_parquet(DATA_FOLDER/'catchments-daily.parquet', index=False)
    print(f'  {len(d)} rows, {d.date.min().date()} - {d.date.max().date()}')
