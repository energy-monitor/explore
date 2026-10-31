"""The parquet files of the web written again with smaller encodings, the values
stay the same, called by fwriteWeb (export/data/_functions.r) for each file:

    python export/data/_parquet.py <file.parquet>...

- doubles as decimals, integers with the number of decimals of the column,
  e.g. 164.732 as 164732 of DECIMAL(9, 3), whole numbers (e.g. the year) as
  INT32, with DELTA_BINARY_PACKED as the dates and integers, the values of
  roundSignif have few decimals but random bits as doubles, which neither
  snappy nor a dictionary compress (2026-10: all files 3.3 MB, 1.5 MB as
  decimals)
- doubles which are no decimal of at most 18 digits stay doubles, e.g. NaN,
  Inf or values of more than 15 decimals
- strings with a dictionary, as arrow writes them
- snappy, which gen-vis reads without loading further decompressors (zstd is
  only 2% smaller)
- without the schemas of arrow and R in the metadata, gen-vis does not use them

hyparquet (of gen-vis) reads the decimals as numbers. The file is read again
and compared with the one of arrow, it is only replaced if all values are the
same.
"""
import os
import sys

import numpy as np
import pyarrow as pa
import pyarrow.compute as pc
import pyarrow.parquet as pq

# two doubles are the same value, e.g. 164.732 and 164732 / 1000
RTOL = 1e-12


def decimals(v):
    """The fewest decimals of the finite values v, None if more than 15."""
    for d in range(16):
        if np.all(np.abs(np.round(v, d) - v) <= RTOL * np.abs(v)):
            return d
    return None


def encoded(col):
    """The column as decimals (or integers) and its encoding, the column as it
    is and None if it stays a double."""
    x = col.to_numpy(zero_copy_only=False)
    # NaN and Inf are no decimals, nulls are
    if np.count_nonzero(~np.isfinite(x)) != col.null_count:
        return col, None
    v = x[np.isfinite(x)]
    if len(v) == 0:
        return col, None
    d = decimals(v)
    if d is None:
        return col, None
    digits = len(str(int(np.max(np.abs(v))))) + d
    mask = np.isnan(x)
    if d == 0 and digits <= 9:
        return pa.array(x, mask=mask).cast(pa.int32()), 'DELTA_BINARY_PACKED'
    if d == 0 and digits <= 15:
        return pa.array(x, mask=mask).cast(pa.int64()), 'DELTA_BINARY_PACKED'
    if digits <= 9:
        return pa.array(np.round(x, d), mask=mask).cast(pa.decimal32(9, d)), 'DELTA_BINARY_PACKED'
    if digits <= 18:
        return pa.array(np.round(x, d), mask=mask).cast(pa.decimal64(18, d)), 'DELTA_BINARY_PACKED'
    return col, None


def delta(t):
    return pa.types.is_integer(t) or pa.types.is_date(t) or pa.types.is_timestamp(t)


def same(a, b):
    """The columns have the same values, decimals as doubles, factors of R
    (dictionaries of arrow) are read again as strings."""
    a, b = [c.dictionary_decode() if pa.types.is_dictionary(c.type) else c for c in (a, b)]
    if a.null_count != b.null_count or not pc.all(pc.equal(pc.is_null(a), pc.is_null(b))).as_py():
        return False
    if pa.types.is_floating(a.type):
        x = a.to_numpy(zero_copy_only=False)
        y = b.cast(pa.float64()).to_numpy(zero_copy_only=False)
        return bool(np.all((np.isnan(x) & np.isnan(y)) | (np.abs(x - y) <= RTOL * np.abs(x))))
    return a.equals(b)


def rewrite(file):
    t = pq.read_table(file)
    cols, encodings = [], {}
    for name, col in zip(t.column_names, t.columns):
        col = col.combine_chunks()
        if pa.types.is_floating(col.type):
            col, encoding = encoded(col)
        else:
            encoding = 'DELTA_BINARY_PACKED' if delta(col.type) else None
        cols.append(col)
        if encoding:
            encodings[name] = encoding
    new = pa.table(cols, names=t.column_names)

    tmp = file + '.tmp'
    pq.write_table(new, tmp, compression='snappy',
                   use_dictionary=[n for n in t.column_names if n not in encodings],
                   column_encoding=encodings, store_decimal_as_integer=True,
                   store_schema=False)
    back = pq.read_table(tmp)
    differs = [n for n in t.column_names if not same(t[n].combine_chunks(), back[n].combine_chunks())]
    if back.num_rows != t.num_rows or differs:
        os.remove(tmp)
        raise ValueError(f'{file}: other values after the encoding, {differs or "rows"}')
    before = os.path.getsize(file)
    os.replace(tmp, file)
    print(f'{file}: {before} -> {os.path.getsize(file)} bytes')


if __name__ == '__main__':
    for f in sys.argv[1:]:
        rewrite(f)
