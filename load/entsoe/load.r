# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/entsoe/_shared.r")
# loadPackages()


# - DOIT -----------------------------------------------------------------------
d.base = loadEntsoeComb(
    type = "load", month.start = month.start, month.end = month.end
    # type = "load", month.start = "2019-12", month.end = month.end, check.updates = TRUE
)

# d.t = unique(d.base[, .(ResolutionCode, AreaCode, AreaTypeCode, AreaName, MapCode)])
# d.t = unique(d.base[AreaTypeCode == "CTY", .(ResolutionCode, AreaCode, AreaName, MapCode)])

d.base.f = d.base[AreaTypeCode == "CTY"]

# sort(unique(d.base.f$ResolutionCode))

d.base.f[, factor := resToFactor[ResolutionCode]]
d.base.f[, value := factor*TotalLoadValue]

# Filter, Aggregate
d.agg = d.base.f[, .(
    value = sum(value)/10^6
), by = .(country = MapCode, date = as.Date(DateTime))][order(date)]

# Delete last (most probably incomplete) obs
d.agg = removeLastDays(d.agg, 2)


# - STORE ----------------------------------------------------------------------
saveToStorages(d.agg, list(
    id = "electricity-load",
    source = "entsoe",
    format = "csv"
))
