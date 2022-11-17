# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/entsoe/_shared.r")
# loadPackages()


# - DOIT -----------------------------------------------------------------------
d.base = loadEntsoeComb(
    type = "dayAheadPrices", month.start = month.start, month.end = month.end #, check.updates = FALSE
    # type = "load", month.start = "2019-12", month.end = month.end, check.updates = TRUE
)

# unique(d.base[, .(ResolutionCode, AreaCode, AreaTypeCode, AreaName, MapCode)])
# d.t = unique(d.base[AreaTypeCode == "CTY", .(ResolutionCode, AreaCode, AreaName, MapCode)])

d.base.f = d.base[AreaCode == "10YAT-APG------L" & ResolutionCode == "PT60M"]
# unique(d.base.f[, .(ResolutionCode, AreaCode, AreaTypeCode, AreaName, MapCode)])

d.agg = d.base.f[, .(
    mean = mean(Price),
    max = max(Price),
    min = min(Price)
), by=.(
    date = as.Date(DateTime)
)]

# sort(unique(d.base.f$ResolutionCode))

# d.base.f[, factor := resToFactor[ResolutionCode]]
# d.base.f[, value := factor*TotalLoadValue]

# # Filter, Aggregate
# d.agg = d.base.f[, .(
#     value = sum(value)/10^3
# ), by = .(country = MapCode, date = as.Date(DateTime))][order(date)]

# Delete last (most probably incomplete) obs
d.agg = removeLastDays(d.agg, 1)


# # - STORE ----------------------------------------------------------------------
saveToStorages(d.agg, list(
    id = "electricity-price-entsoe",
    source = "entsoe",
    format = "csv"
))
