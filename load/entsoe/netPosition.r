# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/entsoe/_shared.r")
# loadPackages()


# - DOIT -----------------------------------------------------------------------
d.base = loadEntsoeComb(
    type = "netPositions", 
    month.start = month.start, month.end = month.end
    # month.start = "2024-10", month.end = "2024-10", check.updates = TRUE
)

# d.t = unique(d.base[, .(ResolutionCode, AreaCode, AreaTypeCode, AreaName, MapCode)])
# d.t = unique(d.base[AreaTypeCode == "CTY", .(ResolutionCode, AreaCode, AreaName, MapCode)])

# d.base.f = d.base[AreaTypeCode == "CTY"]

# sort(unique(d.base.f$ResolutionCode))

# d.base.f[, factor := resToFactor[ResolutionCode]]
# d.base.f[, value := factor*TotalLoadValue]

# # Filter, Aggregate
# d.agg = d.base.f[, .(
#     value = sum(value)/10^3
# ), by = .(country = MapCode, date = as.Date(DateTime))][order(date)]

# # Delete last (most probably incomplete) obs
# d.agg = removeLastDays(d.agg, 2)


# # - STORE ----------------------------------------------------------------------
# saveToStorages(d.agg, list(
#     id = "electricity-load",
#     source = "entsoe",
#     format = "csv"
# ))
