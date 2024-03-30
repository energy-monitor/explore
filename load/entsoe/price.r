# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/entsoe/_shared.r")
# loadPackages()


# - DOIT -----------------------------------------------------------------------
update.time = now()
d.base = loadEntsoeComb(
    #type = "dayAheadPrices", month.start = month.start, month.end = month.end
    type = "dayAheadPrices", month.start = month.start, month.end = month.end

    # type = "load", month.start = "2019-12", month.end = month.end, check.updates = TRUE
)

# unique(d.base[, .(ResolutionCode, AreaCode, AreaTypeCode, AreaName, MapCode)])
# d.t = unique(d.base[AreaTypeCode == "CTY", .(ResolutionCode, AreaCode, AreaName, MapCode)])

#d.base.f = d.base[AreaCode == "10YAT-APG------L" & ResolutionCode == "PT60M"]
d.base.f = d.base[,
                  hour := cut(DateTime, breaks = "hour")]

# unique(d.base.f[, .(ResolutionCode, AreaCode, AreaTypeCode, AreaName, MapCode)])

d.agg = d.base.f[, .(
    mean = mean(Price),
    max = max(Price),
    min = min(Price)
), by = .(
    date = as.Date(DateTime)
)]

d.agg.hours = d.base.f[, .(
    mean = mean(Price),
    max = max(Price),
    min = min(Price)
), by = .(
    AreaName,
    DateTime = ymd_hms(hour)
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


# # - STORE --------------------------------------------------------------------
saveToStorages(d.agg, list(
    id = "electricity-price-entsoe",
    source = "entsoe",
    format = "csv",
    update.time = update.time
))

saveToStorages(d.agg.hours, list(
    id = "electricity-price-entsoe-hourly",
    source = "entsoe",
    format = "csv",
    update.time = update.time
))
