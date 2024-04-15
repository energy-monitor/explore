# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/entsoe/_shared.r")
# loadPackages()


# - DOIT -----------------------------------------------------------------------
update.time = now()
d.base = loadEntsoeComb(
    type = "load", month.start = "2014-12", month.end = month.end
    # type = "load", month.start = "2019-12", month.end = month.end, check.updates = TRUE
)

d.base.f = d.base[AreaTypeCode == "CTY"]

d.base.f[, factor := resToFactor[ResolutionCode]]

d.base.f[,  hour := (floor_date(DateTime, unit = "hours"))]

d.agg.hours = d.base.f[, .(
    value = mean(value / factor, na.rm = TRUE)
), by = .(
    country = MapCode,
    DateTime = hour
)]

# Filter, Aggregate
d.agg = d.agg.hours[, .(
    value = sum(value) / 10^6
), by = .(country = country, date = as.Date(DateTime))][order(date)]


# Delete last (most probably incomplete) obs
d.agg = removeLastDays(d.agg, 2)


# - STORE ----------------------------------------------------------------------
saveToStorages(d.agg, list(
    id = "electricity-load",
    source = "entsoe",
    format = "csv",
    update.time = update.time
))

saveToStorages(d.agg.hours, list(
    id = "electricity-load-hourly-res",
    source = "entsoe",
    format = "csv",
    update.time = update.time
))
