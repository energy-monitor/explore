# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/entsoe/_shared.r")
#loadPackages(ggplot2)

# - DOIT -----------------------------------------------------------------------
update.time = now()
d.base = loadEntsoeComb(
    type = "load", month.start = month.start, month.end = month.end
    # type = "load", month.start = "2022-08", month.end = month.end, check.updates = FALSE
)

d.base.f = d.base[AreaName == "AT CTY"]

# sort(unique(d.base.f$ResolutionCode))

d.base.f[, factor := resToFactor[ResolutionCode]]
d.base.f[, value := factor * TotalLoadValue]

d = d.base[AreaName == "AT CTY", .(
    dateTime = DateTime, load = TotalLoadValue
)]

rm(d.base)

# Filter, Aggregate
d.agg = d[, .(
    value = mean(load)/10^3
), by = .(
    year = year(dateTime),
    hour = hour(dateTime)
)][year > 2018][order(year, hour)]


# - STORAGE --------------------------------------------------------------------
saveToStorages(d.agg, list(
    id = "electricity-load-hourly-year",
    source = "entsoe",
    format = "csv",
    update.time = update.time
))
