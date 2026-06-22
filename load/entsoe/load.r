# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/entsoe/_shared.r")
# loadPackages()


# - DOIT -----------------------------------------------------------------------
update.time = now()
d.base = load_entsoe(
    c.nice2entsoe["load"], from = date.start#, to = "2026-01-01"
)


d.base.f = d.base[grep("CTY", AreaTypeCode, fixed = TRUE)]

d.base.f[, factor := c.resToFactor[ResolutionCode]]
# d.base.f[, value := factor * TotalLoadValue]
d.base.f[, value := factor * `TotalLoad[MW]`]

# Filter, Aggregate
d.agg = d.base.f[, .(
    value = sum(value) / 10^6
), by = .(country = AreaMapCode, date = as.Date(`DateTime(UTC)`))][order(date)]

# Delete last (most probably incomplete) obs
d.agg = removeLastDays(d.agg, 2)


# - STORE ----------------------------------------------------------------------
saveToStorages(d.agg, list(
    id = "electricity-load",
    source = "entsoe",
    format = "csv",
    update.time = update.time
))


d.base.f.hourly = d.base[grep("CTY", AreaTypeCode, fixed = TRUE)]

setnames(d.base.f.hourly, "DateTime(UTC)", "DateTime")
d.base.f.hourly[, hour := (floor_date(DateTime, unit = "hours"))]

d.agg = d.base.f.hourly[, .(
    value = mean(`TotalLoad[MW]`, na.rm = TRUE)
), by = .(country = AreaMapCode, DateTime = hour)]


# - STORE ----------------------------------------------------------------------
saveToStorages(d.agg, list(
    id = "electricity-load-hourly-res",
    source = "entsoe",
    format = "csv",
    update.time = update.time
))

