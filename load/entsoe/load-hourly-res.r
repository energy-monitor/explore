# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/entsoe/_shared.r")
# loadPackages()


# - DOIT -----------------------------------------------------------------------
update.time = now()
d.base = loadEntsoeComb(
    type = "load",
    month.start = "2014-12",
    month.end = month.end
)

d.base.f = d.base[AreaTypeCode == "CTY"]
d.base.f[, hour := (floor_date(DateTime2, unit = "hours"))]

d.agg = d.base.f[, .(
    value = mean(TotalLoadValue, na.rm = TRUE)
), by = .(country = MapCode, DateTime = hour)]


# - STORE ----------------------------------------------------------------------
saveToStorages(d.agg, list(
    id = "electricity-load-hourly-res",
    source = "entsoe",
    format = "csv",
    update.time = update.time
))
