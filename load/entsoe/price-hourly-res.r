# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/entsoe/_shared.r")
# loadPackages()


# - DOIT -----------------------------------------------------------------------
update.time = now()
d.base = loadEntsoeComb(
    type = "dayAheadPrices",
    month.start = month.start,
    month.end = month.end
)

d.base.f = d.base[ResolutionCode == "PT60M"]
d.base.f = d.base.f[, hour := floor_date(DateTime2, unit = "hours")]

d.agg = d.base.f[, .(
    price = mean(Price)
), by = .(
    country = MapCode,
    DateTime = hour,
    ResolutionCode
)][order(country, DateTime)]


# # - STORE --------------------------------------------------------------------
saveToStorages(
    d.agg,
    list(
        id = "electricity-price-entsoe-hourly",
        source = "entsoe",
        format = "csv",
        update.time = update.time
    )
)

