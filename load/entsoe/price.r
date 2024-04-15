# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/entsoe/_shared.r")
# loadPackages()


# - DOIT -----------------------------------------------------------------------
update.time = now()
d.base = loadEntsoeComb(#type = "dayAheadPrices", month.start = month.start, month.end = month.end
    type = "dayAheadPrices",
    month.start = month.start,
    month.end = month.end)

# type = "load", month.start = "2019-12", month.end = month.end, check.updates = TRUE)

d.base.f = d.base[,
                  hour := floor_date(DateTime, unit = "hours")]

d.agg = d.base.f[MapCode == "AT", .(mean = mean(Price),
                                    max = max(Price),
                                    min = min(Price)), by = .(date = as.Date(DateTime))]

d.agg.hours = d.base.f[, .(price = mean(Price)), by = .(country = MapCode,
                                                        DateTime = hour,
                                                        ResolutionCode)]

# Delete last (most probably incomplete) obs
d.agg = removeLastDays(d.agg, 1)


# # - STORE --------------------------------------------------------------------
saveToStorages(
    d.agg,
    list(
        id = "electricity-price-entsoe",
        source = "entsoe",
        format = "csv",
        update.time = update.time
    )
)

saveToStorages(
    d.agg.hours,
    list(
        id = "electricity-price-entsoe-hourly",
        source = "entsoe",
        format = "csv",
        update.time = update.time
    )
)
