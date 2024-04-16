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

d.base.f = d.base[AreaCode == "10YAT-APG------L" & ResolutionCode == "PT60M",]

d.agg = d.base.f[, .(mean = mean(Price),
                                    max = max(Price),
                                    min = min(Price)), by = .(date = as.Date(DateTime))][order(date)]

d.base.f.hourly = d.base[ResolutionCode == "PT60M",
                  hour := floor_date(DateTime, unit = "hours")]

d.agg.hours = d.base.f.hourly[, .(price = mean(Price)), by = .(country = MapCode,
                                                        DateTime = hour,
                                                        ResolutionCode)][order(country, DateTime)]

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

