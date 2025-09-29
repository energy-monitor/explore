# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/entsoe/_shared.r")
#loadPackages(ggplot2)

# - DOIT -----------------------------------------------------------------------
update.time = now()
d.base = load_entsoe_data(
    c.nice2entsoe["load"], from = date.start
)

fromDate = today() %m-% months(1)

d.base.f = d.base[AreaName == "AT CTY"]

# sort(unique(d.base.f$ResolutionCode))

d.base.f[, factor := c.resToFactor[ResolutionCode]]
d.base.f[, value := factor * TotalLoadValue]

d = d.base.f[AreaName == "AT CTY" & DateTime >= fromDate, .(
    dateTime = DateTime, load = TotalLoadValue
)]

rm(d.base)

# Filter, Aggregate
d.agg = d[, .(
    value = mean(load)/10^3
), by = .(
    date = as.Date(dateTime),
    hour = hour(dateTime)
)][order(date, hour)]


# - STORAGE --------------------------------------------------------------------
saveToStorages(d.agg, list(
    id = "electricity-load-recent",
    source = "entsoe",
    format = "csv",
    update.time = update.time
))
