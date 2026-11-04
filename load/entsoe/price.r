# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/entsoe/_shared.r")
# loadPackages()


# - DOIT -----------------------------------------------------------------------
update.time = now()
d.base = load_entsoe(
    c.nice2entsoe["dayAheadPrices"], from = date.start
)
setnames(d.base, "DateTime(UTC)", "DateTime")

# Since the revision r3.1 (October 2026) the files also contain intraday
# prices (ContractType) and sometimes a timestamp twice from two publications
# (InstanceCode, UpdateTime), the last one counts
d.base = d.base[ContractType == "Day-ahead"]
d.base = d.base[Sequence == 1 | is.na(Sequence)]
d.base = d.base[ResolutionCode %in% c("PT60M", "PT15M")]
d.base = d.base[order(`UpdateTime(UTC)`)]
d.base = d.base[!duplicated(d.base, by = c("AreaMapCode", "DateTime", "ResolutionCode"), fromLast = TRUE)]

d.base[, hour := floor_date(DateTime, unit = "hours")]

d.agg = d.base[, .(
    price = mean(`Price[Currency/MWh]`)
), by = .(
    country = AreaMapCode,
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


# The daily prices of all bidding zones (AreaMapCode, e.g. AT, DE_LU, IT_NORD)
# in EUR: the mean, the least and the largest one of a day (UTC, as the ones of
# Austria below), of the finest resolution of the day, e.g. the quarter hours
# since the 15 minute products of October 2025 when a zone has both
d.zones = d.base[Currency == "EUR", .(
    country = AreaMapCode,
    date = as.Date(DateTime),
    res = c.resToFactor[ResolutionCode],
    price = `Price[Currency/MWh]`
)]
d.zones = d.zones[d.zones[, .I[res == min(res)], by = .(country, date)]$V1]
d.zones = d.zones[, .(
    mean = mean(price),
    min = min(price),
    max = max(price)
), by = .(country, date)][order(country, date)]
d.zones = removeLastDays(d.zones, 1)

saveToStorages(
    d.zones,
    list(
        id = "electricity-price-entsoe-countries",
        source = "entsoe",
        format = "csv",
        update.time = update.time
    )
)


d.base.f = d.base[AreaCode == "10YAT-APG------L" & Currency == "EUR"]

# max(d.base[AreaCode == "10YAT-APG------L" & Currency == "EUR" & ResolutionCode == "PT60M"]$DateTime)

# unique(d.base[, .(ResolutionCode, AreaCode, AreaTypeCode, AreaName, MapCode)])
# d.t = unique(d.base[AreaTypeCode == "CTY", .(ResolutionCode, AreaCode, AreaName, MapCode)])

# d.base.f = d.base[AreaCode == "10YAT-APG------L" & ResolutionCode == "PT60M" & Currency == "EUR"]
setnames(d.base.f, "Price[Currency/MWh]", "Price")
# unique(d.base.f[, .(ResolutionCode, AreaCode, AreaTypeCode, AreaName, MapCode)])

d.agg = d.base.f[, .(
    mean = mean(Price),
    max = max(Price),
    min = min(Price)
), by = .(
    date = as.Date(DateTime)
)]

# sort(unique(d.base.f$ResolutionCode))

# d.base.f[, factor := c.resToFactor[ResolutionCode]]
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
