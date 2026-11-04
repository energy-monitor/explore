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
        format = c("csv", "parquet"),
        update.time = update.time
    )
)


# The daily prices of all bidding zones (AreaMapCode, e.g. AT, DE_LU, IT-NORTH)
# in EUR, the days and hours in the local time of the zone, of the finest
# resolution of the day, e.g. the quarter hours since the 15 minute products of
# October 2025 when a zone has both:
# - mean (base), min and max: of all hours of the day
# - peak: the mean of Monday to Friday 8:00-20:00 (as the peak products of the
#   exchanges, holidays included), NA on weekends, offpeak: the mean of the
#   other hours (a monthly mean weights the days by their hours, 12 on a
#   working day, 24 on a weekend)
# - negative: the hours with a negative price (quarter hours as a quarter)
# the zones are in CET/CEST, but the ones of the time zones below
c.zoneTz = c(
    PT = "Europe/Lisbon", IE_SEM = "Europe/Dublin",
    FI = "Europe/Helsinki", EE = "Europe/Tallinn", LV = "Europe/Riga", LT = "Europe/Vilnius",
    GR = "Europe/Athens", RO = "Europe/Bucharest", BG = "Europe/Sofia", MD = "Europe/Chisinau"
)
d.zones = d.base[Currency == "EUR", .(
    country = AreaMapCode,
    DateTime,
    res = c.resToFactor[ResolutionCode],
    price = `Price[Currency/MWh]`
)]
d.zones[, tz := fifelse(country %in% names(c.zoneTz), c.zoneTz[country], "Europe/Vienna")]
d.zones[, c("date", "hour", "wday") := {
    lt = as.POSIXlt(DateTime, tz = tz[1])
    list(as.Date(format(lt, "%Y-%m-%d")), lt$hour, lt$wday)
}, by = tz]
d.zones[, peak := wday %in% 1:5 & hour >= 8 & hour < 20]
d.zones = d.zones[d.zones[, .I[res == min(res)], by = .(country, date)]$V1]
d.zones = d.zones[, .(
    mean = mean(price),
    min = min(price),
    max = max(price),
    peak = if (any(peak)) mean(price[peak]) else NA_real_,
    offpeak = mean(price[!peak]),
    negative = sum(res[price < 0])
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
