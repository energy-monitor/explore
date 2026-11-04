# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")

# The day-ahead prices of the bidding zones of Europe by month, for the map of
# web/data/electricity/price-map.json (the zones of web/assets/geo/
# europe-bidding-zones.json, see export/geo/bidding-zones.sh): the mean of the
# daily means, the lowest and the highest price of a day of the month (of
# load/entsoe/price.r, the finest resolution of a day), in EUR/MWh, the full
# months of the last 13, without the current one.

d.base = loadFromStorage(id = "electricity-price-entsoe-countries")[,
    date := as.Date(date)
]

# not areas of the map: the NordLink cable of NO2 (NO2NSL) and the links of
# Sardinia, Corsica and the mainland (IT-SACOAC, IT-SACODC)
d.base = d.base[!country %in% c("NO2NSL", "IT-SACOAC", "IT-SACODC")]

# the full months, the last one ends with the data or before the last date
d.base[, month := format(date, "%Y-%m")]
lastFull = if (max(d.base$date) == ceiling_date(max(d.base$date), "month") - days(1))
    format(max(d.base$date), "%Y-%m") else format(floor_date(max(d.base$date), "month") - days(1), "%Y-%m")
months = sort(unique(d.base[month <= lastFull]$month))
months = tail(months, 13)

d.plot = d.base[month %in% months, .(
    mean = mean(mean),
    min = min(min),
    max = max(max),
    days = .N
), by = .(country, month)]

# only zones with the prices of (almost) every day of the month
d.plot = d.plot[days >= 25][, days := NULL]

fwriteWeb(d.plot[order(month, country)], file.path(g$d$wd, "electricity", "price-map.csv"))
