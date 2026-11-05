# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")

# The day-ahead prices of the bidding zones of Europe by month, for the map of
# web/data/electricity/price-map.json (the zones of web/assets/geo/
# europe-bidding-zones.json, see export/geo/bidding-zones.sh), in EUR/MWh, of
# the daily prices of load/entsoe/price.r (local time of the zones):
# - mean (base), peak (Monday to Friday 8:00-20:00) and offpeak (the other
#   hours, the days weighted by their hours, 12 on a working day, 24 on a
#   weekend)
# - min and max: the lowest and the highest price of an hour (or a quarter
#   hour) of the month
# - negative: the hours with a negative price
# the full months since January 2019, without the current one, Moldova is
# left out (its prices since December 2025 seem wrong, e.g. a mean of 740
# EUR/MWh in its first month).

d.base = loadFromStorage(id = "electricity-price-entsoe-countries")[,
    date := as.Date(date)
]

# not areas of the map: the NordLink cable of NO2 (NO2NSL) and the links of
# Sardinia, Corsica and the mainland (IT-SACOAC, IT-SACODC)
d.base = d.base[!country %in% c("NO2NSL", "IT-SACOAC", "IT-SACODC", "MD")]

# the full months, the last one ends with the data or before the last date
d.base[, month := format(date, "%Y-%m")]
lastFull = if (max(d.base$date) == ceiling_date(max(d.base$date), "month") - days(1))
    format(max(d.base$date), "%Y-%m") else format(floor_date(max(d.base$date), "month") - days(1), "%Y-%m")
months = sort(unique(d.base[month <= lastFull & month >= "2019-01"]$month))

d.base[, offpeak.hours := fifelse(is.na(peak), 24, 12)]
d.plot = d.base[month %in% months, .(
    mean = mean(mean),
    peak = mean(peak, na.rm = TRUE),
    offpeak = weighted.mean(offpeak, offpeak.hours),
    min = min(min),
    max = max(max),
    negative = sum(negative),
    days = .N
), by = .(country, month)]

# only zones with the prices of (almost) every day of the month
d.plot = d.plot[days >= 25][, days := NULL]

fwriteWeb(d.plot[order(month, country)], file.path(g$d$wd, "electricity", "price-map.csv"))
