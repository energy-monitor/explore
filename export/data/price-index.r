# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")


# - LOAD/PREP ------------------------------------------------------------------
# Prices of the energy sources as index, the mean of a base year is 100, the
# whole period since 2019 (not only the shown years). Monthly means, the wood
# prices are monthly only. Brent and coal are quoted in $, converted to € with
# the daily exchange rate (price-dollar is $ per €) before the mean, so the
# index is the one of the € price.
d.dollar = loadFromStorage(id = "price-dollar")[, .(date = as.Date(date), usd = price)]

loadDaily = function(id, value = "price") {
    d = loadFromStorage(id = id)[, .(date = as.Date(date), value = get(value))]
    # only months completely covered by the data, the current one is missing
    d[date < floor_date(max(date) + 1, "month")]
}

inEuro = function(d) {
    d = merge(d, d.dollar, by = "date")
    d[, .(date, value = value / usd)]
}

l.sources = list(
    gas = loadDaily("price-gas"),
    oil = inEuro(loadDaily("price-brent")),
    coal = inEuro(loadDaily("price-coal")),
    electricity = loadDaily("electricity-price-entsoe", "mean"),
    eua = loadDaily("price-eua"),
    # the mean of hard and soft firewood
    firewood = loadFromStorage(id = "price-firewood")[, .(date = as.Date(date), value = (hard + soft) / 2)],
    pellets = loadFromStorage(id = "price-pellets")[, .(date = as.Date(date), value = price)]
)

d.base = rbindlist(l.sources, idcol = "type")[!is.na(value)]
d.base = d.base[, .(value = mean(value)), by = .(type, date = floor_date(date, "month"))]


# - INDEX ----------------------------------------------------------------------
# The index for every base year, selectable in the chart (the column `base`).
# The base years are the complete ones since 2019, all sources with all
# months, the shown period starts in 2019 for all of them.
c.firstYear = 2019
d.base = d.base[year(date) >= c.firstYear]

d.months = d.base[, .N, by = .(type, year = year(date))]
c.baseYears = d.months[, .(complete = .N == length(l.sources) && all(N == 12)), by = year][complete == TRUE, sort(year)]
if (!c.firstYear %in% c.baseYears)
    stop(glue("price-index: not all months of {c.firstYear}"))

d.plot = rbindlist(lapply(c.baseYears, function(b) d.base[,
    .(base = b, date, value = 100 * value / mean(value[year(date) == b])),
    by = type
]))

# Save
fwrite(d.plot[order(base, type, date)], file.path(g$d$wd, "economy", "price-index.csv"))
