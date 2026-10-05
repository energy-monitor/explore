# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")


# - MONTHLY --------------------------------------------------------------------
# new passenger cars in Austria, Statistik Austria
d.base = loadFromStorage(id = "car-registrations")[,
    date := as.Date(date)
]

d.plot = d.base[, .(date, type, value = cars)]
d.plot[, value.share := value / value[match("total", type)], by = date]
dates2PlotDates(d.plot)

# the order of the stacked bars: electrified, fossil, others, unknown types are kept
c.types = c("bev", "phev", "hybrid", "petrol", "diesel", "other", "total")
d.plot[, type := factor(type, c(c.types, setdiff(unique(type), c.types)))]

fwrite(d.plot[year >= min(yearsShown())][order(type, date)], file.path(g$d$wd, "mobility", "registrations.csv"))


# - ANNUAL ---------------------------------------------------------------------
# new passenger cars and stock by country, eurostat, in one file with the
# series as column, the layout of `electricity/generation-map.csv` (europe map)
c.series = c(
    road_eqr_carpda = "registrations",
    road_eqs_carpda = "stock"
)

d.plot = rbindlist(lapply(names(c.series), function(id) {
    # countries only, no aggregates
    d.base = loadFromStorage(id = id)[nchar(geo) == 2]
    d.base[, .(series = c.series[[id]], country = iso2(geo), year, type, value = cars)]
}))
d.plot[, share := value / value[match("total", type)], by = .(series, country, year)]

fwrite(d.plot[order(series, country, year, type)], file.path(g$d$wd, "mobility", "cars-map.csv"))
