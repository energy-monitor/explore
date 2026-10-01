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
# new passenger cars and stock by country, eurostat
# same layout as `electricity/generation-map.csv` (europe map)
c.files = c(
    road_eqr_carpda = "cars-map-registrations",
    road_eqs_carpda = "cars-map-stock"
)
c.geo2iso = c(EL = "GR", UK = "GB")

for (id in names(c.files)) {
    # countries only, no aggregates
    d.base = loadFromStorage(id = id)[nchar(geo) == 2]
    d.base[geo %in% names(c.geo2iso), geo := c.geo2iso[geo]]

    d.plot = d.base[, .(country = geo, year, type, value = cars)]
    d.plot[, share := value / value[match("total", type)], by = .(country, year)]

    fwrite(d.plot[order(country, year, type)], file.path(g$d$wd, "mobility", glue("{c.files[[id]]}.csv")))
}
