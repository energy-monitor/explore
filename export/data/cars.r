# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")


# - MONTHLY --------------------------------------------------------------------
# new passenger cars in Austria, Statistik Austria
d.base = loadFromStorage(id = "car-registrations")[,
    date := as.Date(date)
]

d.plot = d.base[, .(date, type, value = cars)]
d.plot[, share := value / value[match("total", type)], by = date]
dates2PlotDates(d.plot)

fwrite(d.plot[year >= min(yearsShown())][order(type, date)], file.path(g$d$wd, "others", "car-registrations.csv"))


# - ANNUAL ---------------------------------------------------------------------
# new passenger cars and stock by country, eurostat
# same layout as `electricity/generation-year-g2.csv` (europe map)
c.files = c(
    road_eqr_carpda = "car-registrations-europe",
    road_eqs_carpda = "car-stock-europe"
)
c.geo2iso = c(EL = "GR", UK = "GB")

for (id in names(c.files)) {
    # countries only, no aggregates
    d.base = loadFromStorage(id = id)[nchar(geo) == 2]
    d.base[geo %in% names(c.geo2iso), geo := c.geo2iso[geo]]

    d.plot = d.base[, .(country = geo, year, type, value = cars)]
    d.plot[, share := value / value[match("total", type)], by = .(country, year)]

    fwrite(d.plot[order(country, year, type)], file.path(g$d$wd, "others", glue("{c.files[[id]]}.csv")))
}
