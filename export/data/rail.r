# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")


# - QUARTERLY ------------------------------------------------------------------
# goods transported by rail in Austria, eurostat (Statistik Austria), in
# million tonnes and billion tonne-km, the date is the first day of the quarter
d.base = loadFromStorage(id = "rail_go_quartal")[geo == "AT"][, date := as.Date(date)]

d.plot = rbind(
    d.base[, .(measure = "tonnes", date, value = ths.t / 1e3)],
    d.base[, .(measure = "tkm", date, value = mio.tkm / 1e3)]
)[!is.na(value)]
dates2PlotDates(d.plot)

fwrite(d.plot[year >= min(yearsShown())][order(measure, date)], file.path(g$d$wd, "mobility", "rail-goods.csv"))


# - ANNUAL ---------------------------------------------------------------------
# passengers transported by rail in Austria (annual only), in millions and
# billion passenger-km, all years
d.base = loadFromStorage(id = "rail_pa_total")[geo == "AT"]

d.plot = rbind(
    d.base[, .(measure = "passengers", year, value = ths.pas / 1e3)],
    d.base[, .(measure = "pkm", year, value = mio.pkm / 1e3)]
)[!is.na(value)]

fwrite(d.plot[order(measure, year)], file.path(g$d$wd, "mobility", "rail-passengers.csv"))
