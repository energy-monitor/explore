# Precipitation and snow water equivalent of Austria as indicators of the
# hydro power generation, see `load/geosphere/precipitation-snow.r`:
# - precipitation: sum of the last 90 days in mm (a single month says little,
#   the accumulated precipitation of the last two to three months tracks the
#   run-of-river generation)
# - snow: water stored in the snow cover in mm, i.e. the melt water still to
#   come in spring and summer

# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")

sum.length = 90

# Reference periods, the shown years are kept as own lines additionally
# (the latest period overlaps with them)
c.periods = list("avg60-90" = 1961:1990, "avg90-20" = 1991:2020)


# - LOAD/PREP ------------------------------------------------------------------
d.base = loadFromStorage(id = "precipitation-snow")[, date := as.Date(date)]

d.precipitation = d.base[, .(
    date, value = rollsum(precipitation, sum.length, fill = NA, align = "right")
)]
d.snow = d.base[, .(date, value = swe)]


# - PLOT -----------------------------------------------------------------------
yearsOverlay = function(d) {
    d.plot = rbind(
        rbindlist(lapply(names(c.periods), function(p) d[year(date) %in% c.periods[[p]]][, year := p])),
        d[year(date) %in% yearsShown()][, year := as.character(year(date))]
    )

    d.plot = d.plot[!is.na(value), .(
        value = mean(value)
    ), by = .(
        year, date20 = {
            t = copy(date)
            year(t) = 2020
            t
        }
    )]

    # Feb 29 of the averages only contains leap years, interpolate it from its neighbours instead
    d.plot[startsWith(year, "avg"), value := ifelse(
        date20 == as.Date("2020-02-29"),
        (value[date20 == as.Date("2020-02-28")] + value[date20 == as.Date("2020-03-01")]) / 2,
        value
    ), by = year]

    d.plot[order(year, date20)]
}

fwriteWeb(yearsOverlay(d.precipitation), file.path(g$d$wd, "weather", "precipitation.csv"))
fwriteWeb(yearsOverlay(d.snow), file.path(g$d$wd, "weather", "snow.csv"))
