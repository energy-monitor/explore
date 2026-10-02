# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")

# Index of the traffic, chained from month to month: the change to the previous
# month with values is the one of the sum over the stations with values in both
# months, so that new stations and failures do not shift the level. Scaled to
# the average of the base year = 100.
chainIndex = function(d, base.year) {
    d = d[order(date)]
    dates = sort(unique(d$date))
    d.index = data.table(date = dates, value = NA_real_, stations = 0L)
    i.last = NA_integer_
    for (i in seq_along(dates)) {
        x = d[date == dates[i]]
        d.index$stations[i] = nrow(x)
        if (nrow(x) == 0) next
        if (is.na(i.last)) {
            d.index$value[i] = 1
        } else {
            m = merge(x, d[date == dates[i.last]], by = "station.id")
            if (nrow(m) == 0) next
            d.index$value[i] = d.index$value[i.last] * sum(m$value.x) / sum(m$value.y)
        }
        i.last = i
    }
    d.index[, value := 100 * value / mean(value[year(date) == base.year], na.rm = TRUE)]
}


# - LOAD/PREP ------------------------------------------------------------------
# average daily traffic per month (Mo-Su), both directions
d.base = loadFromStorage(id = "traffic-asfinag")[
    direction == "total" & !is.na(dtv.ms),
    .(date = as.Date(date), road, station.id, vehicle, value = dtv.ms)
]

# base year is the last complete year, the one all roads (incl. the newest)
# have values for, roads without values in it have no index
c.base.year = d.base[, .(months = uniqueN(month(date))), by = .(year = year(date))][months == 12, max(year)]

# all roads (total) and per road
d.plot = rbind(
    d.base[, chainIndex(.SD, c.base.year), by = vehicle][, road := "total"],
    d.base[, chainIndex(.SD, c.base.year), by = .(road, vehicle)]
)[!is.na(value)]

c.roads = c("total", sort(unique(d.base$road)))
d.plot[, road := factor(road, c.roads)]
d.plot[, vehicle := factor(vehicle, c("all", "heavy", "light"))]
dates2PlotDates(d.plot)

fwrite(d.plot[year >= min(yearsShown())][order(road, vehicle, date), .(
    road, vehicle, date, value, stations, year, date20
)], file.path(g$d$wd, "mobility", "traffic.csv"))
