# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")

mean.length = 28


# - LOAD/PREP ------------------------------------------------------------------
d.plot = loadFromStorage(id = "temperature-hdd")[, .(
    date = as.Date(date),
    value = temp
)]

addRollMean(d.plot, mean.length)

# Reference periods, the shown years are kept as own lines additionally
# (the latest period overlaps with them)
c.periods = list("avg40-60" = 1940:1960, "avg60-90" = 1961:1990, "avg90-20" = 1991:2020)
d.plot = rbind(
    rbindlist(lapply(names(c.periods), function(p) d.plot[year(date) %in% c.periods[[p]]][, year := p])),
    d.plot[year(date) %in% yearsShown()][, year := as.character(year(date))]
)

d.plot = d.plot[, .(
    value = mean(get(glue("rm{mean.length}")), na.rm = TRUE)
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

fwrite(d.plot[order(year, date20)], file.path(g$d$wd, "weather", "temperature.csv"))
