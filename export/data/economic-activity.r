# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")


# - LOAD/PREP ------------------------------------------------------------------
d.base = loadFromStorage(id = "economic-activity")

d.plot = d.base[year >= 2019, .(
    date = as.Date(paste(year, month, 1, sep = "-")), value
)]

dates2PlotDates(d.plot)

fwrite(d.plot[order(year, date20)], file.path(g$d$wd, "others", "economic-activity.csv"))
