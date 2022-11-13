# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/web-monitor/_shared.r")


# - LOAD/PREP ------------------------------------------------------------------
d.plot = loadFromStorage(id = "temperature-hdd")[, .(
    date = as.Date(date), value = hdd
)]

addRollMean(d.plot, 28)
addCum(d.plot)

d.plot = meltAndRemove(d.plot)
dates2PlotDates(d.plot)

fwrite(d.plot, file.path(g$d$wd, "others", "hdd.csv"))
