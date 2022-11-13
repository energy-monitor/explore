# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/web-monitor/_shared.r")


# - LOAD/PREP ------------------------------------------------------------------
# d.plot = loadFromStorage(id = "temperature-hdd")[, .(
#     date = as.Date(date), value = hdd
# )]
#
# addRollMean(d.plot, 28)
# addCum(d.plot)
#
# d.plot = meltAndRemove(d.plot)
#
#
# d.plot[variable == "rm28", variable := "rm"]
# d.plot = d.plot[variable %in% c("rm", "cum")]
#
# dates2PlotDates(d.plot)
# year(d.plot$date20[d.plot$date20 > "2020-09-01"]) = 2019
#
# d.plot(date20 >)

fwrite(d.plot, file.path(g$d$wd, "others", "hdd.csv"))
