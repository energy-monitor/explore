# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/web-monitor/_shared.r")


# - LOAD -----------------------------------------------------------------------
d.base = loadFromStorage(id = "electricity-generation-hourly-year-g1")


# Save
d.plot = d.base[year == 2021, .(
    hour, source.group, value
)]
fwrite(d.plot, file.path(g$d$wd, "electricity", "generation-hourly.csv"))
