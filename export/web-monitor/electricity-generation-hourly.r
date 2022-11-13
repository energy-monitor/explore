# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/web-monitor/_shared.r")


# - LOAD -----------------------------------------------------------------------
d.base = loadFromStorage(id = "electricity-generation-hourly-year-g1")


# Save
fwrite(d.base, file.path(g$d$wd, "electricity", "generation-hourly.csv"))
