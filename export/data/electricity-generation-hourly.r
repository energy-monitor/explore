# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")


# - LOAD -----------------------------------------------------------------------
d.base = loadFromStorage(id = "electricity-generation-hourly-year-g1")


# Save, the last complete year
c.year = year(Sys.Date()) - 1
d.plot = d.base[year == c.year, .(
    hour, source.group, value
)]
# without the sources of no generation, e.g. coal after 2020 (not in the legend)
d.plot = d.plot[, if (any(value > 0)) .SD, by = source.group]
fwriteWeb(d.plot, file.path(g$d$wd, "electricity", "generation-hourly.csv"))
setSubtitle("electricity/generation-hourly.json", glue("im Jahr {c.year}, nach Quellen, mittlere Leistung in GW"))
