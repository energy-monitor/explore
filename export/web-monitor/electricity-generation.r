# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/web-monitor/_shared.r")


# - LOAD -----------------------------------------------------------------------
d.base = loadFromStorage(id = "electricity-generation-g1")[, 
    date := as.Date(date)
]

unique(d.base$source.group)


# - AT -------------------------------------------------------------------------
c.order = d.agg.group[, .(value = sum(value)), by=source.group][order(-value)]$source.group
c.order = c(c.order[c.order != nameOthers], nameOthers)

d.agg.group[, source.group := factor(source.group, c.order, c.order)]
d.agg.group = d.agg.group[order(date, source.group)]

fwrite(d.agg.group, file.path(g$d$wd, "electricity", "generation.csv"))



# GAS

# d.agg = d.base[AreaName == "AT CTY" & ResolutionCode == "PT15M" & ProductionType == "Fossil Gas", .(
# Plot, Preparation
addRollMean(d.agg, 7)
addCum(d.agg)
d.plot = meltAndRemove(d.agg)
dates2PlotDates(d.plot)

# Save
fwrite(d.plot, file.path(g$d$wd, "electricity", "generation-gas.csv"))



# FACETS

# Plot
c.order = d.agg.group[, .(value = sum(value)), by=source.group][order(-value)]$source.group
c.order = c(c.order[c.order != nameOthers], nameOthers)

d.agg.group[, source.group := factor(source.group, c.order, c.order)]
d.plot = d.agg.group[order(date, source.group)]

addRollMean(d.plot, 28, g="source.group")
dates2PlotDates(d.plot)


fwrite(d.plot, file.path(g$d$wd, "electricity", "generation-facets.csv"))
