# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/web-monitor/_shared.r")

nameOthers = "others"

# - LOAD -----------------------------------------------------------------------
d.base = loadFromStorage(id = "electricity-generation-g1")[,
    date := as.Date(date)
]

# unique(d.base$source.group)


# - AT -------------------------------------------------------------------------
d.plot = d.base[country == "AT", .(
    value = sum(value)
), by = .(date = {t = as.Date(date); day(t) = 1; t}, source.group)]

c.order = d.plot[, .(value = sum(value)), by=source.group][order(-value)]$source.group
c.order = c(c.order[c.order != nameOthers], nameOthers)

d.plot[, source.group := factor(source.group, c.order, c.order)]
d.plot = d.plot[order(date, source.group)]

fwrite(d.plot, file.path(g$d$wd, "electricity", "generation.csv"))


# - AT GAS ---------------------------------------------------------------------
d.plot = d.base[country == "AT" & source.group == "Gas", .(date, value)]

# Plot, Preparation
addRollMean(d.plot, 7)
addCum(d.plot)
d.plot = meltAndRemove(d.plot)
dates2PlotDates(d.plot)

# Save
fwrite(d.plot, file.path(g$d$wd, "electricity", "generation-gas.csv"))


# - AT FACETS ------------------------------------------------------------------
d.plot = d.base[country == "AT"][, value := value * 1000][]

# Plot
c.order = d.plot[, .(value = sum(value)), by=source.group][order(-value)]$source.group
c.order = c(c.order[c.order != nameOthers], nameOthers)

d.plot[, source.group := factor(source.group, c.order, c.order)]
d.plot = d.plot[order(date, source.group)]

addRollMean(d.plot, 28, g="source.group")
dates2PlotDates(d.plot)

fwrite(d.plot, file.path(g$d$wd, "electricity", "generation-facets.csv"))
