# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/web-monitor/_shared.r")

nameOthers = "others"

# - LOAD -----------------------------------------------------------------------
d.base.g1 = loadFromStorage(id = "electricity-generation-g1")[,
    date := as.Date(date)
][date < "2022-11-11"]

d.base.g2 = loadFromStorage(id = "electricity-generation-g2")[,
    date := as.Date(date)
][date < "2022-11-11"]
# unique(d.base$source.group)


# - AT -------------------------------------------------------------------------
d.plot = d.base.g1[date < "2022-11-01" & country == "AT", .(
    value = sum(value)
), by = .(date = {t = as.Date(date); day(t) = 1; t}, source.group)]

c.order = d.plot[, .(value = sum(value)), by=source.group][order(-value)]$source.group
c.order = c(c.order[c.order != nameOthers], nameOthers)

d.plot[, source.group := factor(source.group, c.order, c.order)]
d.plot = d.plot[order(date, source.group)]

fwrite(d.plot, file.path(g$d$wd, "electricity", "generation-monthly-g1.csv"))



# - AT GAS ---------------------------------------------------------------------
d.plot = d.base.g1[country == "AT" & source.group == "Gas", .(date, value)]

# Plot, Preparation
addRollMean(d.plot, 7)
addCum(d.plot)
d.plot = meltAndRemove(d.plot)
dates2PlotDates(d.plot)

# Save
fwrite(d.plot, file.path(g$d$wd, "electricity", "generation-gas.csv"))


# - AT G1 ----------------------------------------------------------------------
d.plot = d.base.g1[country == "AT"][, value := value * 1000][]

# Plot
c.order = d.plot[, .(value = sum(value)), by=source.group][order(-value)]$source.group
c.order = c(c.order[c.order != nameOthers], nameOthers)

d.plot[, source.group := factor(source.group, c.order, c.order)]
d.plot = d.plot[order(date, source.group)]

addRollMean(d.plot, 28, g = "source.group")
dates2PlotDates(d.plot)

fwrite(d.plot, file.path(g$d$wd, "electricity", "generation-g1.csv"))


# - AT G2 ----------------------------------------------------------------------
d.plot = d.base.g2[country == "AT"][, value := value * 1000][]

# Plot
c.order = d.plot[, .(value = sum(value)), by=source.group][order(-value)]$source.group
c.order = c(c.order[c.order != nameOthers], nameOthers)

d.plot[, source.group := factor(source.group, c.order, c.order)]
d.plot = d.plot[order(date, source.group)]

addRollMean(d.plot, 28, g = "source.group")
dates2PlotDates(d.plot)

fwrite(d.plot, file.path(g$d$wd, "electricity", "generation-g2.csv"))


# MAP INT
d.agg = d.base.g2[, .(value = sum(value)), by=.(country, year = year(date), type = source.group)]

d.agg[, share := value/sum(value), by=.(country, year)]

d.plot = d.agg[year >= 2019 & !is.na(share)][order(country, year)]

fwrite(d.plot, file.path(g$d$wd, "electricity", "generation-year-g2.csv"))
