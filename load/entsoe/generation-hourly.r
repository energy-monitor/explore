# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/entsoe/_shared.r")
# loadPackages()


# - DOIT -----------------------------------------------------------------------
update.time = now()
d.base = load_entsoe_data(
    c.nice2entsoe["generation"], from = date.start
)

# d.base[, .(sum = sum(ActualGenerationOutput)), by=.(ProductionType)][order(sum)]

# Filter, Aggregate
# unique(d.base$ProductionType)
d.agg = d.base[AreaMapCode == "AT" & ResolutionCode == "PT15M", .(
    value = mean(`ActualGenerationOutput[MW]`) / 4 / 10^3
), by = .(
    year = year(`DateTime(UTC)`), 
    hour = hour(`DateTime(UTC)`), 
    source = ProductionType
)][order(year, hour)]

d.agg = d.agg[year >= 2019]


# Group
nameOthers = "others"
addGroupCol(d.agg, c.sourceGroups1, nameOthers = nameOthers)
# Agg
d.agg.group = d.agg[, .(value = sum(value)), by = .(year, hour, source.group)]


# Plot
c.order = d.agg.group[, .(
    value = sum(value)
), by=source.group][order(-value)]$source.group
c.order = c(c.order[c.order != nameOthers], nameOthers)

d.agg.group[, source.group := factor(source.group, c.order, c.order)]
d.agg.group = d.agg.group[order(year, hour, source.group)]

# - STORAGE --------------------------------------------------------------------
saveToStorages(d.agg.group, list(
    id = "electricity-generation-hourly-year-g1",
    source = "entsoe",
    format = "csv",
    update.time = update.time
))

