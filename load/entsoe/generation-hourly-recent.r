# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/entsoe/_shared.r")
# loadPackages()


# - DOIT -----------------------------------------------------------------------
update.time = now()
d.base = load_entsoe_data(
    c.nice2entsoe["generation"], from = date.start
)



fromDate = today() %m-% months(1)
# d.base[, .(sum = sum(ActualGenerationOutput)), by=.(ProductionType)][order(sum)]

# Filter, Aggregate
# unique(d.base$ProductionType)
d.agg = d.base[AreaName == "AT CTY" & ResolutionCode == "PT15M" & DateTime >= fromDate, .(
    value = mean(ActualGenerationOutput) / 4 / 10^3
), by = .(
    hour = hour(DateTime),
    date = as.Date(DateTime),
    source = ProductionType
)][order(date, hour)]



# Group
nameOthers = "others"
addGroupCol(d.agg, c.sourceGroups1, nameOthers = nameOthers)
# Agg
d.agg.group = d.agg[, .(value = sum(value)), by = .(date, hour, source.group)]


# Plot
c.order = d.agg.group[, .(
    value = sum(value)
), by=source.group][order(-value)]$source.group
c.order = c(c.order[c.order != nameOthers], nameOthers)

d.agg.group[, source.group := factor(source.group, c.order, c.order)]
d.agg.group = d.agg.group[order(date, hour, source.group)]

# - STORAGE --------------------------------------------------------------------
saveToStorages(d.agg.group, list(
    id = "electricity-generation-hourly-recent-g1",
    source = "entsoe",
    format = "csv",
    update.time = update.time
))
