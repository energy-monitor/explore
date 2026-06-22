# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/entsoe/_shared.r")
# loadPackages()


# - LOAD/PREP ------------------------------------------------------------------
update.time = now()
d.base = load_entsoe(
    c.nice2entsoe["generation"], from = date.start
)

d.base.f = d.base[grep("CTY", AreaTypeCode, fixed = TRUE)]
d.base.f[, factor := c.resToFactor[ResolutionCode]]


d.agg = d.base.f[, .(
    value = sum(`ActualGenerationOutput[MW]`*factor, na.rm = TRUE)/10^6,
    cons = sum(`ActualConsumption[MW]`*factor, na.rm = TRUE)/10^6
), by = .(
    country = AreaMapCode,
    date = as.Date(`DateTime(UTC)`),
    source = ProductionType
)][order(date)]

# Delete last (most probably incomplete) obs
d.agg = removeLastDays(d.agg, 2)


# - STORE ----------------------------------------------------------------------
saveToStorages(d.agg, list(
    id = "electricity-generation",
    source = "entsoe",
    format = "csv",
    update.time = update.time
))

nameOthers = "others"

# - GROUP 1
addGroupCol(d.agg, c.sourceGroups1, nameOthers = nameOthers)
# Agg
d.agg.group = d.agg[, .(
    value = sum(value),
    cons = sum(cons)
), by = .(country, date, source.group)]
# Store
saveToStorages(d.agg.group, list(
    id = "electricity-generation-g1",
    source = "entsoe",
    format = "csv",
    update.time = update.time
))

# - GROUP 2
addGroupCol(d.agg, c.sourceGroups2, nameOthers = nameOthers)
# Agg
d.agg.group = d.agg[, .(
    value = sum(value),
    cons = sum(cons)
), by = .(country, date, source.group)]
# Store
saveToStorages(d.agg.group, list(
    id = "electricity-generation-g2",
    source = "entsoe",
    format = "csv",
    update.time = update.time
))

d.base.f = d.base[grep("CTY", AreaTypeCode, fixed = TRUE)]
d.base.f[, hour := floor_date(`DateTime(UTC)`, unit = "hours")]


d.agg = d.base.f[, .(
    value = mean(`ActualGenerationOutput[MW]`, na.rm = TRUE),
    cons = mean(`ActualConsumption[MW]`, na.rm = TRUE)
), by = .(
    country = AreaMapCode,
    source = ProductionType,
    DateTime = hour
)][order(DateTime)]


# - STORE ----------------------------------------------------------------------
saveToStorages(
    d.agg,
    list(
        id = "electricity-generation-hourly",
        source = "entsoe",
        format = "csv",
        update.time = update.time
    )
)
