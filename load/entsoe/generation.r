# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/entsoe/_shared.r")
# loadPackages()

library(lubridate)


# - LOAD/PREP ------------------------------------------------------------------
update.time = now()

d.base = loadEntsoeComb(
    # type = "generation", month.start = "2022-07", month.end = "2022-07", check.updates = FALSE
    type = "generation", month.start = "2014-12", month.end = month.end

)

# unique(d.base$ProductionType)

d.base.f = d.base[AreaTypeCode == "CTY"]
d.base.f[,
         hour := floor_date(DateTime, unit = "hours")]
d.base.f[,
         factor := resToFactor[ResolutionCode]]


# d.base.f[, .(sum = sum(ActualGenerationOutput)), by=.(ProductionType)][order(sum)]
# unique(d.base.f[,. (ResolutionCode, AreaCode, AreaTypeCode, AreaName, MapCode)])


d.agg.hours = d.base.f[, .(
    value = mean(ActualGenerationOutput, na.rm = TRUE),
    cons = mean(ActualConsumption, na.rm = TRUE)
), by = .(
    country = MapCode,
    source = ProductionType,
    DateTime = hour
)][order(DateTime)]

# - AGG -----------------------------------------------------------------------
d.agg = d.agg.hours[, .(
    value = sum(value) / 10^6,
    cons = sum(cons) / 10^6
), by = .(
    country = country,
    date = as.Date(DateTime),
    source = source
)][order(date)]

# Delete last (most probably incomplete) obs
d.agg = removeLastDays(d.agg, 2)

# - STORE ----------------------------------------------------------------------
saveToStorages(d.agg.hours, list(
    id = "electricity-generation-hourly",
    source = "entsoe",
    format = "csv",
    update.time = update.time
))

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
