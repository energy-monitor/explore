# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/entsoe/_shared.r")
# loadPackages()


# - LOAD/PREP ------------------------------------------------------------------
update.time = now()

d.base = load_entsoe_data(
    c.nice2entsoe["generation"], from = date.start
)

d.base.f = d.base[AreaTypeCode == "CTY"]
d.base.f[, hour := floor_date(DateTime, unit = "hours")]


d.agg = d.base.f[, .(
    value = mean(ActualGenerationOutput, na.rm = TRUE),
    cons = mean(ActualConsumption, na.rm = TRUE)
), by = .(
    country = MapCode,
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

