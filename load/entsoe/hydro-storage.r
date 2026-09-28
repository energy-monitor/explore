# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/entsoe/_shared.r")


# - DOIT -----------------------------------------------------------------------
update.time = now()
# yearly files, `load_entsoe_data_wa` only handles monthly ones
# (there is no DateTime column, the package warns and returns all rows)
d.base = load_entsoe_data(
    c.nice2entsoe["hydroStorage"], from = as.POSIXct("2014-01-01", tz = "UTC")
)

d.base.f = d.base[grep("CTY", AreaTypeCode, fixed = TRUE)]

# weekly values, date is the monday of the ISO week
d.base.f[, jan4 := as.Date(paste0(Year, "-01-04"))]
d.base.f[, date := jan4 - (wday(jan4, week_start = 1) - 1) + 7 * (Week - 1)]

# keep the latest update, in case a week shows up in two yearly files
d.agg = unique(
    d.base.f[order(-`UpdateTime(UTC)`)], by = c("AreaMapCode", "date")
)[, .(
    country = AreaMapCode,
    date,
    # in TWh
    value = `StoredEnergy[MWh]` / 10^6
)][order(country, date)]


# - STORE ----------------------------------------------------------------------
saveToStorages(d.agg, list(
    id = "electricity-hydro-storage",
    source = "entsoe",
    format = "csv",
    update.time = update.time
))
