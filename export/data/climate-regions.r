# Yearly climate indicators of Austria, its states and districts for the
# climate section, from the monthly regional data of the GeoSphere grids
# (`calc/climate-regions.r`):
# - climate/districts.csv: the indicators of all regions by year, for the maps
# - climate/regions.csv: the states and Austria as time series, for the line
#   chart with the indicator as radio
#
# Only complete years are exported. Anomalies refer to 1961 - 1990, of the
# precipitation in %, of the days in days.

# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")

c.reference = 1961:1990


# - LOAD -----------------------------------------------------------------------
d.monthly = loadFromStorage(id = "climate-regions-monthly")
d.monthly[, region := as.character(region)]


# - YEARLY ---------------------------------------------------------------------
d.yearly = d.monthly[, .(
    days = sum(days),
    tm = mean(tm), tn = mean(tn), tx = mean(tx),
    rr = sum(rr),
    sa = sum(sa),
    swe.max = max(swe.max),
    days.hot = sum(days.hot), days.summer = sum(days.summer),
    days.frost = sum(days.frost), days.ice = sum(days.ice), days.snow = sum(days.snow)
), by = .(level, region, name, year)]

# Complete years only (a test with partial data can be forced)
if (Sys.getenv("CLIMATE_ALLOW_PARTIAL") == "") {
    d.yearly = d.yearly[days >= 365]
}

# Anomalies
d.yearly[, `:=`(
    tm.anomaly = tm - mean(tm[year %in% c.reference]),
    rr.anomaly = rr / mean(rr[year %in% c.reference]) * 100 - 100,
    days.hot.anomaly = days.hot - mean(days.hot[year %in% c.reference], na.rm = TRUE),
    days.snow.anomaly = days.snow - mean(days.snow[year %in% c.reference], na.rm = TRUE)
), by = region]


# - EXPORT ---------------------------------------------------------------------
c.indicators = c(
    "tm", "tm.anomaly", "rr", "rr.anomaly", "sa", "swe.max",
    "days.hot", "days.hot.anomaly", "days.summer", "days.frost", "days.ice",
    "days.snow", "days.snow.anomaly"
)

d.plot = melt(
    d.yearly, id.vars = c("level", "region", "name", "year"), measure.vars = c.indicators,
    variable.name = "indicator"
)[!is.na(value)]
d.plot[, value := round(value, 2)]

dir.create(file.path(g$d$wd, "climate"), showWarnings = FALSE)

# Maps: the districts
fwrite(
    d.plot[level == "district", .(district = region, year, indicator, value)][order(indicator, year, district)],
    file.path(g$d$wd, "climate", "districts.csv")
)

# Time series: the states and Austria
fwrite(
    d.plot[level != "district", .(date = as.Date(sprintf("%d-07-01", year)), type = region, indicator, value)][order(indicator, type, date)],
    file.path(g$d$wd, "climate", "regions.csv")
)
