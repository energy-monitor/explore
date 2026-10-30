# Yearly climate indicators of Austria, its states and districts for the
# climate section, from the monthly regional data of the GeoSphere grids
# (`calc/climate-regions.r`):
# - climate/districts.csv: the indicators of all regions by year, for the maps,
#   the anomalies of the maps as indicators of their own
# - climate/regions.csv: the states and Austria as time series, for the line
#   chart with the indicator as radio, the anomaly in the column
#   `value.anomaly`
#
# Only complete years are exported. Anomalies refer to 1961 - 1990, of the
# amounts of water (precipitation, snow water equivalent) in %, of the others
# the difference.

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
c.indicators = c(
    "tm", "rr", "sa", "swe.max",
    "days.hot", "days.summer", "days.frost", "days.ice", "days.snow"
)
c.relative = c("rr", "swe.max")

anomaly = function(v, year, relative) {
    ref = mean(v[year %in% c.reference], na.rm = TRUE)
    if (relative) v / ref * 100 - 100 else v - ref
}

for (i in c.indicators) {
    d.yearly[, paste0(i, ".anomaly") := anomaly(get(i), year, i %in% c.relative), by = region]
}


# - EXPORT ---------------------------------------------------------------------
c.ids = c("level", "region", "name", "year")

d.plot = merge(
    melt(
        d.yearly, id.vars = c.ids, measure.vars = c.indicators,
        variable.name = "indicator", variable.factor = FALSE
    ),
    melt(
        d.yearly, id.vars = c.ids, measure.vars = paste0(c.indicators, ".anomaly"),
        variable.name = "indicator", value.name = "value.anomaly", variable.factor = FALSE
    )[, indicator := sub("\\.anomaly$", "", indicator)],
    by = c(c.ids, "indicator")
)[!is.na(value)]
d.plot[, `:=`(value = round(value, 2), value.anomaly = round(value.anomaly, 2))]

# The anomalies shown on the maps
c.maps = c("tm", "rr", "days.hot", "days.snow")

dir.create(file.path(g$d$wd, "climate"), showWarnings = FALSE)

# Maps: the districts
d.districts = d.plot[level == "district"]
fwriteWeb(
    rbind(
        d.districts[, .(district = region, year, indicator, value)],
        d.districts[indicator %in% c.maps, .(district = region, year, indicator = paste0(indicator, ".anomaly"), value = value.anomaly)]
    )[order(indicator, year, district)],
    file.path(g$d$wd, "climate", "districts.csv")
)

# Time series: the states and Austria
fwriteWeb(
    d.plot[level != "district", .(date = as.Date(sprintf("%d-07-01", year)), type = region, indicator, value, value.anomaly)][order(indicator, type, date)],
    file.path(g$d$wd, "climate", "regions.csv")
)
