# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/entsoe/_shared.r")
# loadPackages()

# The generation in Austria by source as an hourly series: the mean power (GW,
# i.e. GWh per hour) of every hour, the hours in UTC (`DateTime`, as the hourly
# prices of `electricity-price-entsoe-hourly`), the hydro power plants by type,
# `consumption` the mean power used by a source, i.e. the pumping of the pumped
# storage (not in the load of ENTSO-E, 6.1.A). Unlike the daily profiles of
# `generation-hourly.r` every day stays, e.g. for the hours of negative prices
# or the market value of the solar power.


# - DOIT -----------------------------------------------------------------------
update.time = now()

# Loaded and filtered by year, the files of all countries at once take too
# much memory, see `generation-hourly.r`
loadYear = function(y) {
    d.base = load_entsoe(
        c.nice2entsoe["generation"],
        from = max(date.start, make_datetime(y, 1, 1)),
        to = min(date.end, make_datetime(y, 12, 1))
    )
    d.base[
        grepl("CTY", AreaTypeCode, fixed = TRUE) & AreaMapCode == "AT" & ResolutionCode == "PT15M", .(
        DateTime = floor_date(`DateTime(UTC)`, "hours"),
        source = ProductionType,
        value = `ActualGenerationOutput[MW]`,
        consumption = fcoalesce(`ActualConsumption[MW]`, 0)
    )][!is.na(value)]
}
d = rbindlist(lapply(year(date.start):year(date.end), loadYear))

# Group, the mean of the quarter hours of the hour, the sum of the sources of
# a group
nameOthers = "others"
addGroupCol(d, c.sourceGroups1Hydro, nameOthers = nameOthers)
d.hour = d[, .(
    value = mean(value), consumption = mean(consumption), n = .N
), by = .(DateTime, source, source.group)][, .(
    value = sum(value) / 10^3,
    consumption = sum(consumption) / 10^3,
    n = max(n)
), by = .(DateTime, source.group)]

# Only complete hours (4 quarter hours) of complete days, the last day is most
# probably incomplete
d.hour = d.hour[n == 4 & DateTime < floor_date(max(DateTime), "day")]
d.hour[, n := NULL]
d.hour = d.hour[order(DateTime, source.group)]


# - STORAGE --------------------------------------------------------------------
saveToStorages(d.hour, list(
    id = "electricity-generation-hourly-series-g1",
    source = "entsoe",
    format = "csv",
    update.time = update.time
))
