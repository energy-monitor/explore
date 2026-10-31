# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/entsoe/_shared.r")
# loadPackages()

# The daily profiles of the generation in Austria by source: the mean power
# (GW, i.e. GWh per hour) of each hour of the day, by year and by month. The
# hours are local time (Europe/Vienna), e.g. hour 12 is 12:00-13:00 in summer
# and in winter. `n` is the number of quarter hours of a source, e.g. to weight
# the months of a season. The hydro power plants by type, `consumption` the
# mean power used by a source, i.e. the pumping of the pumped storage (not in
# the load of ENTSO-E, 6.1.A).


# - DOIT -----------------------------------------------------------------------
update.time = now()

# Loaded and filtered by year (the months of a year in UTC), the files of all
# months of all countries at once take more than 8 GB of memory
loadYear = function(y) {
    d.base = load_entsoe(
        c.nice2entsoe["generation"],
        from = max(date.start, make_datetime(y, 1, 1)),
        to = min(date.end, make_datetime(y, 12, 1))
    )
    # d.base[, .(sum = sum(ActualGenerationOutput)), by=.(ProductionType)][order(sum)]
    # unique(d.base$ProductionType)
    d.base[
        grepl("CTY", AreaTypeCode, fixed = TRUE) & AreaMapCode == "AT" & ResolutionCode == "PT15M", .(
        time = with_tz(`DateTime(UTC)`, "Europe/Vienna"),
        source = ProductionType,
        value = `ActualGenerationOutput[MW]`,
        consumption = fcoalesce(`ActualConsumption[MW]`, 0)
    )][!is.na(value)]
}
d = rbindlist(lapply(year(date.start):year(date.end), loadYear))

d[, `:=`(year = year(time), month = month(time), hour = hour(time))]

# Only complete months, the last one is most probably incomplete
d = d[time < floor_date(max(time), "month")]


# Mean power of the hour (GW) by month and source, from December 2018, the
# first month of the data, e.g. of the winter 2018/19
d.month = d[, .(
    value = mean(value) / 10^3,
    consumption = mean(consumption) / 10^3,
    n = .N
), by = .(year, month, hour, source)]

# Group
nameOthers = "others"
addGroupCol(d.month, c.sourceGroups1Hydro, nameOthers = nameOthers)
d.month = d.month[, .(
    value = sum(value), consumption = sum(consumption), n = max(n)
), by = .(year, month, hour, source.group)]

# Order of the sources: the largest first, the hydro power plants together (by
# the first word of the group), others last
d.order = d.month[, .(value = sum(value * n)), by = source.group]
d.order[, family := sub(" .*", "", source.group)]
d.order[, value.family := sum(value), by = family]
c.order = d.order[order(-value.family, -value)]$source.group
c.order = c(c.order[c.order != nameOthers], nameOthers)
d.month[, source.group := factor(source.group, c.order, c.order)]
d.month = d.month[order(year, month, hour, source.group)]

# By year, the months weighted by their quarter hours
d.year = d.month[year >= 2019, .(
    value = weighted.mean(value, n),
    consumption = weighted.mean(consumption, n),
    n = sum(n)
), by = .(year, hour, source.group)][order(year, hour, source.group)]


# - STORAGE --------------------------------------------------------------------
saveToStorages(d.year, list(
    id = "electricity-generation-hourly-year-g1",
    source = "entsoe",
    format = "csv",
    update.time = update.time
))

saveToStorages(d.month, list(
    id = "electricity-generation-hourly-monthly-g1",
    source = "entsoe",
    format = "csv",
    update.time = update.time
))
