# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/entsoe/_shared.r")
#loadPackages(ggplot2)

# The daily profiles of the load in Austria: the mean power (GW, i.e. GWh per
# hour) of each hour of the day, by year and by month, as the ones of the
# generation (load/entsoe/generation-hourly.r): the hours are local time
# (Europe/Vienna), `n` is the number of quarter hours, e.g. to weight the
# months of a season.


# - DOIT -----------------------------------------------------------------------
update.time = now()

# Loaded and filtered by year (the months of a year in UTC), as the generation
loadYear = function(y) {
    d.base = load_entsoe(
        c.nice2entsoe["load"],
        from = max(date.start, make_datetime(y, 1, 1)),
        to = min(date.end, make_datetime(y, 12, 1))
    )
    # sort(unique(d.base.f$ResolutionCode))
    d.base[grepl("CTY", AreaTypeCode, fixed = TRUE) & AreaMapCode == "AT", .(
        time = with_tz(`DateTime(UTC)`, "Europe/Vienna"),
        value = `TotalLoad[MW]`
    )][!is.na(value)]
}
d = rbindlist(lapply(year(date.start):year(date.end), loadYear))

d[, `:=`(year = year(time), month = month(time), hour = hour(time))]

# Only complete months, the last one is most probably incomplete
d = d[time < floor_date(max(time), "month")]

# Mean power of the hour (GW) by month, from December 2018, the first month of
# the data, e.g. of the winter 2018/19
d.month = d[, .(
    value = mean(value) / 10^3,
    n = .N
), by = .(year, month, hour)][order(year, month, hour)]

# By year, the months weighted by their quarter hours
d.year = d.month[year >= 2019, .(
    value = weighted.mean(value, n),
    n = sum(n)
), by = .(year, hour)][order(year, hour)]


# - STORAGE --------------------------------------------------------------------
saveToStorages(d.year, list(
    id = "electricity-load-hourly-year",
    source = "entsoe",
    format = "csv",
    update.time = update.time
))

saveToStorages(d.month, list(
    id = "electricity-load-hourly-monthly",
    source = "entsoe",
    format = "csv",
    update.time = update.time
))
