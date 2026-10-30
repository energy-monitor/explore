# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")


# - LOAD -----------------------------------------------------------------------
d.gen = loadFromStorage(id = "electricity-generation-hourly-monthly-g1")
d.load = loadFromStorage(id = "electricity-load-hourly-monthly")


# The daily profiles of the meteorological seasons of every year, the months
# weighted by their quarter hours, the winter of a year from the December of
# the year before, e.g. 2026 the winter 2025/26, only complete seasons (of the
# generation), e.g. 2026 without the autumn before December
c.seasons = rep(c("winter", "spring", "summer", "autumn"), each = 3)[c(2:12, 1)]
seasons = function(d, by = character(0)) {
    d = d[, `:=`(period = year + (month == 12), season = c.seasons[month])]
    d = d[, if (uniqueN(month) == 3) .SD, by = .(period, season)]
    d[, .(value = weighted.mean(value, n)), by = c("period", "season", "hour", by)]
}

d.plot = seasons(d.gen, "source.group")
# without the sources of no generation in a season, e.g. coal after 2020
d.plot = d.plot[, if (any(value > 0)) .SD, by = .(period, season, source.group)]
d.plot[, value.share := value / sum(value), by = .(period, season, hour)]

# The total generation and the load as rows of their own (`total` and `load`,
# the charts do not stack them with the sources), the share of the load is the
# one of the generation, e.g. 1.1 if 10% more is used than generated (imports)
d.total = d.plot[, .(total = sum(value)), by = .(period, season, hour)]
d.load = merge(seasons(d.load), d.total, by = c("period", "season", "hour"))[, .(
    period, season, hour, source.group = "load", value, value.share = value / total
)]
d.plot = rbind(
    d.plot,
    d.total[, .(period, season, hour, source.group = "total", value = total, value.share = 1)],
    d.load
)
setcolorder(d.plot, c("period", "season", "hour", "source.group", "value", "value.share"))

fwriteWeb(d.plot, file.path(g$d$wd, "electricity", "generation-hourly-seasons.csv"))

# The charts show the last year with all seasons first, the years are the
# entries of the year of the charts
c.default = d.plot[, .(seasons = uniqueN(season)), by = period][seasons == 4, max(period)]
for (def in c("electricity/generation-hourly-seasons.json", "electricity/generation-hourly-radial.json"))
    setDefText(def, "globals", c.default, key = "period")
