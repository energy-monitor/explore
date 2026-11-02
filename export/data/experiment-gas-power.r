# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")


# - LOAD/PREP ------------------------------------------------------------------
# The day ahead price of electricity against the gas price, daily rolling means
# of 7 days as electricity/price and gas/price (generic.r), a line per year in
# the web (experiment/gas-power.json). The 1st of January of every year is also
# the last row of the year before (`link` = 1), so the lines of the years are
# connected, the web leaves these rows out of the hover and the points.
loadDaily = function(id, col) {
    d = loadFromStorage(id = id)[, .(date = as.Date(date), value = get(col))]
    d = d[date >= firstDateShown() - 30, .(value = mean(value, na.rm = TRUE)), by = date]
    d = merge(d, data.table(date = seq(min(d$date), max(d$date), by = "day")), by = "date", all = TRUE)
    d[order(date), rm := rollmean(value, 7, fill = NA, align = "right", na.rm = TRUE)]
    d[, .(date, rm)]
}

d.power = loadDaily("electricity-price-entsoe", "mean")
d.gas = loadDaily("price-gas", "price")

d.plot = merge(d.power[, .(date, value = rm)], d.gas[, .(date, gas = rm)], by = "date")[
    !is.na(value) & !is.na(gas) & date >= firstDateShown()
][, `:=`(year = year(date), link = 0L)]

d.link = d.plot[month(date) == 1 & mday(date) == 1 & year > min(year)][, `:=`(year = year - 1L, link = 1L)]
d.plot = rbind(d.plot, d.link)[order(year, date)]


# - SAVE -----------------------------------------------------------------------
dir.create(file.path(g$d$wd, "experiment"), showWarnings = FALSE)
fwriteWeb(d.plot[, .(date, year, value, gas, link)], file.path(g$d$wd, "experiment", "gas-power.csv"))
