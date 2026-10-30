# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")

start.year = 2014

# Estimates and inventory values, see calc/emissions.r
d.emissions = loadFromStorage(id = "emissions-fuels")[, date := as.Date(date)]
d.oil.products = loadFromStorage(id = "emissions-oil-products")[, date := as.Date(date)]
d.nid = loadFromStorage(id = "emissions-nid")

# Monthly values and cumulated values since the start of the year, each year
# on the axis of 2020, the years 2014-18 averaged; by optional groups (g)
yearProfile = function(d, g = character(0)) {
    d = copy(d)[, year := ifelse(year(date) %in% 2014:2018, "avg14-18", year(date))]
    d = d[year == "avg14-18" | year %in% yearsShown()]

    d = d[, .(
        value = mean(value, na.rm = TRUE)
    ), by = c("year", g, "date20")][order(year, date20)]

    rbind(
        d[, c("year", g, "date20", "value"), with = FALSE][, variable := "month"],
        d[, .(date20, value = cumsum(value)), by = c("year", g)][, variable := "cum"],
        use.names = TRUE
    )[, c("year", g, "variable", "date20", "value"), with = FALSE]
}


# - TOTAL ----------------------------------------------------------------------
# Monthly emissions by fuel, only months with data for all fuels
c.fuels = c("gas", "oil", "coal")
d.fuels = d.emissions[fuel %in% c.fuels]
d.fuels = d.fuels[, if (.N == length(c.fuels)) .SD, by = date]

d.plot = d.fuels[year(date) >= start.year, .(date, fuel, value = kt.co2 / 1000)]
fwriteWeb(d.plot[order(date, fuel)], file.path(g$d$wd, "fossil", "emissions-total.csv"))

# Sum of all fuels, by year
d.plot = d.fuels[, .(value = sum(kt.co2) / 1000), by = date]
dates2PlotDates(d.plot)
fwriteWeb(yearProfile(d.plot), file.path(g$d$wd, "fossil", "emissions-total-year.csv"))

# Gas and coal, by year
for (f in c("gas", "coal")) {
    d.plot = d.emissions[fuel == f & year(date) >= start.year, .(date, value = kt.co2 / 1000)]
    dates2PlotDates(d.plot)
    fwriteWeb(yearProfile(d.plot), file.path(g$d$wd, "fossil", glue("emissions-{f}-year.csv")))
}


# - OIL PRODUCTS ---------------------------------------------------------------
# Rolling 12 months mean by product group
c.groups = c(
    gasoil = "gasoil", gasoline = "gasoline",
    kerosene = "others", lpg = "others", fueloil = "others",
    correction = "correction"
)
d.oil = d.oil.products[product %in% names(c.groups)]
d.oil = d.oil[, .(kt.co2 = sum(kt.co2)), by = .(date, product = c.groups[product])][order(product, date)]
d.oil[, value := frollmean(kt.co2, 12), by = product]

d.plot = d.oil[year(date) >= start.year & !is.na(value), .(date, product, value)]
fwriteWeb(d.plot[order(date, product)], file.path(g$d$wd, "fossil", "emissions-oil.csv"))

# Oil in total and by product group, by year (without the constant correction,
# which is part of the total only)
d.plot = rbind(
    d.oil[, .(product = "total", value = sum(kt.co2) / 1000), by = date],
    d.oil[product != "correction", .(date, product, value = kt.co2 / 1000)]
)[year(date) >= start.year]
dates2PlotDates(d.plot)
d.plot = yearProfile(d.plot, "product")
d.plot[, product := factor(product, levels = c("total", "gasoil", "gasoline", "others"))]
# facets support one dimension only, so product and variable are combined
d.plot = d.plot[order(product, variable != "month")][, panel := paste(product, variable, sep = "-")]
fwriteWeb(d.plot, file.path(g$d$wd, "fossil", "emissions-oil-year.csv"))


# - INTERNATIONAL AVIATION -----------------------------------------------------
d.plot = d.emissions[fuel == "intaviation", .(date, value = kt.co2)]

# eurostat reports international aviation from 2014 on
d.plot[, year := ifelse(year(date) %in% 2014:2018, "avg14-18", year(date))]
d.plot = d.plot[year == "avg14-18" | year %in% yearsShown()]

d.plot = d.plot[, .(
    value = mean(value, na.rm = TRUE)
), by = .(
    year, date20 = {
        t = copy(date)
        year(t) = 2020
        t
    }
)]

fwriteWeb(d.plot[order(year, date20)], file.path(g$d$wd, "fossil", "emissions-aviation.csv"))


# - COMPARISON WITH THE NID ----------------------------------------------------
# Yearly estimates vs the national inventory, for the years available in both
d.own = d.emissions[, .(value = sum(kt.co2), n = .N), by = .(fuel, year = year(date))][n == 12, !"n"]

d.plot = merge(d.own, d.nid[, .(fuel, year, value = kt.co2)], by = c("fuel", "year"), suffixes = c(".own", ".nid"))[year >= start.year]

# Total of gas, oil and coal, only for years with all three
c.fuels = c("gas", "oil", "coal")
d.plot = rbind(d.plot, d.plot[fuel %in% c.fuels, if (.N == length(c.fuels)) .(
    fuel = "total", value.own = sum(value.own), value.nid = sum(value.nid)
), by = year])

# Relative deviation of the own estimate from the NID
d.plot[, `:=`(
    date = as.Date(paste0(year, "-01-01")),
    deviation = value.own / value.nid - 1
)]
d.plot[, sign := fifelse(deviation >= 0, "higher", "lower")]

# Order of the facets
d.plot[, fuel := factor(fuel, levels = c("total", "gas", "oil", "coal", "intaviation"))]
fwriteWeb(d.plot[order(fuel, year), .(date, year, fuel, sign, value = deviation, own = value.own / 1000, nid = value.nid / 1000)], file.path(g$d$wd, "fossil", "emissions-nid.csv"))


# - LONG TERM ------------------------------------------------------------------
# Inventory since 1990 and the estimates as rolling sums of 12 months in Mt,
# the sum of December is the one of the calendar year, so the yearly values
# of the inventory are dated to December. The inventory of coal (reference
# approach) only has some years, the total only the years of all three.
c.facets = c(c.fuels, "total")

d.est = d.emissions[fuel %in% c.fuels][order(fuel, date), .(date, value = frollsum(kt.co2, 12) / 1000), by = fuel]
d.est = rbind(d.est, d.est[, if (.N == length(c.fuels) && !anyNA(value)) .(fuel = "total", value = sum(value)), by = date])

d.inv = d.nid[fuel %in% c.fuels, .(fuel, year, value = kt.co2 / 1000)]
d.inv = rbind(d.inv, d.inv[, if (.N == length(c.fuels)) .(fuel = "total", value = sum(value)), by = year])

d.plot = rbind(
    d.est[!is.na(value), .(date, facet = fuel, type = "estimate", value)],
    d.inv[, .(date = as.Date(paste0(year, "-12-01")), facet = fuel, type = "inventory", value)]
)
fwriteWeb(d.plot[order(match(facet, c.facets), type, date)], file.path(g$d$wd, "emissions", "estimate.csv"))
