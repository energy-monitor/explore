# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")

start.year = 2014

# Estimates and inventory values, see calc/emissions.r
d.emissions = loadFromStorage(id = "emissions-fuels")[, date := as.Date(date)]
d.oil.products = loadFromStorage(id = "emissions-oil-products")[, date := as.Date(date)]
d.nid = loadFromStorage(id = "emissions-nid")


# - TOTAL ----------------------------------------------------------------------
# Monthly emissions by fuel, only months with data for all fuels
c.fuels = c("gas", "oil", "coal")
d.fuels = d.emissions[fuel %in% c.fuels]
d.fuels = d.fuels[, if (.N == length(c.fuels)) .SD, by = date]

d.plot = d.fuels[year(date) >= start.year, .(date, fuel, value = kt.co2 / 1000)]
fwrite(d.plot[order(date, fuel)], file.path(g$d$wd, "others", "emissions-total.csv"))

# Sum of all fuels, by year
d.plot = d.fuels[, .(value = sum(kt.co2) / 1000), by = date]
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

d.plot = d.plot[order(year, date20)]
d.plot = rbind(
    d.plot[, .(year, date20, variable = "month", value)],
    d.plot[, .(date20, variable = "cum", value = cumsum(value)), by = year]
)

fwrite(d.plot, file.path(g$d$wd, "others", "emissions-total-year.csv"))


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
fwrite(d.plot[order(date, product)], file.path(g$d$wd, "others", "emissions-oil.csv"))


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

fwrite(d.plot[order(year, date20)], file.path(g$d$wd, "others", "emissions-aviation.csv"))


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
fwrite(d.plot[order(fuel, year), .(date, year, fuel, sign, value = deviation, own = value.own / 1000, nid = value.nid / 1000)], file.path(g$d$wd, "others", "emissions-nid.csv"))
