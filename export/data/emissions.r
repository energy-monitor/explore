# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")
source("export/data/_emissions.r")

start.year = 2014


# - TOTAL ----------------------------------------------------------------------
# Monthly emissions by fuel, only months with data for all fuels
c.fuels = c("gas", "oil", "coal")
d.fuels = emissionsFuels()[fuel %in% c.fuels]
d.fuels = d.fuels[, if (.N == length(c.fuels)) .SD, by = date]

d.plot = d.fuels[year(date) >= start.year, .(date, fuel, value = kt.co2 / 1000)]
fwrite(d.plot[order(date, fuel)], file.path(g$d$wd, "others", "emissions-total.csv"))


# - OIL PRODUCTS ---------------------------------------------------------------
# Rolling 12 months mean by product group
c.groups = c(
    gasoil = "gasoil", gasoline = "gasoline",
    kerosene = "others", lpg = "others", fueloil = "others",
    correction = "correction"
)
d.oil = emissionsOilProducts()[product %in% names(c.groups)]
d.oil = d.oil[, .(kt.co2 = sum(kt.co2)), by = .(date, product = c.groups[product])][order(product, date)]
d.oil[, value := frollmean(kt.co2, 12), by = product]

d.plot = d.oil[year(date) >= start.year & !is.na(value), .(date, product, value)]
fwrite(d.plot[order(date, product)], file.path(g$d$wd, "others", "emissions-oil.csv"))


# - INTERNATIONAL AVIATION -----------------------------------------------------
d.plot = emissionsFuels()[fuel == "intaviation", .(date, value = kt.co2)]

# eurostat reports international aviation from 2014 on
d.plot[, year := ifelse(year(date) %in% 2014:2018, "avg14-18", year(date))]

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
