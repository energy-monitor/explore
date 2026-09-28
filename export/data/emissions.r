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

# Sum of all fuels, by year
d.plot = d.fuels[, .(value = sum(kt.co2) / 1000), by = date]
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


# - COMPARISON WITH THE NID ----------------------------------------------------
# Yearly estimates vs the national inventory, for the years available in both
d.own = emissionsFuels()[, .(value = sum(kt.co2), n = .N), by = .(fuel, year = year(date))][n == 12, !"n"]

d.nid.1a = readNid("nid_2026_1a_fuel_combustion_gas_oil.csv")
d.nid = rbind(
    d.nid.1a[grepl("^Gaseous", fuel_type), .(fuel = "gas", year, value = co2_kt)],
    d.nid.1a[grepl("^Liquid", fuel_type), .(fuel = "oil", year, value = co2_kt)],
    readNid("nid_2026_ra_solid_fuels.csv")[, .(fuel = "coal", year, value = co2_incl_stored_kt)],
    loadFromStorage(id = "uba-thg-crt")[pollutant == "CO2" & code == "Memo 1 D 1 a", .(fuel = "intaviation", year, value = value / 1000)]
)

d.plot = merge(d.own, d.nid, by = c("fuel", "year"), suffixes = c(".own", ".nid"))[year >= start.year]

# Total of gas, oil and coal, only for years with all three
c.fuels = c("gas", "oil", "coal")
d.plot = rbind(d.plot, d.plot[fuel %in% c.fuels, if (.N == length(c.fuels)) .(
    fuel = "total", value.own = sum(value.own), value.nid = sum(value.nid)
), by = year])

d.plot = melt(d.plot, id.vars = c("fuel", "year"), variable.name = "source", variable.factor = FALSE)
d.plot[, source := sub("value.", "", source, fixed = TRUE)]

# Bars of a year left (own) and right (NID) of the 1st of January
d.plot[, date := as.Date(paste0(year, "-01-01")) + fifelse(source == "own", -45, 45)]
d.plot[, value := value / 1000]

# Order of the facets
d.plot[, fuel := factor(fuel, levels = c("total", "gas", "oil", "coal", "intaviation"))]
fwrite(d.plot[order(fuel, year, source), .(date, year, fuel, source, value)], file.path(g$d$wd, "others", "emissions-nid.csv"))
