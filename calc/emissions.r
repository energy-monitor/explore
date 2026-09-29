# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("_shared.r")

# CO₂ emissions from the combustion of fossil fuels in Austria, monthly in kt,
# saved as "emissions-fuels" (by fuel) and "emissions-oil-products", together
# with the yearly values of the national inventory ("emissions-nid").
#
# Gas and oil follow the methodology of co2-emissions-austria
# (https://github.com/ElijahStaengl/co2_fuel_combustion_final), which has been
# validated against the national inventory (NID/CRT 2026):
# - gas: inland consumption converted from GCV to NCV, minus the average gas
#   used non-energetically for ammonia production
# - oil: country specific calorific values and carbon factors per product,
#   kerosene delivered to international aviation is split off, and
#   a constant monthly amount is added to calibrate to the CRT (combustion not
#   covered by the selected products)
# - international aviation: deliveries to international aviation from
#   eurostat, calibrated to the inventory (Umweltbundesamt, load/uba-thg-crt.r)
#   with the mean ratio of the last 3 years
# - coal: hard coal, brown coal and net imports of coke (coke produced in
#   Austria is made from hard coal, which is counted already), with the
#   country specific factors of the NID, not calibrated; compared against the
#   solid fuels of the reference approach incl. the carbon stored (used as
#   reductant in blast furnaces, CRT 2.C.1)


# - CONF -----------------------------------------------------------------------
emissions.nid.path = "data/nid"

readNid = function(file) {
    fread(file.path(emissions.nid.path, file), sep = ";", dec = ",")
}

# NID 2026, page 164 and 172
emissions.gas.tco2.per.tj = 55.6
emissions.gas.ncv.per.gcv = 0.9

# NID 2026, Annex 3, Table A 67: calorific value [TJ/kt], carbon factor [t C/TJ]
emissions.oil.factors = data.table(
    product = c("gasoil", "fueloil", "lpg", "gasoline", "kerosene"),
    tj.per.kt = c(42.5, 41.47, 46.12, 41.63, 43.40),
    tc.per.tj = c(20.20, 21.10, 17.20, 18.90, 19.50)
)[, kt.co2.per.kt := tj.per.kt * tc.per.tj * 44 / 12 / 1000]

# NID 2026, Annex 3, Table A 67, incl. the fraction of carbon oxidised (0.98);
# hard coal as coking coal (by far the largest share), brown coal as
# sub-bituminous coal
emissions.coal.factors = data.table(
    product = c("hardcoal", "browncoal", "coke"),
    tj.per.kt = c(30.78, 21.84, 28.21),
    tc.per.tj = c(25.53, 26.20, 29.73)
)[, kt.co2.per.kt := tj.per.kt * tc.per.tj * 0.98 * 44 / 12 / 1000]


# - FUNCTIONS ------------------------------------------------------------------
# Keeps the months with data for all products. Months missing at the start
# (series starting later) and at the end (not published yet) are dropped,
# gaps in between stop with an error, as they would break sums and rolling
# means silently.
completeMonths = function(d, products) {
    d = d[, if (all(products %in% product[!is.na(value) & value > 0])) .SD, by = date][order(date)]
    c.gap = as.Date(setdiff(seq(min(d$date), max(d$date), by = "month"), unique(d$date)))
    if (length(c.gap) > 0) {
        stop(glue("missing data for {paste(products, collapse = ', ')} in {paste(format(c.gap, '%Y-%m'), collapse = ', ')}"))
    }
    d
}

emissionsGas = function() {
    # NID 2026, page 336, Table 138: natural gas input for ammonia production [TJ, NCV]
    d.ammonia = readNid("nid_2026_ammonia_gas.csv")
    tj.ammonia.month = mean(d.ammonia$gas_used_ammonia_tj) / 12

    d = loadFromStorage(id = "nrg_cb_gasm-emissions")[, .(
        date = as.Date(date), product = "gas", value = tj.gcv
    )]
    d = completeMonths(d, "gas")
    d[, .(
        date, fuel = "gas",
        kt.co2 = (value * emissions.gas.ncv.per.gcv - tj.ammonia.month) * emissions.gas.tco2.per.tj / 1000
    )]
}

# Returns the oil emissions per product (incl. "intaviation", which is not
# part of the oil total, and the "correction" calibrating to the CRT)
emissionsOilProducts = function() {
    d = loadFromStorage(id = "nrg_cb_oilm-emissions")[, .(
        date = as.Date(date), product, value = ths.t
    )]
    d = completeMonths(d, c(emissions.oil.factors$product, "intaviation"))

    # Kerosene for international aviation is not part of the national emissions
    d = dcast(d, date ~ product, value.var = "value")[, kerosene := kerosene - intaviation]
    d = melt(d, id.vars = "date", variable.name = "product", variable.factor = FALSE)
    d = merge(d, rbind(
        emissions.oil.factors[, .(product, kt.co2.per.kt)],
        emissions.oil.factors[product == "kerosene", .(product = "intaviation", kt.co2.per.kt)]
    ), by = "product")
    d[, kt.co2 := value * kt.co2.per.kt]

    # Calibration of international aviation: mean ratio of the last 3 years
    d.crt.av = loadFromStorage(id = "uba-thg-crt")[pollutant == "CO2" & code == "Memo 1 D 1 a", .(year, crt = value / 1000)]
    d.av = d[product == "intaviation", .(kt.co2 = sum(kt.co2), n = uniqueN(date)), by = .(year = year(date))][n == 12]
    d.av = tail(merge(d.av, d.crt.av, by = "year")[order(year)], 3)
    factor.av = mean(d.av$crt / d.av$kt.co2)
    l(glue("international aviation calibration factor: {round(factor.av, 4)} ({paste(d.av$year, collapse = ', ')})"), iL = 2)
    d[product == "intaviation", kt.co2 := kt.co2 * factor.av]
    d = d[, .(date, product, kt.co2)]

    # Calibration: mean yearly difference to the CRT, as constant monthly
    # value (positive if the estimate is too low), rounded to 10 kt
    d.crt = readNid("nid_2026_1a_fuel_combustion_gas_oil.csv")[
        grepl("^Liquid", fuel_type), .(year, crt = co2_kt)
    ]
    d.year = d[product != "intaviation", .(kt.co2 = sum(kt.co2), n = uniqueN(date)), by = .(year = year(date))][n == 12]
    d.diff = merge(d.year, d.crt, by = "year")
    correction = round(mean(d.diff$crt - d.diff$kt.co2) / 12, -1)

    rbind(d, unique(d[, .(date)])[, .(date, product = "correction", kt.co2 = correction)])[order(date, product)]
}

emissionsCoal = function() {
    d = loadFromStorage(id = "nrg_cb_sffm-emissions")[, .(
        date = as.Date(date), product, value = ths.t
    )]
    d = completeMonths(d, c("hardcoal", "coke", "cokeproduction"))

    d = dcast(d, date ~ product, value.var = "value", fill = 0)[, coke := coke - cokeproduction]
    d = melt(d[, !"cokeproduction"], id.vars = "date", variable.name = "product", variable.factor = FALSE)
    d = merge(d, emissions.coal.factors[, .(product, kt.co2.per.kt)], by = "product")
    d = d[, .(fuel = "coal", kt.co2 = sum(value * kt.co2.per.kt)), by = date][order(date)]

    # Deviation from the reference approach of the NID (not calibrated)
    d.ref = readNid("nid_2026_ra_solid_fuels.csv")[, .(year, ref = co2_incl_stored_kt)]
    d.year = d[, .(kt.co2 = sum(kt.co2), n = .N), by = .(year = year(date))][n == 12]
    d.diff = merge(d.year, d.ref, by = "year")
    l(glue("coal deviation from NID: {paste0(d.diff$year, ': ', sprintf('%+.1f%%', 100 * (d.diff$kt.co2 / d.diff$ref - 1)), collapse = ', ')}"), iL = 2)

    d
}

# - DOIT -----------------------------------------------------------------------
d.oil = emissionsOilProducts()

# Monthly emissions by fuel (gas, oil, coal, intaviation) in kt CO₂
d.fuels = rbind(
    emissionsGas(),
    d.oil[product != "intaviation", .(fuel = "oil", kt.co2 = sum(kt.co2)), by = date],
    d.oil[product == "intaviation", .(date, fuel = "intaviation", kt.co2)],
    emissionsCoal()
)[order(date, fuel)]

# Yearly values of the national inventory in kt CO₂, to compare with
d.nid.1a = readNid("nid_2026_1a_fuel_combustion_gas_oil.csv")
d.nid = rbind(
    d.nid.1a[grepl("^Gaseous", fuel_type), .(fuel = "gas", year, kt.co2 = co2_kt)],
    d.nid.1a[grepl("^Liquid", fuel_type), .(fuel = "oil", year, kt.co2 = co2_kt)],
    readNid("nid_2026_ra_solid_fuels.csv")[, .(fuel = "coal", year, kt.co2 = co2_incl_stored_kt)],
    loadFromStorage(id = "uba-thg-crt")[pollutant == "CO2" & code == "Memo 1 D 1 a", .(fuel = "intaviation", year, kt.co2 = value / 1000)]
)

saveToStorages(d.fuels, list(id = "emissions-fuels", source = "calc", format = "csv"))
saveToStorages(d.oil, list(id = "emissions-oil-products", source = "calc", format = "csv"))
saveToStorages(d.nid[order(fuel, year)], list(id = "emissions-nid", source = "calc", format = "csv"))
