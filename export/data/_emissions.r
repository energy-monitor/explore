# CO₂ emissions from the combustion of fossil fuels in Austria, monthly in kt.
#
# Gas and oil follow the methodology of co2-emissions-austria
# (https://github.com/ElijahStaengl/co2_fuel_combustion_final), which has been
# validated against the national inventory (NID/CRT 2026):
# - gas: inland consumption converted from GCV to NCV, minus the average gas
#   used non-energetically for ammonia production
# - oil: country specific calorific values and carbon factors per product,
#   kerosene for international aviation is split off (share from the CRT), and
#   a constant monthly amount is added to calibrate to the CRT (combustion not
#   covered by the selected products)
# - coal: as in load/eurostat/nrg_cb_sffm.r, not calibrated

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


# Drops months with missing products and everything after the first gap
completeMonths = function(d, products) {
    d = d[, if (uniqueN(product[value > 0]) == length(products)) .SD, by = date][order(date)]
    c.all = seq(min(d$date), max(d$date), by = "month")
    c.gap = setdiff(c.all, unique(d$date))
    if (length(c.gap) > 0) d = d[date < min(as.Date(c.gap))]
    d
}

emissionsGas = function() {
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

# Share of kerosene used for international aviation, average of the last 3 CRT years
emissionsIntAviationShare = function() {
    d = readNid("aviation_percent.csv")[order(year)]
    round(mean(tail(d$international_aviation_percent, 3)), 2) / 100
}

# Returns the oil emissions per product (incl. "intaviation", which is not
# part of the oil total, and the "correction" calibrating to the CRT)
emissionsOilProducts = function() {
    share.int = emissionsIntAviationShare()

    d = loadFromStorage(id = "nrg_cb_oilm-emissions")[, .(
        date = as.Date(date), product, value = ths.t
    )]
    d = completeMonths(d, emissions.oil.factors$product)
    d = merge(d, emissions.oil.factors[, .(product, kt.co2.per.kt)], by = "product")
    d[, kt.co2 := value * kt.co2.per.kt]

    d = rbind(
        d[product != "kerosene", .(date, product, kt.co2)],
        d[product == "kerosene", .(date, product, kt.co2 = kt.co2 * (1 - share.int))],
        d[product == "kerosene", .(date, product = "intaviation", kt.co2 = kt.co2 * share.int)]
    )

    # Calibration: mean yearly difference to the CRT, as constant monthly
    # value, rounded to 10 kt
    d.crt = readNid("nid_2026_1a_fuel_combustion_gas_oil.csv")[
        grepl("^Liquid", fuel_type), .(year, crt = co2_kt)
    ]
    d.year = d[product != "intaviation", .(kt.co2 = sum(kt.co2), n = uniqueN(date)), by = .(year = year(date))][n == 12]
    d.diff = merge(d.year, d.crt, by = "year")
    correction = round(abs(mean(d.diff$kt.co2 - d.diff$crt) / 12), -1)

    rbind(d, unique(d[, .(date)])[, .(date, product = "correction", kt.co2 = correction)])[order(date, product)]
}

emissionsCoal = function() {
    loadFromStorage(id = "nrg_cb_sffm")[product == "total", .(
        date = as.Date(date), fuel = "coal", kt.co2 = t.co2 / 1000
    )][!is.na(kt.co2) & kt.co2 > 0][order(date)]
}

# Monthly emissions by fuel (gas, oil, coal, intaviation) in kt CO₂
emissionsFuels = function() {
    d.oil = emissionsOilProducts()
    rbind(
        emissionsGas(),
        d.oil[product != "intaviation", .(fuel = "oil", kt.co2 = sum(kt.co2)), by = date],
        d.oil[product == "intaviation", .(date, fuel = "intaviation", kt.co2)],
        emissionsCoal()
    )[order(date, fuel)]
}
