# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/eurostat/_shared.r")

# The prices of electricity and gas for households of all countries, half-yearly
# (S1: January-June, S2: July-December), in EUR per kWh, of the typical
# consumption band (electricity 2 500-4 999 kWh a year, gas 20-199 GJ), with
# all taxes (I_TAX), without the VAT and other recoverable taxes (X_VAT) and
# without taxes (X_TAX), e.g. the share of the taxes or the comparison with the
# wholesale prices.


# - DOIT -----------------------------------------------------------------------
update.time = now()

loadPrices = function(id, product, cons) {
    d = as.data.table(get_eurostat(id, filters = list(
        nrg_cons = cons, unit = "KWH", currency = "EUR"
    ), time_format = "raw"))
    d[, .(
        product = product,
        geo,
        period = as.character(time),
        tax,
        value = values
    )][!is.na(value)]
}

d.prices = rbind(
    loadPrices("nrg_pc_204", "electricity", "KWH2500-4999"),
    loadPrices("nrg_pc_202", "gas", "GJ20-199")
)

# The first day of the half year, e.g. 2025-07-01 of 2025-S2
d.prices[, date := as.Date(paste0(
    substr(period, 1, 4), ifelse(substr(period, 6, 7) == "S1", "-01-01", "-07-01")
))]
d.prices = d.prices[order(product, geo, tax, date), .(product, geo, date, period, tax, value)]


# - STORE ----------------------------------------------------------------------
saveToStorages(d.prices, list(
    id = "household-prices-eurostat",
    source = "eurostat",
    format = "csv",
    update.time = update.time
))
