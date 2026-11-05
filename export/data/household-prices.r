# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")

# The prices of electricity and gas for households of the countries of Europe,
# half-yearly, for the map of web/data/economy/household-prices-map.json (of
# load/eurostat/household-prices.r, the typical consumption band), in cent per
# kWh:
# - total: with all taxes and levies (I_TAX)
# - net: without taxes and levies (X_TAX), energy and network
# - levies: the taxes and levies without the VAT (X_VAT - X_TAX), negative of
#   subsidies counted as levies, e.g. the Strompreisbremse of Austria
# - vat: the VAT and other recoverable taxes (I_TAX - X_VAT)
# `period` is e.g. "2026 H1" (January to June), the countries without the
# aggregates (EU, euro area) and without the ones of no area of the map

d.base = loadFromStorage(id = "household-prices-eurostat")

d.wide = dcast(d.base, product + geo + period ~ tax, value.var = "value")
d.wide = d.wide[!is.na(I_TAX) & !geo %in% c("EU27_2020", "EA", "GE", "XK")]

d.plot = d.wide[, .(
    product,
    country = iso2(geo),
    period = sub("-S", " H", period),
    total = I_TAX * 100,
    net = X_TAX * 100,
    levies = (X_VAT - X_TAX) * 100,
    vat = (I_TAX - X_VAT) * 100
)]

fwriteWeb(d.plot[order(product, period, country)], file.path(g$d$wd, "economy", "household-prices-map.csv"))


# - AUSTRIA --------------------------------------------------------------------
# the components of the prices of Austria over time, for the stacked bars of
# web/data/economy/household-prices-at.json, `date` the first day of the half
# year, `type` the component (net, levies, vat) or the total (not stacked)
d.at = melt(
    d.plot[country == "AT"],
    id.vars = c("product", "period"), measure.vars = c("net", "levies", "vat", "total"),
    variable.name = "type"
)
d.at[, date := as.Date(paste0(substr(period, 1, 4), ifelse(substr(period, 6, 7) == "H1", "-01-01", "-07-01")))]
# the total on every row and the name of the half year, e.g. "2. Halbjahr
# 2023" (July to December), for the title of the hover of the components
d.at[, sum := value[type == "total"], by = .(product, date)]
d.at[, half := paste0(ifelse(substr(period, 6, 7) == "H1", "1", "2"), ". Halbjahr ", substr(period, 1, 4))]

fwriteWeb(d.at[order(product, type, date), .(product, date, half, type, value, sum)], file.path(g$d$wd, "economy", "household-prices-at.csv"))
