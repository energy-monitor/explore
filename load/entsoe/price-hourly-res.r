# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/entsoe/_shared.r")
# loadPackages()


# - DOIT -----------------------------------------------------------------------
update.time = now()
d.base = load_entsoe_data(
    c.nice2entsoe["dayAheadPrices"], from = date.start
)

d.base.f = d.base[ResolutionCode == "PT60M"]
d.base.f = d.base.f[, hour := floor_date(DateTime, unit = "hours")]

d.agg = d.base.f[, .(
    price = mean(`Price[Currency/MWh]`)
), by = .(
    country = MapCode,
    DateTime = hour,
    ResolutionCode
)][order(country, DateTime)]


# # - STORE --------------------------------------------------------------------
saveToStorages(
    d.agg,
    list(
        id = "electricity-price-entsoe-hourly",
        source = "entsoe",
        format = "csv",
        update.time = update.time
    )
)

