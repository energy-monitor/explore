# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/gie/_shared.r")

# - DOIT -----------------------------------------------------------------------
countries = c("AT", "EU")

for (country in countries) {
    # Load
    d.recent = loadGieFull(country = country, startDate = "2022-01-01")

    historic.data.file = file.path(g$d$tmp, glue("gie-{country}-historic.csv"))
    if (!file.exists(historic.data.file)) {
        d.historic = loadGieFull(country = country, startDate = "2016-01-01", endDate = "2021-12-31")
        fwrite(d.historic, historic.data.file)
    }
    d.historic = fread(historic.data.file)[, gasDayStart := as.character(gasDayStart)]

    d.base = rbind(
        d.recent,
        d.historic
    )[, .(
        code = code,
        gasDayStart = as.Date(gasDayStart),
        gasInStorage = as.numeric(gasInStorage),
        consumption = as.numeric(consumption),
        consumptionFull = as.numeric(consumptionFull),
        injection = as.numeric(injection),
        withdrawal = as.numeric(withdrawal),
        netWithdrawal = as.numeric(netWithdrawal),
        workingGasVolume = as.numeric(workingGasVolume),
        injectionCapacity = as.numeric(injectionCapacity),
        withdrawalCapacity = as.numeric(withdrawalCapacity),
        status,
        trend = as.numeric(trend),
        full = as.numeric(full)
    )]

    # - STORAGE ----------------------------------------------------------------
    saveToStorages(d.base, list(
        id = glue("storage-{country}"),
        source = "gie",
        format = "csv"
    ))
}
