# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/aggm/_shared.r")

historic.file.name = file.path(g$d$tmp, "consumption-gas-aggm-historic.csv")

agg = function(d, years) {
    d[, .(
        value = sum(value) / 10^9
    ), by = .(date = as.Date(from))][
        year(date) %in% years
    ]
}

# - LOAD -----------------------------------------------------------------------
if (!file.exists(historic.file.name)) {
    # Split data range (else would be slow or not work)
    d.historic = getGasConsumption("2018-12-30", "2022-01-02")
    d.t0 = agg(d.historic, 2019:2021)

    d.historic = getGasConsumption("2015-12-30", "2019-01-02")
    d.t1 = agg(d.historic, 2016:2018)

    d.historic = getGasConsumption("2012-12-30", "2016-01-02")
    d.t2 = agg(d.historic, 2013:2015)

    fwrite(rbind(d.t0, d.t1, d.t2)[order(date)][!is.na(value)], historic.file.name)

    rm(d.historic, d.t0, d.t1, d.t2)
}

d.historic = fread(historic.file.name)[, date := as.Date(date)]
update.time = now()
d.base = agg(getGasConsumption(startDate = "2021-12-30"), 2022:2024)[order(date)]

# Combine
d.full = rbind(d.historic, d.base)

# loadPackages('ggplot2')
# ggplot(d.full, aes(x = date, y = value)) + geom_line()


# - STORAGE --------------------------------------------------------------------
saveToStorages(d.full, list(
    id = "consumption-gas-aggm",
    source = "aggm",
    format = "csv",
    update.time = update.time
))
