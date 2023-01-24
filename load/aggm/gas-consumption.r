# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/aggm/_shared.r")

d.cache = NULL

file.cache = "consumption-gas-aggm"

agg = function(d, years) {
    d[, .(
        value = sum(value) / 10^9
    ), by = .(date = as.Date(from))][
        year(date) %in% years
    ]
}



last.year = year(Sys.Date() - 1)

# - LOAD -----------------------------------------------------------------------
if (file.exists(glue('data/storage/{file.cache}.csv'))) {

    d.cache = loadFromStorage(id = file.cache)[, `:=`(
        date = as.Date(date)
    )]

}

# Split data range (else would be slow or not work)
d.update = getGasConsumption("2018-12-30", "2022-01-02")
d.t0 = agg(d.update, 2019:last.year)

d.update = getGasConsumption("2015-12-30", "2019-01-02")
d.t1 = agg(d.update, 2016:2018)

d.update = getGasConsumption("2012-12-30", "2016-01-02")
d.t2 = agg(d.update, 2013:2015)

d.base = agg(getGasConsumption(startDate = "2021-12-30"), 2022:2024)[order(date)]

d.full = rbind(d.t2, d.t1, d.t0, d.base) %>%
    na.omit()

if(!is.null(d.cache)){

    d.full.cache = left_join(d.full, d.cache, by = c("date" = "date")) %>%
        dplyr::select(date, value.update = value.x, value.cache = value.y)

    min.cache = min(d.full.cache$value.cache, na.rm =TRUE)

    # if updated data contains very low values
    # (i.e smaller than historical minimum, replace by cache, unless cache is NA)
    d.full.cache = d.full.cache %>%
        mutate(value.update = ifelse(value.update < min.cache & !is.na(value.cache), value.cache, value.update))

}else{

    d.full.cache = d.full %>%
        mutate(value.cache = NULL) %>%
        dplyr::select(date, value.update = value, value.cache)

}

d.full = d.full.cache %>%
    dplyr::select(date, value = value.update)

update.time = now()

saveToStorages(d.full, list(
    id = "consumption-gas-aggm",
    source = "aggm",
    format = "csv",
    update.time = update.time
))

