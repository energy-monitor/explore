# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/aggm/_shared.r")

file.cache = "consumption-gas-aggm"
historic.file.name = file.path(g$d$tmp, "consumption-gas-aggm-historic.csv")

agg = function(d, years) {
    d[, .(
        value = sum(value) / 10^9
    ), by = .(date = as.Date(from))][
        year(date) %in% years
    ]
}

# - LOAD -----------------------------------------------------------------------
d.cache = NULL
if (file.exists(glue("data/storage/{file.cache}.csv"))) {
    d.cache = loadFromStorage(id = file.cache)[, `:=`(
        date = as.Date(date)
    )]
}

if (!file.exists(historic.file.name)) {
    # Split data range (else would be slow or not work)
    d.t = getGasConsumption("2021-12-30", "2024-01-02")
    d.22_23 = agg(d.t, 2022:2023)

    d.t = getGasConsumption("2018-12-30", "2022-01-02")
    d.19_21 = agg(d.t, 2019:2021)

    d.t = getGasConsumption("2015-12-30", "2019-01-02")
    d.16_18 = agg(d.t, 2016:2018)

    d.t = getGasConsumption("2012-12-30", "2016-01-02")
    d.13_15 = agg(d.t, 2013:2015)

    d.t = rbind(d.22_23, d.19_21, d.16_18, d.13_15)[order(date)][!is.na(value)]

    fwrite(d.t, historic.file.name)
    d.historic = d.t

    rm(d.t, d.22_23, d.19_21, d.16_18, d.13_15)
}

d.historic = fread(historic.file.name)[, date := as.Date(date)]

update.time = now()
d.base = agg(getGasConsumption(startDate = "2023-12-30"), 2023:year(Sys.Date()))[order(date)]

d.full = rbind(d.historic, d.base) %>%
    na.omit()

d.full[, value := abs(value)]
# if (!is.null(d.cache)) {
#     d.full.cache = left_join(d.full, d.cache, by = c("date" = "date")) %>%
#         dplyr::select(date, value.update = value.x, value.cache = value.y)

#     min.cache = min(d.full.cache$value.cache, na.rm = TRUE)

#     # if updated data contains very low values
#     # (i.e smaller than historical minimum, replace by cache, unless cache is NA)
#     d.full.cache = d.full.cache %>%
#         mutate(value.update = ifelse(value.update < min.cache & !is.na(value.cache), value.cache, value.update))

# } else {
#     d.full.cache = d.full %>%
#         mutate(value.cache = NULL) %>%
#         dplyr::select(date, value.update = value, value.cache)
# }

# d.full = d.full.cache %>%
#     dplyr::select(date, value = value.update)

saveToStorages(d.full, list(
    id = "consumption-gas-aggm",
    source = "aggm",
    format = "csv",
    update.time = update.time
))
