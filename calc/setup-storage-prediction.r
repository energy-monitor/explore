# - INIT -----------------------------------------------------------------------
source("_shared.r")
loadPackages(tidyverse)
source("calc/prediction-gas-consumption/_functions.r")


# - CONF -----------------------------------------------------------------------
l.options = list(
    temp.threshold = 14,
    lag.max = 1,
    period = list(
        start = as.Date("2022-10-01"),
        end = as.Date("2023-04-01"),
        pred.start = as.Date("2022-11-15")
    ),
    learning.days = 365
)

# these values are taken from https://energie.gv.at
l.gas.info = list(
    c.sources = c(
        russia   = 10,
        domestic =  4,
        others   = 33
    ),
    l.storage = list(
        # Eigentumsverhältnisse Gasspeicher
        strategic = 20,
        c.domestic = c(
            "2022-11-15" = 27.09,
            "2022-11-22" = 26.95,
            "2022-11-29" = 18.76 + 7.79,
            "2022-12-06" = 25.67,
            "2022-12-20" = 22.46,
            "2022-12-27" = 21.72,
            "2023-01-03" = 21.89,
            "2023-01-10" = 22.09,
            "2023-01-17" = 22.00
        )

    )
)

# - DATA -----------------------------------------------------------------------

d.full = loadBase(update = TRUE)

max.date = max(d.full[!is.na(temp), date])

d.base = d.full[date >= max.date - l.options$learning.days - l.options$lag.max & date <= l.options$period$end]

addTempThreshold(d.base, l.options$temp.threshold)

d.temp = d.full[!is.na(temp), .(date, temp)]


# - PREP -----------------------------------------------------------------------
# augment gas info
l.options$period$length = as.integer(l.options$period$end - l.options$period$start)

l.gas.info$c.sources.daily = sapply(l.gas.info$c.sources, function(a) a / l.options$period$length)

l.gas.info$l.storage$d.domestic = data.table(
    date = as.Date(names(l.gas.info$l.storage$c.domestic)),
    level = l.gas.info$l.storage$c.domestic
)
l.gas.info$l.storage$d.domestic.official.last = l.gas.info$l.storage$d.domestic[date == max(date)]

# impute storage.dom
d.base = merge(
    d.base, l.gas.info$l.storage$d.domestic[, .(date, storage.dom.org = level)],
    by = "date", all = TRUE
)
d.base[, prop.dom.int.org := storage.dom.org / (storage - l.gas.info$l.storage$strategic)]
d.base[!is.na(storage) & date >= l.options$period$start, prop.dom.int := zoo::na.fill(prop.dom.int.org, "extend")]
d.base[, storage.dom := prop.dom.int * (storage - l.gas.info$l.storage$strategic)]

max.date.storage = max(d.base[!is.na(storage.dom)]$date)

l.gas.info$l.storage$d.domestic.last = d.base[
    date == max.date.storage, .(
        date, level = storage.dom
    )
]


d.all.years = bind_rows(lapply(
    1950:2021, one.prediction, d.temp, d.base, max.date
)) %>% mutate(type = "observed")




# Detrend functions
estimate.temperature.trend = function(d.temp) {
    d.temp = d.temp |>
        mutate(t = seq_len(n()))

    summary(lm(temp ~ t, data = d.temp))
    summary(lm(temp ~ t, data = d.temp))$coefficients[2, 1]
}

add.temperature.trend = function(d.temp) {
    t.inc = estimate.temperature.trend(d.temp)

    d.temp |>
        mutate(t = rev(seq_len(n()))) |>
        mutate(temp = temp + t * t.inc)
}

d.all.years.trend = bind_rows(lapply(
    1950:2021, one.prediction, add.temperature.trend(d.temp), d.base, max.date
)) %>% mutate(type = "detrended")

d.all.years = bind_rows(d.all.years, d.all.years.trend)
