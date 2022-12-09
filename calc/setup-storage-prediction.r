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
        end = as.Date("2023-04-01")
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
            "2022-11-29" = 18.76 + 7.79
        )
    )
)


# - PREP -----------------------------------------------------------------------
# augment gas info
l.options$period$length = as.integer(l.options$period$end - l.options$period$start)

l.gas.info$c.sources.daily = sapply(l.gas.info$c.sources, function(a) a / l.options$period$length)

l.gas.info$l.storage$d.domestic = data.table(
    date = as.Date(names(l.gas.info$l.storage$c.domestic)),
    level = l.gas.info$l.storage$c.domestic
)
l.gas.info$l.storage$d.domestic.official.last = l.gas.info$l.storage$d.domestic[date == max(date)]

calc.stor.start.domestic = function(d.official.last = l.gas.info$l.storage$d.domestic.official.last) {
    d.storage = loadFromStorage("storage-AT")[, .(
        date = gasDayStart, value = gasInStorage
    )][order(date)]

    d.storage.official.last = d.storage[date == d.official.last$date]
    d.storage.last = tail(d.storage, 1)

    prop.dom.int = d.official.last$level / (d.storage.official.last$value - l.gas.info$l.storage$strategic)

    change.in.storage = (d.storage.last$value - d.storage.official.last$value) * prop.dom.int

    data.table(
        date = d.storage.last$date,
        level = d.official.last$level + change.in.storage
    )
}

l.gas.info$l.storage$d.domestic.last = calc.stor.start.domestic(l.gas.info$l.storage$d.domestic.official.last)


# - DATA -----------------------------------------------------------------------

# load
d.base = loadBase(update = TRUE)
max.date = max(d.base[!is.na(temp), date])

d.base = d.base[date >= max.date - l.options$learning.days - l.options$lag.max & date <= l.options$period$end]

addTempThreshold(d.base,  l.options$temp.threshold)

diff.days.dec.mar = as.numeric(as.Date("2023-03-31") - as.Date("2022-12-31") + 1)


d.temp = loadFromStorage(id = "temperature-hdd")[, .(
    date = as.Date(date), temp
)]

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
