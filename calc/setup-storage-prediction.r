# - INIT -----------------------------------------------------------------------
source("_shared.r")
loadPackages(tidyverse)
source("calc/prediction-gas-consumption/_functions.r")


# - CONF -----------------------------------------------------------------------
temp.threshold = 14

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

learning.period.days = 365


# - PREP -----------------------------------------------------------------------
days_of_months = c(31, 30, 31, 31, 28, 31)

# AUGMENT GAS INFO DICT
l.gas.info$c.sources.daily = sapply(l.gas.info$c.sources, function(a) a / sum(days_of_months))

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

max.date = max(d.base$date)

diff.days.dec.mar = as.numeric(as.Date("2023-03-31") - as.Date("2022-12-31") + 1)


d.hdd = loadFromStorage(id = "temperature-hdd")[, .(
    date = as.Date(date), temp
)]

d.all.years = bind_rows(lapply(
    c(1950:2021), one.prediction, d.hdd, d.base, max.date
)) %>% mutate(type = "Observed climate")

d.all.years.trend = bind_rows(lapply(
    c(1950:2021), one.prediction, add.temperature.trend(d.hdd), d.base, max.date
)) %>% mutate(type = "Detrended climate")

d.all.years = bind_rows(d.all.years, d.all.years.trend)
