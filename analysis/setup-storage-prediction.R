###### code for storage prediction ######

days_of_months = c(31, 30, 31, 31, 28, 31)

diff.days.dec.mar = as.numeric(as.Date("2023-03-31") - as.Date("2022-12-31") + 1)

learning.period.days = 365

### these values are taken from https://energie.gv.at/gut-zu-wissen/wie-wird-der-winter
gas.from.russia = 10
gas.from.russia.per.day = gas.from.russia / (sum(days_of_months))

gas.domestic = 4
gas.domestic.per.day = gas.domestic / (sum(days_of_months))

gas.from.others = 33
gas.from.others.per.day = gas.from.others / (sum(days_of_months))

### values from energie.gv.at (Eigentumsverhältnisse Gasspeicher)
storage.start.strategic = 20

storage.levels.domestic = tibble(date = c(as.Date("2022-11-15"),
                                          as.Date("2022-11-22"),
                                          as.Date("2022-11-29")),
                        level = c(27.09, 26.95, 26.55))

storage.start.domestic.data = storage.levels.domestic %>% slice_tail(n = 1) %>% .$level

storage.start.domestic.date.data = storage.levels.domestic %>% slice_tail(n = 1) %>% .$date

storage.start.domestic = calc.stor.start.domestic(storage.start.domestic.data,
                                                  storage.start.domestic.date.data)

d.base = loadBase(update = TRUE)

max.date = max(d.base$date)

temp.threshold = 14

d.comb = copy(d.base)

addTempThreshold(d.comb, temp.threshold)

d.hdd = loadFromStorage(id = "temperature-hdd")[, .(
    date = as.Date(date), temp
)]

d.all.years = bind_rows(lapply(c(1950:2021),
                               one.prediction,
                               d.hdd,
                               d.base,
                               max.date)) %>%
    as.data.table() |>
    mutate(type = "Observed climate")

d.all.years.trend = bind_rows(lapply(c(1950:2021),
                               one.prediction,
                               add.temperature.trend(d.hdd),
                               d.base,
                               max.date)) %>%
    as.data.table() |>
    mutate(type = "Detrended climate")

d.all.years = bind_rows(d.all.years,
                        d.all.years.trend)

