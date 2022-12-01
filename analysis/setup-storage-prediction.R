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
### these values are not updated regularly
### I therefore update them proportional to emptying the complete gas storage
### Date of the initial data is 15/11/2022
storage.start.strategic = 20

#storage levels
storage.levels = tibble(date = c(as.Date("2022-11-15"), as.Date("2022-11-22")),
                        level = c(27.09, 26.95))
storage.start.domestic = storage.levels %>% slice_tail(n = 1) %>% .$level
storage.start.domestic.date = storage.levels %>% slice_tail(n = 1) %>% .$date


storage.at = loadFromStorage("storage-AT") %>%
    filter(year(gasDayStart) == 2022)

storage.at.fill.state = storage.at %>%
    filter(gasDayStart == storage.start.domestic.date) %>%
    mutate(value = gasInStorage)

storage.at.last = storage.at %>%
    arrange(gasDayStart) %>%
    slice_tail() %>%
    mutate(value = gasInStorage)

prop.dom.int = (storage.start.domestic) / (storage.at.fill.state$value -  storage.start.strategic)

change.in.storage = (storage.at.fill.state$value - storage.at.last$value) * prop.dom.int

storage.start.domestic = storage.start.domestic - change.in.storage

d.base = loadBase(update = TRUE)

max.date = max(d.base$date)

temp.threshold = 14

d.comb = copy(d.base)

addTempThreshold(d.comb, temp.threshold)

d.hdd = loadFromStorage(id = "temperature-hdd")[, .(
    date = as.Date(date), temp
)]

d.all.years = bind_rows(lapply(c(1950:2021), one.prediction, d.hdd, d.base, max.date)) %>%
    as.data.table()

