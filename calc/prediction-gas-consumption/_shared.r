# - INIT -----------------------------------------------------------------------
source("_shared.r")
loadPackages(stringr, tidyverse, imputeTS)

source("calc/prediction-gas-consumption/_functions.r")

# - CONF -----------------------------------------------------------------------
start.date = as.Date("2022-03-01")
temp.threshold = 14
c.years.avg = 2017:2021


# - BASE -----------------------------------------------------------------------
d.base = loadBase(TRUE)[!is.na(gas.consumption) & !is.na(temp)]
# start data with first complete year
d.base = d.base[year >= min(d.base[day == 1]$year)]

max.date = d.base[max(date) == date]$date
max.day = day(max.date)
max.year = year(max.date)
max.month = month(max.date)

d.gas.years = d.base[, .(days = .N, mean = mean(gas.consumption) * 10^3), by = year]
# d.gas.years = d.base[day <= max.day, .(days = .N, mean = mean(gas.consumption) * 10^3), by = year]
mean.gas.before22 = d.gas.years[year < 2022, mean(mean)]
d.gas.years[, rel := mean / mean.gas.before22]
savings.2022 = 1 - d.gas.years[year == 2022, rel]


# - MODEL ----------------------------------------------------------------------

# AUGMENT DATA
d.comb = copy(d.base)
addTempThreshold(d.comb, temp.threshold)
addTrend(d.comb)

# MODEL DEFINITION
model.base = gas.consumption ~
    t + t.squared + # week +
    temp.below.threshold + temp.below.threshold.lag + temp.below.threshold.squared +
    wday + is.holiday + as.factor(vacation.name) + is.lockdown + is.hard.lockdown

l.model.base = estimate(model.base, d.comb)

model.power = gas.consumption ~
    t + t.squared + gas.power + # week +
    temp.below.threshold + temp.below.threshold.lag + temp.below.threshold.squared +
    wday + is.holiday + as.factor(vacation.name) + is.lockdown + is.hard.lockdown

l.model.power = estimate(model.power, d.comb)

# PLOTTING DATA
d.baseline.savings = d.base[, value := gas.consumption] %>%
    dplyr::select(date, value) %>%
    filter(date >= "2017-01-01" & date <= "2021-12-31") %>%
    mutate(day = yday(date)) %>%
    group_by(day) %>%
    summarize(value = mean(value)) %>%
    ungroup() %>%
    mutate(date = as.Date(day, origin = "2022-01-01")) %>%
    dplyr::select(date, value) %>%
    merge(d.base %>% dplyr::select(date, value.22 = value)) %>%
    mutate(difference = (value.22 - value) / value) %>%
    gather(variable, value, -date) %>%
    group_by(variable) %>%
    mutate(rollmean = rollmean(value, 14, fill = NA)) %>%
    as.data.table()

# COMPARISON TABLE
d.comp.pred = rbind(
    l.model.base$d.plot[variable %in% c("prediction")][date >= start.date][, .(
        date, variable = "pred.base", value
    )],
    l.model.power$d.plot[variable %in% c("prediction")][date >= start.date][, .(
        date, variable = "pred.power", value
    )]
)[, .(value = sum(value)), by = .(year = year(date), month = month(date), variable)]

d.base.2017.2021 = d.base %>%
    filter(year(date) >= 2017 & year(date) <= 2021) %>%
    mutate(day = yday(date)) %>%
    group_by(day) %>%
    summarize(gas.consumption = mean(gas.consumption)) %>%
    mutate(variable = "observation 2017-2021") %>%
    as_tibble()

d.base.full = d.base %>%
    mutate(day = yday(date)) %>%
    mutate(variable = glue('observation')) %>%
    dplyr::select(date, day, gas.consumption, variable) %>%
    as_tibble()

d.predictions = bind_rows(l.model.base$d.pred %>%
    mutate(day = yday(date)) %>%
    mutate(variable = glue('prediction.base')) %>%
    dplyr::select(date, day, gas.consumption = prediction, variable),
    l.model.power$d.pred %>%
    mutate(day = yday(date)) %>%
    mutate(variable = glue('prediction.power')) %>%
    dplyr::select(date, day, gas.consumption = prediction, variable)) %>%
    as_tibble()


end.date =  max(d.predictions$date)
months = c("März 2022",
           "April 2022",
           "Mai 2022",
           "Juni 2022",
           "Juli 2022",
           "August 2022",
           "September 2022",
           "Oktober 2022",
           "November 2022",
           "Dezember 2022",
           "Jänner 2023",
           "Februar 2023")

start.dates = seq(as.Date("2022-03-01"), end.date, by = "month")
end.dates = c(start.dates[2:length(start.dates)] - 1, end.date)

months.rel = mapply(compare.values.complete,
       start.dates,
       end.dates,
       months[1:length(start.dates)],
       SIMPLIFY = FALSE
       ) %>%
    bind_rows()


totals.rel = bind_rows(
    compare.values.complete(as.Date("2022-01-01"),
                            as.Date("2022-12-31"),
                            "Gesamt 2022"),
    compare.values.complete(as.Date("2022-03-01"),
                        end.date,
                        "Seit März 2022"),
    compare.values.complete(as.Date("2022-08-01"),
                        end.date,
                        "Seit August 2022"),
    compare.values.complete(as.Date("2023-01-01"),
                        end.date,
                        "Seit Jänner 2023")
)

totals.rel$name = paste0("**", totals.rel$name, "**")

d.comp.f = bind_rows(months.rel, totals.rel)


#d.comp.f = dcast(d.comp.c, month.name ~ full.variable, value.var = "g100")

#setcolorder(d.comp.f, c("month.name", "2021.observation", "2017-2021.observation", "prediction.pred.base", "prediction.pred.power"))

