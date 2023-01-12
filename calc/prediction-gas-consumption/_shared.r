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
# d.base = d.base[year >= min(d.base[day == 1]$year)]

max.date = d.base[max(date) == date]$date
max.day = day(max.date)
max.year = year(max.date)
max.month = month(max.date)

#d.gas.years = d.base[day <= max.day, .(days = .N, mean = mean(gas.consumption) * 10^3), by = year]
#mean.gas.before22 = d.gas.years[year < 2022, mean(mean)]
#d.gas.years[, rel := mean / mean.gas.before22]
#savings.2022 = 1 - d.gas.years[year == 2022, rel]


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

d.data.agg = d.base[month(date) != max.month, .(
    variable = "observation",
    value = sum(gas.consumption)

), by = .(month = month(date), year = year(date))]

d.data.agg.short = d.base[(max.month == month(date)) &
                              (day(date) <= max.day), .(
    variable = "observation",
    value = sum(gas.consumption)
    ),
    by = .(month = month(date), year = year(date))]

available.months.march = d.comp %>%
    filter(variable == "pred.base") %>%
    dplyr::select(month.name) %>%
    unique()

available.months.august = d.comp %>%
    filter(variable == "pred.base") %>%
    dplyr::select(month.name) %>%
    unique() %>%
    mutate(n=1:n()) %>%
    filter(n>5)

d.comp = rbind(
    d.comp.pred[ , .(year = "prediction", month, variable, value)],
    d.data.agg[year %in% c.years.avg, .(year="2017-2021", variable = "observation", value = mean(value)), by = month],
    d.data.agg[year == 2023, .(year = "2023", month, variable, value)],
    d.data.agg[year == 2022  & month >=3, .(year = "2022", month, variable, value)],
    d.data.agg[year == 2021, .(year = "2021", month, variable, value)],
    d.data.agg.short[year %in% c.years.avg, .(year="2017-2021", variable = "observation", value = mean(value)), by = month],
    d.data.agg.short[year == 2023, .(year = "2023", month, variable, value)],
    d.data.agg.short[year == 2022 & month >=3, .(year = "2022", month, variable, value)],
    d.data.agg.short[year == 2021, .(year = "2021", month, variable, value)]
)

d.comp[, month.name := as.character(month(month, label = TRUE, abbr = FALSE, locale = "de_AT.UTF-8"))]

d.comp = d.comp %>%
    mutate(year = ifelse(year %in% c("2022", "2023"), "2022-2023", year))

available.months.march = d.comp %>%
    filter(variable == "pred.base") %>%
    dplyr::select(month.name) %>%
    unique()

available.months.august = d.comp %>%
    filter(variable == "pred.base") %>%
    dplyr::select(month.name) %>%
    unique() %>%
    mutate(n=1:n()) %>%
    filter(n>5)

d.comp.a = rbind(
    d.comp[, .(year, month.name, variable, value)],
    d.comp %>%
        ungroup() %>%
        filter(month.name %in% available.months.march$month.name) %>%
        group_by(year, variable) %>%
        summarize(value = sum(value)) %>%
        mutate(month.name = "**Seit März**"),
    d.comp %>%
        ungroup() %>%
        filter(month.name %in% available.months.august$month.name) %>%
        group_by(year, variable) %>%
        summarize(value = sum(value)) %>%
        mutate(month.name = "**Seit August**")
)

d.comp.a[, month.name := factor(
    month.name, unique(d.comp.a$month.name), unique(d.comp.a$month.name)
)]

d.comp.c = d.comp.a %>%
    as_tibble() %>%
    filter(year == "2022-2023",
           variable == "observation") %>%
    left_join(d.comp.a, by= c("month.name")) %>%
    mutate(comparison = value.y,
           value = value.x) %>%
    filter(year.y != "2022-2023") %>%
    as.data.table()

d.comp.c[, rel := value / comparison]
d.comp.c[, g100 := round((rel - 1) * 100, 2)]
d.comp.c = d.comp.c %>%
    mutate(full.variable = glue('{year.y}.{variable.y}'))

d.comp.f = dcast(d.comp.c, month.name ~ full.variable, value.var = "g100")

setcolorder(d.comp.f, c("month.name", "2021.observation", "2017-2021.observation", "prediction.pred.base", "prediction.pred.power"))
