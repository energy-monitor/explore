d.base = loadBase(TRUE)
# start data with first complete year
d.base = d.base[year >= min(d.base[day == 1]$year)]
# d.base = d.base[year >= 2017]

max.date = d.base[max(date) == date]$date
max.day = yday(max.date)

d.gas.years = d.base[day <= max.day, .(days = .N, mean = mean(value) * 10^3), by = year]
mean.gas.before22 = d.gas.years[year < 2022, mean(mean)]
d.gas.years[, rel := mean / mean.gas.before22]
savings.2022 = 1 - d.gas.years[year == 2022, rel]

temp.threshold = 14

# - MODEL ----------------------------------------------------------------------

# AUGMENT
d.comb = copy(d.base)
addTempThreshold(d.comb, temp.threshold)


model.base = value ~
    t + t.squared + # week +
    temp.below.threshold + temp.below.threshold.lag + temp.below.threshold.squared +
    wday + is.holiday + as.factor(vacation.name) + is.lockdown + is.hard.lockdown

l.model.base = estimate(model.base, d.comb)


model.power = value ~
    t + t.squared + gas.power + # week +
    temp.below.threshold + temp.below.threshold.lag + temp.below.threshold.squared +
    wday + is.holiday + as.factor(vacation.name) + is.lockdown + is.hard.lockdown

l.model.power = estimate(model.power, d.comb)

d.plot = rbind(
    l.model.base$d.plot[variable %in% c("value", "prediction")][date >= "2022-03-01"],
    l.model.base$d.plot[variable %in% c("value")][date >= "2021-03-01" & date <= "2021-11-10"][, .(
        date = as.Date(paste("2022", month(date), day(date), sep = "-")),
        variable = "value21", value, rollmean
    )]
)

d.baseline.savings = d.base %>%
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

startDate = "2022-03-01"
c.years.avg = 2017:2021

d.comp.pred = rbind(
    l.model.base$d.plot[variable %in% c("prediction")][date >= startDate][, .(
        date, variable = "pred.base", value
    )],
    l.model.power$d.plot[variable %in% c("prediction")][date >= startDate][, .(
        date, variable = "pred.power", value
    )]
)[, .(value = sum(value)), by = .(month = month(date), variable)]

d.data.agg = d.base[day <= max.day & month >= 3, .(
    value = sum(value)
), by = .(month = month(date), year = year(date))]

d.comp = rbind(
    d.comp.pred,
    d.data.agg[year %in% c.years.avg, .(variable = "avg", value = mean(value)), by = month],
    d.data.agg[year == 2022, .(month, variable = "value.22", value)],
    d.data.agg[year == 2021, .(month, variable = "value.21", value)]
)

d.comp[, month.name := as.character(month(month, label = TRUE, abbr = FALSE, locale = "de_AT.UTF-8"))]


d.comp.a = rbind(
    d.comp[, .(month.name, variable, value)],
    d.comp[month >= 3, .(month.name = "**Seit März**", value = sum(value)), by = variable],
    d.comp[month >= 8, .(month.name = "**Seit August**", value = sum(value)), by = variable]
)

d.comp.a[, month.name := factor(
    month.name, unique(d.comp.a$month.name), unique(d.comp.a$month.name)
)]


d.comp.c = merge(
    d.comp.a[variable == "value.22", .(month.name, value)],
    d.comp.a[, .(month.name, variable, comparison = value)],
    by = "month.name"
)

d.comp.c[, rel := value / comparison]
d.comp.c[, g100 := round((rel - 1) * 100, 2)]

d.comp.f = dcast(d.comp.c, month.name ~ variable, value.var = "g100")
d.comp.f[, value.22 := NULL]

setcolorder(d.comp.f, c("month.name", "value.21", "avg", "pred.base", "pred.power"))
