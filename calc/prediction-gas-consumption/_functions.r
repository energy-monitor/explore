loadBase = function(update) {
    base.file.name = file.path(g$d$tmp, "prediction-gas-consumption-base.rData")

    if (!update && file.exists(base.file.name)) {
        return(readRDS(base.file.name))
    }

    # - LOAD -------------------------------------------------------------------
    # Temp & HDD
    d.hdd = loadFromStorage(id = "temperature-hdd")[, `:=`(
        date = as.Date(date)
    )]

    # Gas Consumption
    d.consumption = loadFromStorage(id = "consumption-gas-aggm")[, .(
        date = as.Date(date), value
    )]

    # Electricity generation gas
    d.power.gas = loadFromStorage(id = "electricity-generation-g1")[
        country == "AT" & source.group == "Gas", .(
        date = as.Date(date), gas.power = value
    )]

    # Holidays
    d.holidays = loadFromStorage(id = "holidays")[, `:=`(
        date = as.Date(date)
        # holiday.name = ifelse(holiday.name == "", NA, holiday.name),
        # vacation.name = ifelse(vacation.name == "", NA, vacation.name)
    )]

    d.lockdowns = loadFromStorage(id = "lockdowns")[, `:=`(
        date = as.Date(date),
        is.hard.lockdown = is.hard.lockdown > 0,
        is.lockdown = is.lockdown > 0
    )]

    # - MERGE ------------------------------------------------------------------
    d.comb = merge(d.consumption, d.hdd, by = "date")
    d.comb = merge(d.comb, d.holidays, by = "date")

    d.comb = merge(d.comb, d.lockdowns, by = "date", all.x = TRUE)
    d.comb[is.na(is.hard.lockdown), is.hard.lockdown := FALSE]
    d.comb[is.na(is.lockdown), is.lockdown := FALSE]

    d.comb = merge(d.comb, d.power.gas, by = "date")

    min(d.comb$date)

    d.comb[, `:=`(value.without.power = value - gas.power)]

    # - AUGMENT ----------------------------------------------------------------
    d.comb[, `:=`(
        t = as.integer(date - min(date)),
        year = year(date),
        month = month(date),
        day = yday(date),
        week = str_pad(week(date), 2, pad = "0"),
        wday = factor(
            as.character(clock::date_weekday_factor(date)),
            c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
        )
    )]

    d.comb[, `:=`(
        t.squared = t^2,
        workday = ifelse(wday %in% c("Sat", "Sun"), as.character(wday), "Working day")
    )]

    saveRDS(d.comb, base.file.name)
    d.comb[]
}

addTempThreshold = function(d, threshold) {
    d[, `:=`(
        temp.below.threshold = ifelse(temp < threshold, threshold - temp, 0)
        # temp.above.threshold = ifelse(temp > threshold, temp - threshold, 0)
    )]

    d[, `:=`(
        temp.below.threshold.squared = temp.below.threshold^2,
        temp.below.threshold.lag = shift(temp.below.threshold, 1)
    )]
}


### the economic indicator is published at monthly resolution, t + 45 days
### to be able to predict up to the current moment,
### I do a very dirty prediction of the economic indicator:
### it is economic activity of (t - 365 days) times
### the growth rate of the current year for the available time period
### compared to last year
growth.rate = function(d.economic.activity) {
    maximum.month.last.year = as_tibble(d.economic.activity) %>%
        filter(year == max(year)) %>%
        filter(month == max(month)) %>%
        dplyr::select(month) %>%
        unlist()

    as_tibble(d.economic.activity) %>%
        filter(year >= max(year) - 1) %>%
        filter(month <= maximum.month.last.year) %>%
        group_by(year) %>%
        summarize(economic.activity = sum(economic.activity)) %>%
        add_row(.[2, ] / .[1, ]) %>%
        slice_tail(n = 1) %>%
        dplyr::select(economic.activity) %>%
        unlist()

}

estimate = function(model, d, train.years = c(2000:2021)) {
    d.train = d[year %in% train.years]
    d.ret = copy(d)

    m.linear = lm(model, data = d.train)

    prediction.prediction = predict(m.linear, d.ret, interval = "prediction", level = 0.95) %>%
        as.data.table()

    prediction.confidence = predict(m.linear, d.ret, interval = "confidence", level = 0.95) %>%
        as.data.table()

    d.ret[, `:=`(
        prediction = prediction.prediction$fit,
        lower.pred = prediction.prediction$lwr,
        upper.pred = prediction.prediction$upr,
        lower.conf = prediction.confidence$lwr,
        upper.conf = prediction.confidence$upr
    )]

    d.ret[, `:=`(
        difference = (value - prediction) / prediction,
        diff.pred.lower = (value - lower.pred) / lower.pred,
        diff.pred.upper = (value - upper.pred) / upper.pred,
        diff.conf.lower = (value - lower.conf) / lower.conf,
        diff.conf.upper = (value - upper.conf) / upper.conf
    )]

    d.ret[, `:=`(
        difference.mean = rollmean(difference, 14, fill = NA)
    )]

    d.plot = d.ret[, .(
        date, value, prediction, difference,
        diff.conf.lower, diff.conf.upper, diff.pred.lower, diff.pred.upper
    )]

    d.plot = melt(d.plot, id.vars = "date")
    d.plot[, rollmean := rollmean(value, 14, fill = NA, align = "right"), by = variable]

    list(
        m = m.linear,
        d.pred = d.ret,
        d.plot = d.plot
    )
}

cross.validation <- function(model, d, train.years){

    f = function(year){
        years.in = train.years[train.years != year]
        d.res = estimate(model, d, years.in)$d.pred
        val.year = d.res[year == year]
        r2 = round(cor(val.year$value, val.year$prediction, use = "pairwise.complete.obs")^2, 2)
        rmse = round(rmse(val.year$value, val.year$prediction), 2)
        return(tibble(year = c(year), r2 = c(r2), rmse = c(rmse)))
    }

    mapply(f, train.years, SIMPLIFY = FALSE) |>
        bind_rows()
}

getSummaryTable = function(m) {
    d = data.table(coef(summary(m)), keep.rownames = "term")
    d[, Variable := c.nice.names[term]]
    d[is.na(Variable), Variable := term]
    d$`Pr(>|t|)` = NULL
    d$term = NULL
    d$`t value` = NULL
    setcolorder(d, "Variable")
    kable(d, align = "r", digits = 3)
}

one.prediction = function(year.select, d.hdd, d.base, start.date, prediction.start = start.date) {

    model.base = value ~
        #t + t.squared + # week +
        temp.below.threshold +
        temp.below.threshold.lag +
        temp.below.threshold.squared +
        wday + is.holiday + as.factor(vacation.name)

    d.train = d.comb[(date > (start.date - learning.period.days) & date <= start.date)]

    m.linear = lm(model.base, data = d.train)

    temp.in = d.hdd[((year(date) == year.select) & (month(date) >= 10)) |
                        ((year(date) == (year.select + 1)) & (month(date) <= 3))]

    temp.in[, `:=`(
        date = date - diff.days.dec.mar
    )]

    temp.in[, `:=`(
        day = yday(date),
        year = year(date)
    )]

    addTempThreshold(temp.in, temp.threshold)

    d.prediction = copy(d.base)

    d.prediction = d.prediction[, `:=`(date = date + 365), ][(date >= (prediction.start)) &
                                                                 (date < "2023-04-01")][, .(date)]

    d.prediction[, `:=`(
        wday = factor(
            as.character(clock::date_weekday_factor(date)),
            c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
        )
    )]

    holidays = loadFromStorage(id = "holidays")

    d.prediction = merge(d.prediction, holidays, by = "date") [ ,.(date,
                                                                   wday,
                                                                   is.holiday,
                                                                   vacation.name)]



    d.prediction[, `:=`(date = date - diff.days.dec.mar), ]

    d.prediction[, `:=`(day = yday(date)), ]

    d.prediction = merge(d.prediction, temp.in, by = "day")

    prediction = predict(m.linear, d.prediction)

    d.prediction[, `:=`(
        day = day,
        year = year,
        gas.cons.cum = cumsum(prediction),
        prediction = prediction,
        storage.strategic = storage.start.strategic,
        storage.domestic = storage.start.domestic,
        gas.from.russia = gas.from.russia.per.day,
        gas.other = gas.from.others.per.day + gas.domestic.per.day
    )]

    d.prediction[, `:=`(
        storage.with.russia = storage.domestic + storage.strategic - gas.cons.cum +
            cumsum(gas.from.russia) + cumsum(gas.other),
        storage.without.russia = storage.domestic + storage.strategic - gas.cons.cum + cumsum(gas.other)
    )]

    d.prediction = d.prediction %>%
        dplyr::select(
            day, year,
            storage.with.russia,
            storage.without.russia,
            gas.cons.pred = prediction
        ) %>%
        gather(variable, value, -day, -year) %>%
        mutate(year = year.select)

    d.prediction %>%
        mutate(day = as.numeric(day)) %>%
        mutate(day.name = day + diff.days.dec.mar - 1) %>%
        mutate(date = as.Date(day.name, origin = "2022-01-01"))
}

reforecasting_consumption_model = function(year, d.hdd, d.base, start.date, max.date){
    pred.from.start = one.prediction(2022, d.hdd, d.base, as.Date("2022-11-15")) %>%
        spread(variable, value)

    pred.from.end = one.prediction(2022, d.hdd, d.base, max.date, as.Date("2022-11-15")) %>%
        spread(variable, value)


    updating.period = seq(as.Date("2022-11-15"), max.date, by = 1)

    predict.one.day = function(year.select, d.hdd, d.base, updating.period){
        one.prediction(year.select, d.hdd, d.base, updating.period) %>%
            filter(date == min(date)) %>%
            return()
    }

    pred.update.daily = bind_rows(mapply(predict.one.day, list(2022), list(d.hdd), list(d.base), updating.period, SIMPLIFY = FALSE)) %>%
        dplyr::select(date, variable, value) %>%
        spread(variable, value)

    d.base %>%
        dplyr::select(date, gas.cons.obs = value) %>%
        merge(pred.from.start, by = "date") %>%
        merge(pred.update.daily, by = "date") %>%
        merge(pred.from.end, by = "date") %>%
        return()
}

get.d.loess = function(d.base){
    d.base[, .(
        date, temp,
        value = value * 1000
    )]
}

get.d.lines = function(d.loess){
    m.loess = loess(value ~ temp, d.loess)

    order.loess = order(d.loess$temp)

    d.lines = data.table(
        temp = d.loess$temp[order.loess],
        value = m.loess$fitted[order.loess]
    )
}

calc.stor.start.domestic = function(storage.start.domestic,
                                    storage.start.domestic.date){

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
}

rmse = function(a, b){
    tibble(a, b) |>
        na.omit() |>
        summarize(rmse = sqrt(mean((a - b)^2))) |>
        unlist()
}
