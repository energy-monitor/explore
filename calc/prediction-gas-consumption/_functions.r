loadBase = function(update) {
    base.file.name = file.path(g$d$tmp, "prediction-gas-consumption-base.rData")

    if (!update && file.exists(base.file.name)) {
        return(readRDS(base.file.name))
    }

    # - LOAD -------------------------------------------------------------------
    # Temp & HDD
    d.temp = loadFromStorage(id = "temperature-hdd")[, `:=`(
        date = as.Date(date)
    )]

    # Gas Consumption
    d.consumption = loadFromStorage(id = "consumption-gas-aggm")[, .(
        date = as.Date(date), gas.consumption = value
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

    # Storage
    d.storage = loadFromStorage("storage-AT")[, .(
        date = as.Date(gasDayStart), storage = gasInStorage
    )]

    # Lockdowns
    d.lockdowns = loadFromStorage(id = "lockdowns")[, `:=`(
        date = as.Date(date),
        is.hard.lockdown = is.hard.lockdown > 0,
        is.lockdown = is.lockdown > 0
    )]

    # - MERGE ------------------------------------------------------------------
    d.comb = merge(d.consumption, d.temp, by = "date", all = TRUE)
    d.comb = merge(d.comb, d.holidays, by = "date", all = TRUE)
    d.comb = merge(d.comb, d.storage, by = "date", all = TRUE)

    d.comb = merge(d.comb, d.lockdowns, by = "date", all = TRUE)
    d.comb[is.na(is.hard.lockdown), is.hard.lockdown := FALSE]
    d.comb[is.na(is.lockdown), is.lockdown := FALSE]

    d.comb = merge(d.comb, d.power.gas, by = "date", all = TRUE)

    # - AUGMENT ----------------------------------------------------------------
    d.comb[, `:=`(
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

addTrend = function(d, threshold) {
    d[, `:=`(
        t = as.integer(date - min(date))
    )]

    d[, `:=`(
        t.squared = t^2
    )]
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

    dep.var = names(m.linear$model)[1]

    d.ret[, `:=`(
        difference = (get(dep.var) - prediction) / prediction,
        diff.pred.lower = (get(dep.var) - lower.pred) / lower.pred,
        diff.pred.upper = (get(dep.var) - upper.pred) / upper.pred,
        diff.conf.lower = (get(dep.var) - lower.conf) / lower.conf,
        diff.conf.upper = (get(dep.var) - upper.conf) / upper.conf
    )]

    d.ret[, `:=`(
        difference.mean = rollmean(difference, 14, fill = NA)
    )]

    d.plot = d.ret[, .(
        date, value = get(dep.var), prediction, difference,
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

cross.validation = function(model, d, train.years) {
    f = function(year) {
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

getSummaryTable = function(m, c.nice.names) {
    d = data.table(coef(summary(m)), keep.rownames = "term")
    d[, Variable := c.nice.names[term]]
    d[is.na(Variable), Variable := term]
    d$`Pr(>|t|)` = NULL
    d$term = NULL
    d$`t value` = NULL
    setcolorder(d, "Variable")
    kable(d, align = "r", digits = 3)
}

change.year = function(date, year) {
    new.date = copy(date)
    year(new.date) = year
    as.Date(new.date)
}





l.models = list(
    base.without.trend = gas.consumption ~
        #t + t.squared + # week +
        temp.below.threshold +
        temp.below.threshold.lag +
        temp.below.threshold.squared +
        wday + is.holiday + as.factor(is.vacation)
)




# year.select = 2000
# train.start.date = max.date
# prediction.start.date = l.gas.info$l.storage$d.domestic.last$date + 1

# TODO: Prediciton is always the same, maybe extract
prediction.consumption = function(
    year.select, d.temp, d.base,
    train.start.date, prediction.start.date = l.gas.info$l.storage$d.domestic.last$date + 1
) {
    d.train = d.base[(date > (train.start.date - l.options$learning.days - l.options$lag.max) & date <= train.start.date)]
    m.linear = lm(l.models$base.without.trend, data = d.train)

    # get temp of selected year
    # year.date.start = change.year(l.options$period$start, year.select)
    # year.date.end = year.date.start + l.options$period$length

    year.date.start = change.year(l.gas.info$l.storage$d.domestic.last$date, year.select)
    year.date.end = year.date.start + (l.options$period$end - prediction.start.date)

    d.temp.in = d.temp[date >= year.date.start - l.options$lag.max & date < year.date.end][, .(
        season = paste(unique(year(date)), collapse = "/"), date.org = date, 
        date = date + (prediction.start.date - year.date.start), temp.in = temp
    )]

    #  date + (l.options$period$start - year.date.start)

    # merge and predict
    d.prediction = merge(d.base, d.temp.in, by = "date")[, `:=`(
        temp = temp.in
    )]
    addTempThreshold(d.prediction, l.options$temp.threshold)
    d.prediction[, gas.consumption.pred := predict(m.linear, d.prediction)]

    # d.prediction[date >= l.options$period$start]
    d.prediction[date >= prediction.start.date]
}

one.prediction = function(year.select, d.temp, d.base, start.date) {
    d.prediction = prediction.consumption(year.select, d.temp, d.base, start.date)
    calculate.storage.level(d.prediction)
}

calculate.storage.level = function(d.prediction) {
    d.t = d.prediction[, .(
        date, season,
        cons.pred = gas.consumption.pred,
        src.russia = l.gas.info$c.sources.daily["russia"],
        src.other = l.gas.info$c.sources.daily["domestic"] + l.gas.info$c.sources.daily["others"]
    )]

    store = l.gas.info$l.storage$strategic + l.gas.info$l.storage$d.domestic.last$level

    d.t[, `:=`(
        store.with.russia = store - cumsum(cons.pred - src.russia - src.other),
        store.without.russia = store - cumsum(cons.pred - src.other),
        src.russia = NULL,
        src.other = NULL
    )]

    melt(d.t, id.vars = c("date", "season"))[]
}


reforecast.consumption.model = function(year, d.temp, d.base, start.date, max.date) {

    pred.from.start = one.prediction(year, d.temp, d.base, as.Date("2022-11-15")) %>%
        spread(variable, value)

    pred.from.end = one.prediction(year, d.temp, d.base, max.date, as.Date("2022-11-15")) %>%
        spread(variable, value)


    updating.period = seq(as.Date("2022-11-15"), max.date, by = 1)

    predict.one.day = function(year.select, d.temp, d.base, updating.period) {

        ret = prediction.consumption(year.select,
                                     d.temp,
                                     d.base,
                                     updating.period,
                                     updating.period)

        ret[[2]] = ret[[2]][1]

        return(ret)

    }

    pred.update.daily.list = mapply(
        predict.one.day, list(year), list(d.temp), list(d.base), updating.period,
        SIMPLIFY = FALSE
    )

    d.prediction.in = pred.update.daily.list[[1]][1] |> as.data.table()
    prediction.in = map_dbl(pred.update.daily.list, 2)

    pred.update.daily = calculate.storage.level(d.prediction.in, prediction.in, year) |>
        spread(variable, value)

    d.base %>%
        dplyr::select(date, gas.cons.obs = value) %>%
        merge(pred.from.start, by = "date") %>%
        merge(pred.update.daily, by = "date") %>%
        merge(pred.from.end, by = "date") %>%
        return()
}

get.d.loess = function(d.base) {
    d.base[, .(
        date, temp,
        value = value * 1000
    )]
}

get.d.lines = function(d.loess) {
    m.loess = loess(value ~ temp, d.loess)

    order.loess = order(d.loess$temp)

    d.lines = data.table(
        temp = d.loess$temp[order.loess],
        value = m.loess$fitted[order.loess]
    )
}

rmse = function(a, b) {
    tibble(a, b) |>
        na.omit() |>
        summarize(rmse = sqrt(mean((a - b)^2))) |>
        unlist()
}
