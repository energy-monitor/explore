# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("calc/prediction-gas-consumption/_shared.r")
source("export/web-monitor/_shared.r")

library(tidyverse)


# - CONF -----------------------------------------------------------------------
update.data = FALSE


# - LOAD -----------------------------------------------------------------------
d.base = loadBase(update.data)
d.economic.activity = loadFromStorage(id = "economic-activity")[, .(
    year, month, economic.activity = value
)]


d.comb = merge(d.base, d.economic.activity, by = c("year", "month"), all.x = TRUE)

rel.growth = growth.rate(d.economic.activity)

d.comb = d.comb[, economic.activity.estimate := ifelse(is.na(economic.activity), "Naiver Schätzer", "Statistik\nAustria"), ]
d.comb = d.comb[, economic.activity := ifelse(is.na(economic.activity), rel.growth * lag(economic.activity, 365), economic.activity), ]

# d.comb[, `:=`(
#     temp = temp.vienna,
#     hdd = hdd.vienna
# )]


# AUGMENT
# package clock gives weekdays in English, independent of system locale
d.comb[, `:=`(
    t = as.integer(date - min(date)),
    year = year(date),
    day = yday(date),
    week = week(date),
    month = month(date),
    wday = factor(as.character(clock::date_weekday_factor(date)),
        c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    ),
    temp.15 = ifelse(temp < 15, 15 - temp, 0),
    temp.squared  = temp^2,
    temp.lag = shift(temp, 1),
    # AR TERMS, ONLY TESTING
    value.lag = shift(value, 1),
    value.lag2 = shift(value, 2)
)]

d.comb[, `:=`(
    t.squared = t^2,
    workday = ifelse(wday %in% c("Sat", "Sun"), wday, "Working day"),
    week = str_pad(week, 2, pad = "0"),
    temp.15.squared = temp.15^2,
    temp.15.lag = shift(temp.15, 1),
    year_character = as.character(year),
    # temp.f = cut(temp, quantile(temp, 0:20/20), all.inside = TRUE),
    temp.f = cut(temp, seq(min(temp), max(temp), length = 10), all.inside = TRUE)
)]



train.years = c(2015:2021)

d.train = d.comb[year %in% train.years]


estimate = function(model, type="non-economic"){
    d.ret = copy(d.comb)

    m.linear = lm(
        model
        , data = d.train)

    summary(m.linear)

    prediction.prediction = predict(m.linear, d.ret, interval = "prediction", level = 0.95) %>%
        as.data.table()

    prediction.confidence = predict(m.linear, d.ret, interval = "confidence", level = 0.95) %>%
        as.data.table()

    d.ret[, `:=`(prediction = prediction.prediction$fit,
                  lower.pred = prediction.prediction$lwr,
                  upper.pred = prediction.prediction$upr,
                  lower.conf = prediction.confidence$lwr,
                  upper.conf = prediction.confidence$upr), ]

    d.ret[, `:=` (difference = (value - prediction) / prediction,
                   diff.pred.lower = (value - lower.pred) / lower.pred,
                   diff.pred.upper = (value - upper.pred) / upper.pred,
                   diff.conf.lower = (value - lower.conf) / lower.conf,
                   diff.conf.upper = (value - upper.conf) / upper.conf,
                   model.type = type
                   ), ]

    return(d.ret)
}

model = value ~
    # value.lag + value.lag2 + # AR TERMS
    temp + temp.squared + temp.15 + temp.15.lag + temp.15.squared +
    # temp * as.factor(temp.f) +
    wday + is.holiday + as.factor(vacation.name)

d.pred.non.economic = estimate(model)

model.economic = value ~
    # value.lag + value.lag2 + # AR TERMS
    temp + temp.squared + temp.15 + temp.15.lag + temp.15.squared +
    # temp * as.factor(temp.f) +
    wday + is.holiday + as.factor(vacation.name) +
    economic.activity

d.pred.economic = estimate(model.economic, type = "economic")

d.pred = bind_rows(d.pred.non.economic, d.pred.economic)

#####example figures
d.pred %>%
    filter(model.type == "non-economic") %>%
    dplyr::select(date, difference, diff.conf.lower, diff.conf.upper) %>%
    filter(year(date) > 2021) %>%
    gather(variable, value, -date) %>%
    filter(variable %in% c("difference", "diff.conf.lower", "diff.conf.upper")) %>%
    group_by(variable) %>%
    mutate(value = rollmean(100 * value, 30, fill = NA)) %>%
    ungroup() %>%
    spread(variable, value) %>%
    ggplot(aes(x = date, y = difference)) +
    geom_abline(slope = 0, intercept = 0, size = 0.5, linetype = 2) +
        geom_ribbon(aes(ymin = diff.conf.lower, ymax = diff.conf.upper), alpha=0.3) +
        geom_line(size = 1) +
    theme_bw(base_size = 12) +
    xlab("Datum") +
    ylab("Relative Differenz zwischen \nSchätzung und Observation\n(%)") +
    scale_colour_manual(values=c("red", "black")) +
    scale_fill_manual(values=c("red", "black")) +
    scale_x_date(date_labels = "%b",date_breaks  ="1 month") +
    scale_y_continuous(n.breaks = 10)

d.pred %>%
    filter(model.type == "economic") %>%
    dplyr::select(date, difference, diff.conf.lower, diff.conf.upper, `Industrieproduktion` = economic.activity.estimate) %>%
    filter(year(date) > 2021) %>%
    gather(variable, value, -date, -`Industrieproduktion`) %>%
    filter(variable %in% c("difference", "diff.conf.lower", "diff.conf.upper")) %>%
    group_by(variable) %>%
    mutate(value = rollmean(100 * value, 30, fill = NA)) %>%
    ungroup() %>%
    spread(variable, value) %>%
    ggplot(aes(x = date, y = difference)) +
    geom_abline(slope = 0, intercept = 0, size = 0.5, linetype = 2) +
    #geom_ribbon(aes(ymin = diff.pred.lower, ymax = diff.pred.upper), alpha=0.1) +
    geom_ribbon(aes(ymin = diff.conf.lower, ymax = diff.conf.upper, fill = Industrieproduktion), alpha=0.3) +
    geom_line(size = 1, aes(col = Industrieproduktion)) +
    theme_bw(base_size = 12) +
    xlab("Datum") +
    ylab("Relative Differenz zwischen \nSchätzung und Observation\n(%)") +
    scale_colour_manual(values=c("red", "black")) +
    scale_fill_manual(values=c("red", "black")) +
    scale_x_date(date_labels = "%b",date_breaks  ="1 month") +
    scale_y_continuous(n.breaks = 10)



# - OUTPUT ---------------------------------------------------------------------
 d.all = melt(d.pred, variable.name = "type",
              id.vars = c("date", "model.type"), measure.vars = c("value", "prediction", "lower.pred", "upper.pred", "lower.conf", "upper.conf")
 )

# # PREP FOR PLOT
addRollMean(d.all, 7, "type")
addCum(d.all, "type")
d.plot <- melt(d.all, id.vars = c("date", "type", "model.type"))[!is.na(value)]
dates2PlotDates(d.plot)

# # SAVE
fwrite(d.plot[date >= "2019-01-01"], file.path(g$d$wd, "pred-gas-cons.csv"))
