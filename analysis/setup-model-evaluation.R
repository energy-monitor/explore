pred.from.start = one.prediction(2022, d.hdd, d.base, as.Date("2022-11-15")) %>%
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


d.eval = d.base %>%
    dplyr::select(date, gas.cons.obs = value) %>%
    merge(pred.from.start, by = "date") %>%
    merge(pred.update.daily, by = "date")


