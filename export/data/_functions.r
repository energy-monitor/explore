# Years shown in the charts comparing years: the current and the 7 before
# (the colours in web/data/shared.json are rotated by export/data/years.r)
yearsShown = function() (year(Sys.Date()) - 7):year(Sys.Date())
firstDateShown = function() as.Date(paste0(min(yearsShown()), "-01-01"))

addRollMean = function(d, l, g = character(0)) {
    d[, (paste0('rm', l)) := rollmean(value, l, fill = NA, align = "right"), by=c(g)]
}

addCum = function(d, g = character(0)) {
    d[, year := year(date)]
    d[, cum := cumsum(value), by=c("year", g)]
    d[, year := NULL]
}

meltAndRemove = function(d, g = character(0)) {
    melt(d, id.vars = c("date", g))[!is.na(value) & date >= firstDateShown()]
}

dates2PlotDates = function(d) {
    c.date20 = copy(d$date)
    year(c.date20) = 2020

    d[, `:=`(
        # day = yday(date),
        year = year(date),
        date20 = c.date20
    )]
}
