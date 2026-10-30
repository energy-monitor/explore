# Years shown in the charts comparing years: the current and the 7 before
# (the colours in web/data/_years.json are rotated by export/data/years.r)
yearsShown = function() (year(Sys.Date()) - 7):year(Sys.Date())
firstDateShown = function() as.Date(paste0(min(yearsShown()), "-01-01"))

# ISO 3166 codes of the countries, as the ids of web/assets/geo/europe.json,
# eurostat and ENTSO-E use EL for Greece and UK for the United Kingdom
iso2 = function(x) {
    c.map = c(EL = "GR", UK = "GB")
    ifelse(x %in% names(c.map), c.map[x], x)
}

# The values with 6 significant digits, the integer part is kept, e.g. 164.732
# of 164.732471428571, 0.00199541 and 1234568, the charts show at most 2
# decimals (or a share in .1%), the full precision makes the files a fifth larger
roundSignif = function(x, s = 6) {
    d = s - 1 - floor(log10(abs(x)))
    d[!is.finite(d) | d < 0] = 0
    round(x, d)
}

# The CSV of a chart for the web, the numbers rounded by roundSignif, the
# dates (and other classes) are kept, the data is not changed
fwriteWeb = function(d, file, ...) {
    cols = names(d)[sapply(d, function(x) is.double(x) && !is.object(x))]
    d = copy(as.data.table(d))[, (cols) := lapply(.SD, roundSignif), .SDcols = cols]
    fwrite(d, file, ...)
}

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

# Monthly values for the stacked plots: only months with all products and
# without missing values, with the shares of the months as `<col>.share`
# (the order of the stacks is set by the definition of the plot)
monthlyStacked = function(d, products, cols) {
    d = d[product %in% products & date >= firstDateShown()]
    d = d[, if (.N == length(products) && !anyNA(.SD)) .SD, by = date, .SDcols = c("product", cols)]
    d[, (paste0(cols, ".share")) := lapply(.SD, function(v) v / sum(v)), by = date, .SDcols = cols]
    d[order(date, match(product, products))]
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
