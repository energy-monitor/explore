# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")


l.default = list(
    value = "value",
    date = "date",
    others = character(0),
    lags = 7,
    cum = FALSE
)

l.plots = list(
    "gas/price" = list(data = "price-gas", value = "price"),
    "electricity/price-entsoe" = list(data = "electricity-price-entsoe", value =  c("mean", "min", "max")),
    "electricity/price" = list(data = "price-electricity", value = c("base", "peak")),
    # "electricity/price-hourly" = list(data = "price-electricity-hourly"),
    "others/brent" = list(data = "price-brent", value = "price"),
    "others/coal" = list(data = "price-coal", value = "price"),
    "others/dollar" = list(data = "price-dollar", value = "price"),
    "others/eua" = list(data = "price-eua"),
    "others/hdd" = list(data = "temperature-hdd", value = "hdd", lags = 28, cum = TRUE)
)


id = "electricity/price-entsoe"
# id = "electricity/price"
invisible(lapply(names(l.plots), function(id) {
    l('-> ', id, iL = 2);

    def = modifyList(l.default, l.plots[[id]])

    # Load
    d.raw = loadFromStorage(id = def$data)
    d.base = d.raw[,
        c(def$date, def$others, def$value), with = FALSE
    ][, date := as.Date(date)][]

    d.plot = melt(d.base, id.vars = "date")[order(date), ]
    d.plot = d.plot[date >= "2018-01-01"]

    # Fill missing dates
    d.plot = merge(
        d.plot,
        expand.grid(
            date = as.Date(min(d.plot$date):max(d.plot$date)),
            variable = unique(d.plot$variable)
        ),
        by=c('date', 'variable'), all = TRUE
    )

    c.vars = "rm"
    if (def$cum) {
        addCum(d.plot, g = 'variable')
        c.vars = c(c.vars, "cum")
    }

    d.plot[, rm := rollmean(
        value, def$lags, fill = NA, align = "right", na.rm = TRUE
    ), by=variable]

    d.plot = d.plot[date >= "2019-01-01" & !is.na(rm)]

    setnames(d.plot, "variable", "type")
    d.plot = melt(d.plot, id.vars = c("date", "type"), measure.vars = c.vars)[!is.na(value) & date >= "2019-01-01"]

    dates2PlotDates(d.plot)

    if (length(unique(d.plot$type)) == 1)
        d.plot[, type := NULL]

    if (length(unique(d.plot$variable)) == 1)
        d.plot[, variable := NULL]

    fwrite(d.plot, file.path(g$d$wd, glue("{id}.csv")))
    NULL
}))

# # - PLOT -----------------------------------------------------------------------
# # Prepare
# d.m[, year := year(date)]

# d.plot = d.m[year >= 2019, .(
#     value = mean(value)
# ), by=.(year, hour)][order(year, hour)]

# # Save
# fwrite(d.plot, file.path(g$d$wd, "electricity", "price-hourly.csv"))
