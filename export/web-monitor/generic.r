# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/web-monitor/_shared.r")


files = list(
    "gas/price" = list(data = "price-gas"),
    "electricity/price" = list(data = "price-electricity"),
    # "electricity/price-hourly" = list(data = "price-electricity-hourly"),
    "others/brent" = list(data = "price-brent"),
    "others/coal" = list(data = "price-coal"),
    "others/dollar" = list(data = "price-dollar"),
    "others/eua" = list(data = "price-eua")
)

lags = 7

# file = "electricity/price"
invisible(lapply(names(files), function(file) {

    def = files[[file]]

    # Load
    d.price = loadFromStorage(id = def$data)[, 
        date := as.Date(date)
    ]

    d.plot = melt(d.price, id.vars = "date")[order(date), ]
    d.plot = d.plot[date > "2018-12-01"]

    # Fill missing dates
    d.plot = merge(
        d.plot,
        expand.grid(
            date = as.Date(min(d.plot$date):max(d.plot$date)),
            variable = unique(d.plot$variable)
        ),
        by=c('date', 'variable'), all = TRUE
    )

    vl = glue("rm{lags}")
    d.plot[, (vl) := rollmean(value, lags, fill = NA, align = "right", na.rm = TRUE), by=variable]

    d.plot = d.plot[date >= "2019-01-01" & !is.na(get(vl))]
    d.plot[, value := NULL]

    dates2PlotDates(d.plot)

    fwrite(d.plot, file.path(g$d$wd, glue("{file}.csv")))
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
