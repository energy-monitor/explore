# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/web-monitor/_shared.r")


# - LOAD -----------------------------------------------------------------------
d.price = loadFromStorage(id = "price-gas-oil")[, 
    date := as.Date(date)
]


# - PLOT -----------------------------------------------------------------------
d.plot = d.price[date > "2018-12-01"]


# Fill missing dates
d.plot = merge(
    d.plot,
    expand.grid(date = as.Date(min(d.plot$date):max(d.plot$date)), variable=unique(d.plot$variable)),
    by=c('date', 'variable'), all = TRUE
)

d.plot[, last := na.locf(value), by=variable]

d.plot = d.plot[date >= "2019-01-01"]
d.plot[, value := NULL]
dates2PlotDates(d.plot)
# d.plot[]

# - SAVE -----------------------------------------------------------------------
fwrite(d.plot, file.path(g$d$wd, 'others', 'gas-oil.csv'))
