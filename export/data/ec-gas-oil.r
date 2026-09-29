# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")

d.base = loadFromStorage(id = "price-gas-oil")[,
    date := as.Date(date)
]


# - AUSTRIA --------------------------------------------------------------------
d.price = d.base[country == "AT", .(date, variable, value)]

# a month before the shown years, to carry the last price into the start
d.plot = d.price[date >= firstDateShown() - months(1)]

# Fill missing dates
d.plot = merge(
    d.plot,
    expand.grid(date = as.Date(min(d.plot$date):max(d.plot$date)), variable=unique(d.plot$variable)),
    by = c("date", "variable"), all = TRUE
)

d.plot[, last := na.locf(value), by=variable]

d.plot = d.plot[date >= firstDateShown()]
d.plot[, value := NULL]
dates2PlotDates(d.plot)


# Save
fwrite(d.plot, file.path(g$d$wd, "others", "gas-oil.csv"))


# - EUROPE ---------------------------------------------------------------------
# prices of the latest bulletin (europe map), countries only, no aggregates
d.plot = d.base[!country %in% c("EU", "EUR")]
d.plot = d.plot[date == max(date), .(country, date, variable, value)]

fwrite(d.plot[order(country, variable)], file.path(g$d$wd, "others", "gas-oil-europe.csv"))
