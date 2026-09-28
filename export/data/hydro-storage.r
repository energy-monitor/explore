# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")


# - LOAD/PREP ------------------------------------------------------------------
countries = c("AT")

d.base = loadFromStorage(id = "electricity-hydro-storage")[,
    date := as.Date(date)
]

for (c.country in countries) {
    # - PLOT -------------------------------------------------------------------
    # Preparation, weekly values in TWh
    d.plot = d.base[country == c.country, .(
        type = "stock",
        date,
        value
    )][order(date)]

    d.plot = rbind(d.plot, d.plot[, .(
        type = "flow",
        date,
        value = value - shift(value, 1)
    )])
    dates2PlotDates(d.plot)

    # Save
    fwrite(d.plot[year >= min(yearsShown())], file.path(g$d$wd, "electricity", glue("hydro-storage-{c.country}.csv")))
}
