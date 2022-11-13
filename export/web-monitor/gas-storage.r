# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/web-monitor/_shared.r")


# - LOAD/PREP ------------------------------------------------------------------
countries = c("AT", "EU")

for (country in countries) {
    d.plot = loadFromStorage(id = glue("storage-{country}"))[, 
        gasDayStart := as.Date(gasDayStart)
    ]

    # - PLOT -------------------------------------------------------------------
    # Preparation
    d.plot = d.base[, .(
        type = "stock",
        date = gasDayStart,
        value = gasInStorage
    )][order(date)]

    d.plot = rbind(d.plot, d.plot[, .(
        type = "flow",
        date,
        value = value - shift(value, 7)
    )])
    dates2PlotDates(d.plot)

    # Save
    fwrite(d.plot[year >= 2019], file.path(g$d$wd, "gas", glue("storage-{country}.csv")))
}
