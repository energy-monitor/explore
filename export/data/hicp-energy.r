# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")


# - LOAD/PREP ------------------------------------------------------------------
# consumer prices of energy items, index (2025 = 100) and annual rate of change (%)
countries = c(AT = "AT", EU = "EU27_2020")

d.base = loadFromStorage(id = "prc_hicp_minr")[,
    date := as.Date(date)
]

for (c.country in names(countries)) {
    d.plot = d.base[geo == countries[[c.country]], .(
        date,
        type = item,
        index,
        rate
    )]
    dates2PlotDates(d.plot)

    # Save
    fwrite(d.plot[year >= 2019][order(type, date)], file.path(g$d$wd, "others", glue("hicp-energy-{c.country}.csv")))
}
