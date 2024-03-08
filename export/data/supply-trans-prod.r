# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")


# - DOIT -----------------------------------------------------------------------
d.plot = loadFromStorage(id = "supply-trans-prod")[,
    date := as.Date(date)
]

dates2PlotDates(d.plot)


# Save
fwrite(d.plot, file.path(g$d$wd, 'others', 'supply-trans-prod.csv'))
# fwrite(d.plot[order(year, date20)], file.path(g$d$wd, 'others', 'supply-trans-prod.csv'))
