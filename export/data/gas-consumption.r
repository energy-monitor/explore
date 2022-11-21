# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")


# - LOAD/PREP ------------------------------------------------------------------
d.plot = loadFromStorage(id = "consumption-gas-aggm")[, 
    date := as.Date(date)
]


# - PLOT -----------------------------------------------------------------------

# Plot, Preparation
addRollMean(d.plot, 7)
addCum(d.plot)
d.plot = meltAndRemove(d.plot)
dates2PlotDates(d.plot)

# Save plot data
fwrite(d.plot, file.path(g$d$wd, 'gas', 'consumption-aggm.csv'))

