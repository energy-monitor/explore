# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")


# - DOIT -----------------------------------------------------------------------
d.plot = loadFromStorage(id = "nrg_cb_gasm")[,
    date := as.Date(date)
][year(date) >= 2014]

d.plot = d.plot[, .(
    date, product,
    value = mio.m3
)]

# Monthly values for the bar plot, there is no split by products
fwrite(
    monthlyStacked(d.plot, "total", "value"),
    file.path(g$d$wd, "gas", "supply-stacked.csv")
)

# the reference period of all fossil fuels, the first years with all series,
# also the CO₂ emissions of the totals (gas and oil start in 2014)
d.plot[, year := ifelse(year(date) %in% 2014:2018, "avg14-18", year(date)), by = .(date, product)]

d.plot = d.plot[, .(
    value = mean(value, na.rm = TRUE)
), by = .(
    year, product, date20 = {
        t = copy(date)
        year(t) = 2020
        t
    }
)]


# Save
fwrite(d.plot, file.path(g$d$wd, "gas", "supply.csv"))
# fwrite(d.plot[order(year, date20)], file.path(g$d$wd, 'others', 'supply-trans-prod.csv'))
