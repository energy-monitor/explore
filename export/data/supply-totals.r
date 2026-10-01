# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")


# - DOIT -----------------------------------------------------------------------
d.oil = loadFromStorage(id = "nrg_cb_oilm")[,
    date := as.Date(date)
]
d.coal = loadFromStorage(id = "nrg_cb_sffm")[,
    date := as.Date(date)
]
d.gas = loadFromStorage(id = "nrg_cb_gasm")[,
    date := as.Date(date)
]

# Energy content of the sales
d.twh = rbindlist(list(
    d.oil[product == "total", .(date, product = "oil", t.j)],
    d.coal[product == "total", .(date, product = "coal", t.j)],
    d.gas[product == "total", .(date, product = "gas", t.j)]
))

# Emissions calibrated to the national inventory, see calc/emissions.r
d.co2 = loadFromStorage(id = "emissions-fuels")[fuel %in% c("oil", "coal", "gas"), .(date = as.Date(date), product = fuel, t.co2 = kt.co2 * 1000)]

d.plot = merge(d.twh, d.co2, by = c("date", "product"), all = TRUE)

# Monthly values for the stacked plot
d.monthly = monthlyStacked(d.plot[, .(
    date, product,
    twh = t.j / 1000 / 3.6,
    mt.co2 = t.co2 / 1000 / 1000
)], c("oil", "coal", "gas"), c("twh", "mt.co2"))
fwrite(d.monthly, file.path(g$d$wd, "fossil", "supply-stacked.csv"))

d.plot = rbind(
    d.plot,
    d.plot[, if (.N == 3) .(product = "total", t.j = sum(t.j), t.co2 = sum(t.co2)), by = date]
)


# emissions of oil are available from 2014 on
d.plot = d.plot[year(date) >= 2014]
d.plot[, year := ifelse(year(date) %in% 2014:2018, "avg14-18", as.character(year(date)))]

d.plot = d.plot[, .(
    twh = mean(t.j, na.rm = TRUE) / 1000 / 3.6,
    mt.co2 = mean(t.co2, na.rm = TRUE) / 1000 / 1000
), by = .(
    year, product, date20 = {
        t = copy(date)
        year(t) = 2020
        t
    }
)]


# Save
fwrite(d.plot, file.path(g$d$wd, "fossil", "supply.csv"))
