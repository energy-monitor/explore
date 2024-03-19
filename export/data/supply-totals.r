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

d.plot = rbindlist(list(
    d.oil[product == "total", .(date, product = "oil", t.j, t.co2)],
    d.coal[product == "total", .(date, product = "coal", t.j, t.co2)],
    d.gas[product == "total", .(date, product = "gas", t.j, t.co2)]
))


d.plot[, year := ifelse(year(date) %in% 2013:2018, "avg13-18", year(date)), by=.(date, product)]

d.plot = d.plot[, .(
    twh = mean(t.j, na.rm = TRUE) / 1000 / 3.6,
    mt.co2 = mean(t.co2, na.rm = TRUE) / 1000 / 1000
), by = .(
    year, product, date20 = {t = copy(date); year(t) = 2020; t}
)]


# Save
fwrite(d.plot, file.path(g$d$wd, "others", "supply-total.csv"))
