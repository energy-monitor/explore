# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")


# - DOIT -----------------------------------------------------------------------
d.plot = loadFromStorage(id = "nrg_cb_sffm")[,
    date := as.Date(date)
][year(date) >= 2013]

d.plot = d.plot[, .(
    date, product,
    value = ths.t
)]

d.plot[, year := ifelse(year(date) %in% 2013:2018, "avg13-18", year(date)), by=.(date, product)]

d.plot = d.plot[, .(
    value = mean(value, na.rm = TRUE)
), by = .(
    year, product, date20 = {t = copy(date); year(t) = 2020; t}
)]


# Save
fwrite(d.plot, file.path(g$d$wd, "others", "supply-coal.csv"))
# fwrite(d.plot[order(year, date20)], file.path(g$d$wd, 'others', 'supply-trans-prod.csv'))
