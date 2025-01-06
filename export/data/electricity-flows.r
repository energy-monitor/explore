# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")
loadPackages(c(
    "ggplot2"
))
# - LOAD/PREP ------------------------------------------------------------------
d.base = loadFromStorage(id = glue("physical-flows-entsoe"))


# - PLOT -------------------------------------------------------------------
# Preparation


# d.base[, .(exports = sum(exports)), by = .(iso2, year = year(date))]
# d.month = d.base[, .(exports = sum(exports)), by = .(iso2, year = glue("{year(date)}-{month(date)}"))]


# d.day.tot = d.base[, .(exports = sum(exports)), by = .(date)]



# plot(d.day.tot)

# View(d.month)


# d.plot = d.base[, .(
#     type = "stock",
#     date = gasDayStart,
#     value = gasInStorage
# )][order(date)]

d.plot = d.base[, .(value = sum(exports)), by = .(date)][order(date)]


d.plot[, value := rollmean(
    value, 14, fill = NA, align = "right", na.rm = TRUE
)]

dates2PlotDates(d.plot)

ggplot(d.plot[date > "2023-11-01" & date < "2024-11-01"], aes(x = date, y = value)) +
    geom_line()


# Save
fwrite(d.plot[year >= 2019], file.path(g$d$wd, "electricity", glue("flows.csv")))
