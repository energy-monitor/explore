# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")

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

# Save
fwrite(d.plot[year >= min(yearsShown())], file.path(g$d$wd, "electricity", glue("flows.csv")))
