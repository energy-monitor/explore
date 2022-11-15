# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/web-monitor/_shared.r")

nameOthers = "others"

# - LOAD -----------------------------------------------------------------------
d.base = loadFromStorage(id = "electricity-generation-g2")[,
    date := as.Date(date)
][]

# - PREP -----------------------------------------------------------------------
d.agg = d.base[, .(value = sum(value)), by=.(country, year = year(date), type = source.group)]

d.agg[, share := value/sum(value), by=.(country, year)]

d.plot = d.agg[year >= 2019 & !is.na(share)][order(country, year)]

fwrite(d.plot, file.path(g$d$wd, "electricity", "generation-int-map.csv"))
