# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/eurostat/_shared.r")
# previously the SDMX-CSV endpoint was read directly, see commented code below
# source("_shared.r")


# - DOIT -----------------------------------------------------------------------
# HICP according to ECOICOP ver. 2, replaces prc_hicp_midx (discontinued with 2025-12)
c.items = c(
    TOTAL = "total",
    NRG = "energy",
    CP0451 = "electricity",
    CP0452 = "gas",
    CP0453 = "heating.oil",
    # coal, wood and pellets
    CP0454 = "solid.fuels",
    # mostly district heating
    CP0455 = "heat",
    CP07221 = "diesel",
    CP07222 = "petrol"
)

update.time = now()
# the full dataset has >20m rows, with filters the JSON API is used instead of
# the bulk download, which names the time column `time` instead of `TIME_PERIOD`
d.base = as.data.table(get_eurostat("prc_hicp_minr", filters = list(
    freq = "M",
    unit = c("I25", "RCH_A"),
    coicop18 = names(c.items)
)))
# d.base = fread(glue(
#     "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/prc_hicp_minr/",
#     "M.I25+RCH_A.{paste(names(c.items), collapse = '+')}./?format=SDMX-CSV"
# ))

# index: 2025 = 100, rate: annual rate of change in %
d.wide = dcast(d.base, geo + time + coicop18 ~ unit, value.var = "values")
# d.wide = dcast(d.base, geo + TIME_PERIOD + coicop18 ~ unit, value.var = "OBS_VALUE")

d.prep = d.wide[, .(
    geo,
    date = time,
    # date = as.Date(paste0(TIME_PERIOD, "-01")),
    item = factor(c.items[coicop18], levels = c.items),
    index = I25,
    rate = RCH_A
)][!is.na(index) | !is.na(rate)]

saveToStorages(d.prep[order(geo, item, date)], list(
    id = "prc_hicp_minr",
    source = "eurostat",
    format = "csv",
    update.time = update.time
))
