# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/eurostat/_shared.r")


# - DOIT -----------------------------------------------------------------------
update.time = now()
d.base = as.data.table(
    get_eurostat("nrg_cb_gasm")
)[geo == "AT" & nrg_bal == "IC_CAL_MG" & unit == "MIO_M3" & freq == "M"]


d.wide = dcast(d.base, TIME_PERIOD ~ siec)

# Erdgas	[G3000]
d.prep = melt(d.wide[, .(
    date = TIME_PERIOD,
    total = G3000
)], id.vars = "date", variable.name = "product", value.name = "mio.m3")

d.prep[, t.j := mio.m3 * 1000 * 0.0372276]
d.prep[, t.co2 := t.j * 55.59]

saveToStorages(d.prep[!is.na(mio.m3)], list(
    id = "nrg_cb_gasm",
    source = "eurostat",
    format = "csv",
    update.time = update.time
))#, storages = "local")
