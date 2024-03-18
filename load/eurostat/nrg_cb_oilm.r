# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/eurostat/_shared.r")


# - DOIT -----------------------------------------------------------------------
update.time = now()
d.base = as.data.table(
    get_eurostat("nrg_cb_oilm")
)[geo == "AT" & nrg_bal == "GID_CAL" & unit == "THS_T" & freq == "M"]


d.wide = dcast(d.base, TIME_PERIOD ~ siec)

# Mineralölprodukte	[O4652XR5210B]+[O4661XR5230B]+[O46711]-[R5220B]+[O46712]
# Motorenbenzin (ohne Biokraftstoffanteil)	[O4652XR5210B]
# Flugturbinenkraftstoff auf Petroleumbasis (ohne Biokraftstoffanteil)	[O4661XR5230B]
# Kraftfahrzeug-Diesel (ohne Biokraftstoffanteil)	[O46711]-[R5220B]
# Heizöl und sonstiges Gasöl	[O46712]
d.prep = melt(d.wide[, .(
    date = TIME_PERIOD,
    total.fossil = O4652XR5210B + O4661XR5230B + O46711 - R5220B + O46712,
    gasoline.fossil = O4652XR5210B,
    kerosin.fossil = O4661XR5230B,
    diesel.fossil = O46711 - R5220B,
    heating.fossil = O46712
)], id.vars = "date", variable.name = "product", value.name = "value")


saveToStorages(d.prep[!is.na(value)], list(
    id = "nrg_cb_oilm",
    source = "eurostat",
    format = "csv",
    update.time = update.time
))#, storages = "local")
