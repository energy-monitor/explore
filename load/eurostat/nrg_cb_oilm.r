# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/eurostat/_shared.r")


# - DOIT -----------------------------------------------------------------------
update.time = now()
d.base = as.data.table(
    get_eurostat("nrg_cb_oilm")
)[geo == "AT" & TIME_PERIOD >= "2019-01-01" & nrg_bal == "GID_OBS"]

d.base.f = d.base[siec %in% c("O4652", "O46711"), .(
    date = TIME_PERIOD,
    product = siec,
    value = values
)]

saveToStorages(d.base.f, list(
    id = "supply-trans-prod",
    source = "eurostat",
    format = "csv",
    update.time = update.time
))#, storages = "local")
