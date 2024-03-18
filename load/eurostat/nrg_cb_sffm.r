# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/eurostat/_shared.r")


# - DOIT -----------------------------------------------------------------------
update.time = now()
d.base = as.data.table(
    get_eurostat("nrg_cb_sffm")
)[geo == "AT" & nrg_bal == "GID_CAL" & unit == "THS_T" & freq == "M"]


d.wide = dcast(d.base, TIME_PERIOD ~ siec)

# Kohleprodukte	[C0100][C0311][C0200]
# Steinkohle	[C0100]
# Braunkohle	[C0200]
# Kokereikoks	[C0311]
# Torf	[P1100]
# Ölschiefer und bituminöse Sande	[S2000]
d.prep = melt(d.wide[, .(
    date = TIME_PERIOD,
    total = C0100 + C0311 + C0200,
    stein = C0100,
    braun = C0311,
    koks = C0200
)], id.vars = "date", variable.name = "product", value.name = "value")


saveToStorages(d.prep[!is.na(value)], list(
    id = "nrg_cb_sffm",
    source = "eurostat",
    format = "csv",
    update.time = update.time
))#, storages = "local")
