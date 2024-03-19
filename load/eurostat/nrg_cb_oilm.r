# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/eurostat/_shared.r")


# - DOIT -----------------------------------------------------------------------
update.time = now()
d.base = as.data.table(
    get_eurostat("nrg_cb_oilm")
)[geo == "AT" & nrg_bal == "GID_CAL" & unit == "THS_T" & freq == "M"]


d.wide = dcast(d.base, TIME_PERIOD ~ siec)


# Benzin	 73.70 		0.041544527     O4652XR5210B
# Diesel	 73.70 		0.04237         O46711 - R5220B
# Heizöl	 75.00 		0.04337         O46712
# Kerosin	 73.70 		0.043300121     O4661XR5230B

c.types = list(
    gasoline = list(
        tj.per.t = 0.041544527,
        tco2.per.tj = 73.70
    ),
    diesel = list(
        tj.per.t = 0.04237,
        tco2.per.tj = 73.70
    ),
    heating = list(
        tj.per.t = 0.04337,
        tco2.per.tj = 75.00
    ),
    kerosin = list(
        tj.per.t = 0.043300121,
        tco2.per.tj = 73.70
    )
)

d.prep = melt(d.wide[, .(
    date = TIME_PERIOD,
    gasoline = O4652XR5210B,
    kerosin = O4661XR5230B,
    diesel = O46711 - R5220B,
    heating = O46712
)], id.vars = "date", variable.name = "product", value.name = "ths.t")

invisible(sapply(names(c.types), function(n) {
    t = c.types[[n]]
    d.prep[product == n, t.j := ths.t * t$tj.per.t * 1000]
    d.prep[product == n, t.co2 := t.j * t$tco2.per.tj]
}))

d.prep = rbind(
    d.prep[, .(
        product = "total",
        ths.t = sum(ths.t),
        t.j = sum(t.j),
        t.co2 = sum(t.co2)
    ), by = .(date)],
    d.prep
)

saveToStorages(d.prep[!is.na(ths.t)], list(
    id = "nrg_cb_oilm",
    source = "eurostat",
    format = "csv",
    update.time = update.time
))#, storages = "local")
