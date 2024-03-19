# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/eurostat/_shared.r")


# - DOIT -----------------------------------------------------------------------
update.time = now()
d.base = as.data.table(
    get_eurostat("nrg_cb_sffm")
)[geo == "AT" & nrg_bal == "GID_CAL" & unit == "THS_T" & freq == "M"]


# Kohleprodukte	[C0100][C0311][C0200]
# Steinkohle	[C0100] 0.029894732
# Braunkohle	[C0200] 0.021523706
# Kokereikoks	[C0311] 0.0282156
# Torf	[P1100]
# Ölschiefer und bituminöse Sande	[S2000]
c.types = list(
    stein = list(
        code = "C0100",
        tj.per.t = 0.029894732,
        tco2.per.tj = 93.53
    ),
    braun = list(
        code = "C0200",
        tj.per.t = 0.021523706,
        tco2.per.tj = 97.21
    ),
    koks = list(
        code = "C0311",
        tj.per.t = 0.0282156,
        tco2.per.tj = 112.28
    )
)


d.prep = rbindlist(lapply(names(c.types), function(n) {
    t = c.types[[n]]
    d.base[siec == t$code, .(
        date = TIME_PERIOD,
        product = n,
        ths.t = values,
        t.j = values * t$tj.per.t * 1000,
        t.co2 = values * t$tj.per.t * 1000 * t$tco2.per.tj
    )]
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
    id = "nrg_cb_sffm",
    source = "eurostat",
    format = "csv",
    update.time = update.time
))#, storages = "local")
