# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/eurostat/_shared.r")


# - DOIT -----------------------------------------------------------------------
# rail transport of all railway undertakings, collected by the national
# statistical offices (in Austria by Statistik Austria, incl. the private ones,
# without metros, trams and tourist railways)
# - rail_go_quartal: goods, quarterly, since 2004, the date is the first day of
#   the quarter
# - rail_pa_total: passengers, annual only, the quarterly data of Austria
#   (rail_pa_quartal) ends in 2009 as it is confidential
# thousand tonnes or passengers (THS_T, THS_PAS) and million tonne- or
# passenger-km (MIO_TKM, MIO_PKM)
c.series = list(
    rail_go_quartal = list(freq = "Q", units = c(THS_T = "ths.t", MIO_TKM = "mio.tkm")),
    rail_pa_total = list(freq = "A", units = c(THS_PAS = "ths.pas", MIO_PKM = "mio.pkm"))
)

for (id in names(c.series)) {
    s = c.series[[id]]
    update.time = now()
    d.base = as.data.table(
        get_eurostat(id)
    )[freq == s$freq & unit %in% names(s$units)]
    d.base[, unit := s$units[unit]]

    d.prep = dcast(d.base, geo + TIME_PERIOD ~ unit, value.var = "values")
    setnames(d.prep, "TIME_PERIOD", "date")
    if (s$freq == "A") {
        d.prep[, `:=`(year = year(date), date = NULL)]
        setcolorder(d.prep, c("geo", "year"))
    }

    saveToStorages(d.prep[rowSums(!is.na(d.prep[, s$units, with = FALSE])) > 0], list(
        id = id,
        source = "eurostat",
        format = "csv",
        update.time = update.time
    ))
}
