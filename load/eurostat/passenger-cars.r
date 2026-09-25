# - INIT -----------------------------------------------------------------------
rm(list = ls())
# no `load/eurostat/_shared.r`: the eurostat package is not on conda-forge and
# can not be built in the pixi environment, the SDMX-CSV endpoint is read directly
source("_shared.r")


# - DOIT -----------------------------------------------------------------------
# road_eqr_carpda: new passenger cars, road_eqs_carpda: passenger car stock
# PET = PET_X_HYB + ELC_PET_HYB + ELC_PET_PI (DIE likewise), hybrids exclude plug-ins
# ALT = ELC + GAS + LPG + HYD_FCELL + REN_OTH + OTH
# TOTAL = PET + DIE + ALT
for (id in c("road_eqr_carpda", "road_eqs_carpda")) {
    update.time = now()
    d.base = fread(glue(
        "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/{id}/?format=SDMX-CSV"
    ))[freq == "A" & unit == "NR"]

    # stock is additionally split by owner
    if ("leg_form" %in% names(d.base))
        d.base = d.base[leg_form == "TOTAL"]

    d.wide = dcast(d.base, geo + TIME_PERIOD ~ mot_nrg, value.var = "OBS_VALUE")

    d.prep = melt(d.wide[, .(
        geo,
        year = TIME_PERIOD,
        petrol = PET_X_HYB,
        diesel = DIE_X_HYB,
        hybrid = ELC_PET_HYB + ELC_DIE_HYB,
        phev = ELC_PET_PI + ELC_DIE_PI,
        bev = ELC,
        other = ALT - ELC,
        total = TOTAL
    )], id.vars = c("geo", "year"), variable.name = "type", value.name = "cars")

    saveToStorages(d.prep[!is.na(cars)], list(
        id = id,
        source = "eurostat",
        format = "csv",
        update.time = update.time
    ))
}
