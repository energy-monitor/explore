# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/eurostat/_shared.r")

# Inputs for the CO₂ emission estimates (export/data/_emissions.r), following
# the methodology of https://github.com/ElijahStaengl/co2_fuel_combustion_final


# - GAS ------------------------------------------------------------------------
# Inland consumption (calculated as defined in MOS GAS) of natural gas [G3000]
update.time = now()
d.gas = as.data.table(
    get_eurostat("nrg_cb_gasm", filters = list(geo = "AT", freq = "M"), time_format = "date")
)[nrg_bal == "IC_CAL_MG" & siec == "G3000" & unit == "TJ_GCV" & !is.na(values), .(
    date = time,
    product = "gas",
    tj.gcv = values
)]

saveToStorages(d.gas[order(date)], list(
    id = "nrg_cb_gasm-emissions",
    source = "eurostat",
    format = "csv",
    update.time = update.time
))


# - OIL ------------------------------------------------------------------------
# Gross inland deliveries (observed) of oil products, without the bio fractions
update.time = now()
c.oil = c(
    O4671XR5220B = "gasoil",    # Gas oil and diesel oil (excluding biofuel portion)
    O4652XR5210B = "gasoline",  # Motor gasoline (excluding biofuel portion)
    O4661XR5230B = "kerosene",  # Kerosene-type jet fuel (excluding biofuel portion)
    O4680 = "fueloil",          # Fuel oil
    O4630 = "lpg"               # Liquefied petroleum gases
)

d.oil = as.data.table(
    get_eurostat("nrg_cb_oilm", filters = list(geo = "AT", freq = "M"), time_format = "date")
)[nrg_bal == "GID_OBS" & siec %in% names(c.oil) & unit == "THS_T" & !is.na(values), .(
    date = time,
    product = c.oil[siec],
    ths.t = values
)]

saveToStorages(d.oil[order(date, product)], list(
    id = "nrg_cb_oilm-emissions",
    source = "eurostat",
    format = "csv",
    update.time = update.time
))
