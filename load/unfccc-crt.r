# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("_shared.r")
loadPackages(
    readxl
)

# Austrian greenhouse gas inventory, submission to the UNFCCC (CRT), yearly by
# CRT sector and fuel, from the summary workbook of the Umweltbundesamt
# (no download, the file is placed in data/unfccc)
# - activity: fuel combustion in TJ (1. Energy only)
# - CO2, CH4, N2O, NF3, SF6: emissions in kt of the gas itself
# - HFCs, PFCs, unspecified mix (of HFCs and PFCs): emissions in kt CO2
#   equivalents, as they are mixtures of gases
# - fuel: liquid, solid, gaseous, waste (other fossil fuels), peat, biomass,
#   empty for totals and single products (gasoline, diesel oil, ...); totals
#   of 1.A exclude biomass, its CO2 is a memo item
# - notation: notation key (NO, IE, NA, NE, C, ...) if there is no value; the
#   key "NA" (not applicable) is read back as missing by fread, a row without
#   value and notation is therefore "NA"
# - info: rows of an information item, already included elsewhere (e.g. waste
#   incineration with energy recovery in 1.A.5.b, reported under 1.A.1.a)


# - CONF -----------------------------------------------------------------------
unfccc.file = "data/unfccc/Summary_UNFCCC_052026_ex.xlsx"
unfccc.years = 1990:2024

# Sheets of the raw data set ("_ghg" is not used, its values are not cached)
unfccc.sheets = data.table(
    sheet = c("act_energy", "_co2", "_ch4", "_n2o", "_hfc", "_pfc", "_unspec", "_nf3", "_sf6"),
    gas = c("activity", "CO2", "CH4", "N2O", "HFCs", "PFCs", "unspecified", "NF3", "SF6"),
    unit = c("TJ", "kt", "kt", "kt", "kt CO2e", "kt CO2e", "kt CO2e", "kt", "kt")
)


# - FUNCTIONS ------------------------------------------------------------------
readSheet = function(sheet) {
    d = as.data.table(read_excel(unfccc.file, sheet = sheet, col_names = FALSE, col_types = "text", .name_repair = "minimal"))
    setnames(d, paste0("V", seq_along(d)))
    c.years = paste0("V", seq_along(unfccc.years) + 6)

    # The header of the first year reads "2015", the values are those of 1990
    stopifnot(
        identical(d$V1[5], "SECTOR"),
        identical(as.integer(unlist(d[5, c.years[-1], with = FALSE])), unfccc.years[-1])
    )

    # Rows of the CRT tables, without the filler rows at the end ("6.", "0")
    d = d[-(1:5)][!is.na(V1) & V2 != "0"]
    d[, info := cumsum(grepl("^Information item", V2)) > 0, by = V1]
    d = d[!grepl("^Information item|:$", V2)]

    # The name of a sector is given in its total row, after the code
    d.sector = d[V2 == "Total", .(V1, sector = trimws(substring(V5, nchar(V1) + 1)))]
    d = merge(d, d.sector, by = "V1", all.x = TRUE, sort = FALSE)

    d = melt(d, id.vars = c("V1", "sector", "V2", "V4", "info"), measure.vars = c.years, variable.name = "year", value.name = "value.raw")
    d[, year := unfccc.years[match(year, c.years)]]
    d = d[!is.na(value.raw)]
    d[, value := suppressWarnings(as.numeric(value.raw))]
    d[, .(year, code = V1, sector, fuel = V4, item = V2, info, value, notation = ifelse(is.na(value), value.raw, NA))]
}


# - LOAD -----------------------------------------------------------------------
l(unfccc.file)
update.time = file.mtime(unfccc.file)

d.prep = rbindlist(lapply(seq_len(nrow(unfccc.sheets)), function(i) {
    s = unfccc.sheets[i]
    l(glue("{s$sheet} -> {s$gas} [{s$unit}]"), iL = 2)
    readSheet(s$sheet)[, .(year, gas = s$gas, unit = s$unit, code, sector, fuel, item, info, value, notation)]
}))

# One value per year, gas, code and item
stopifnot(
    nrow(d.prep) > 0,
    !anyDuplicated(d.prep[, .(year, gas, code, item, info)]),
    d.prep[gas == "CO2" & code == "1.A." & item == "Total", .N] == length(unfccc.years)
)

saveToStorages(d.prep[order(gas, code, year)], list(
    id = "unfccc-crt",
    source = "Umweltbundesamt",
    format = "csv",
    update.time = update.time
))
