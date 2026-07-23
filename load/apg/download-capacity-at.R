
# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/apg/_shared.r")


# - LOAD -----------------------------------------------------------------------
start_year = 2023
end_year = as.integer(format(Sys.Date(), "%Y")) + 1
update.time = now()

d.base = load_apg_capacity_at(start_year = start_year, end_year = end_year)


# - STORE ----------------------------------------------------------------------

saveToStorages(d.base, list(
	id = "capacity-at",
	source = "apg",
	format = "csv",
	update.time = update.time
))

