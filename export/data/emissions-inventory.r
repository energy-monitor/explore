# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")

# Yearly values of the greenhouse gas inventory (submission to the UNFCCC, see
# load/unfccc-crt.r), fuel combustion (CRT 1.A) only
d.crt = loadFromStorage(id = "unfccc-crt")[code %like% "^1\\.A\\." & info == FALSE]

# Shares of the categories of a year as `value.share`
addShares = function(d) {
    d[, value.share := value / sum(value), by = year]
}


# - SECTORS --------------------------------------------------------------------
# CO₂ of fossil fuels by sector, in Mt (the totals of a sector do not include
# biomass, its CO₂ is a memo item); 1.A.5 is military only
c.sectors = c(
    "1.A.1." = "energy", "1.A.2." = "industry", "1.A.3." = "transport",
    "1.A.4.b." = "households", "1.A.4.a." = "services", "1.A.4.c." = "agriculture",
    "1.A.5." = "other"
)
d.plot = d.crt[gas == "CO2" & item == "Total" & code %in% names(c.sectors), .(
    year, sector = c.sectors[code], value = value / 1000
)]

# The sectors add up to the total of 1.A
d.check = merge(
    d.plot[, .(value = sum(value)), by = year],
    d.crt[gas == "CO2" & item == "Total" & code == "1.A.", .(year, total = value / 1000)],
    by = "year"
)
stopifnot(nrow(d.check) == uniqueN(d.plot$year), all(abs(d.check$value - d.check$total) < 1e-6))

addShares(d.plot)
fwriteWeb(d.plot[order(year, match(sector, c.sectors))], file.path(g$d$wd, "emissions", "sectors.csv"))


# - FUELS ----------------------------------------------------------------------
# Energy used in fuel combustion by fuel, in PJ; peat (a few TJ until 2005)
# is added to the solid fuels
c.fuels = c(
    liquid = "oil", biomass = "biomass", solid = "coal", peat = "coal",
    waste = "waste", gaseous = "gas"
)
d.plot = d.crt[gas == "activity" & code == "1.A." & fuel %in% names(c.fuels), .(
    value = sum(value, na.rm = TRUE) / 1000
), by = .(year, fuel = c.fuels[fuel])]

addShares(d.plot)
fwriteWeb(d.plot[order(year, match(fuel, unique(c.fuels)))], file.path(g$d$wd, "emissions", "fuels.csv"))
