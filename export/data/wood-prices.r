# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")


# - PELLETS --------------------------------------------------------------------
# Quelle: proPellets Austria; https://www.propellets.at/aktuelle-pelletpreise
# Lose Pellets (ISO 17225-2 A1 bzw. ENplus A1) bei einer Bestellmenge von 6 t,
# in Cent/kg. Monthly, the survey is published on the 15th of a month.
d.plot = loadFromStorage(id = "price-pellets")[, .(
    date = as.Date(date),
    value = price
)]
dates2PlotDates(d.plot)

# Save
fwrite(
    d.plot[year >= min(yearsShown())][order(date)],
    file.path(g$d$wd, "wood", "pellets.csv")
)


# - FIREWOOD -------------------------------------------------------------------
# Quelle: Statistik Austria, Land- und Forstwirtschaftliche Erzeugerpreisstatistik
# Brennholz hart bzw. weich, in EURO/RM (Raummeter). The two are kept apart,
# they differ by roughly a third and share no common volume measure with the
# pellet price, hence the separate file.
d.raw = loadFromStorage(id = "price-firewood")[, date := as.Date(date)]
d.plot = melt(d.raw, id.vars = "date", variable.name = "type")
dates2PlotDates(d.plot)

# Save
fwrite(
    d.plot[year >= min(yearsShown())][order(type, date)],
    file.path(g$d$wd, "wood", "firewood.csv")
)
