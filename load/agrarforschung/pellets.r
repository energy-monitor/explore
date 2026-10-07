# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/agrarforschung/_shared.r")


# - DOIT -----------------------------------------------------------------------

# Title	Source	Unit	Frequency	Start date	contentId
# Pellets lose in Österreich	proPellets Austria	Cent/kg	Monthly	01.2010	pellets

# proPellets Austria surveys >50 dealers (>70% of the traded volume) and
# publishes on the 15th of a month, the price is for loose ENplus A1 pellets
# at an order size of 6 t. 1 Cent/kg = 10 EUR/t, with ~4.8 kWh/kg that is
# roughly EUR/MWh = value * 100 / 4.8.

c.attrs = c(
    Pellets = "price"
)

saveAgrarforschungData("pellets", "price-pellets", c.attrs)
