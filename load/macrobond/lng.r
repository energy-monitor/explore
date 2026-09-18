# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/macrobond/_shared.r")


# - DOIT -----------------------------------------------------------------------

# Title	Source	Frequency	Start date	Name
# EU, LNG, EU Price, Spot, EUR	European Union Agency for the Cooperation of Energy Regulators (ACER)	Daily	08.03.2023	eueucaes0014

# Unlike the other price series this one is quoted in EUR/MWh, i.e. on the same
# basis as the CEGH gas price (`price-gas`) and the electricity prices.

c.series = c(
    eueucaes0014 = "price"
)

saveMacrobondData(c.series, "price-lng")
