# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/macrobond/_shared.r")


# - DOIT -----------------------------------------------------------------------

# Title	Source	Frequency	Start date	Name
# ICE U.S. Dollar per Euro, 1st Position, Close	Intercontinental Exchange (ICE)	Daily	07.01.1986	eo_c1_cl

c.series = c(
    eo_c1_cl = "price"
)

saveMacrobondData(c.series, "price-dollar")