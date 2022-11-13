# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/macrobond/_shared.r")


# - DOIT -----------------------------------------------------------------------

# Title	Source	Frequency	Start date	Name
# EUA Future - Daily, 1st Position	NASDAQ OMX Commodities	Daily	10.10.2013	ned_c1_st

c.series = c(
    ned_c1_st = "value"
)

saveMacrobondData(c.series, "price-eua")
