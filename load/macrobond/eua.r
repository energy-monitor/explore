# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/macrobond/_shared.r")


# - DOIT -----------------------------------------------------------------------

# Title	Source	Frequency	Start date	Name
# Future, ICE EUA, 1st Position, Close	Intercontinental Exchange (ICE)	Daily	22.04.2005	icec_c1_cl

# The previous series (NASDAQ OMX Commodities, `ned_c1_st`) was discontinued by
# the source: Nasdaq Commodities delisted all its futures and withdrew the
# service on 30.04.2026, the last observation is 06.01.2026. Its data is kept
# as `price-eua-nasdaq`.

c.series = c(
    icec_c1_cl = "value"
)

saveMacrobondData(c.series, "price-eua")
