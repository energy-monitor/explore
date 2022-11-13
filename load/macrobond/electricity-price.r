# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/macrobond/_shared.r")


# - DOIT -----------------------------------------------------------------------

# Austria, AT, Day Base Price, EUR	EEX (European Energy Exchange)	Daily	01.10.2018	atelspotbase
# Austria, AT, Day Base Volume	EEX (European Energy Exchange)	Daily	01.10.2018	atelspotbasevol
# Austria, AT, Day Peak Price, EUR	EEX (European Energy Exchange)	Daily	01.10.2018	atelspotpeak
# Austria, AT, Day Peak Volume	EEX (European Energy Exchange)	Daily	01.10.2018	atelspotpeakvol

c.series = c(
    atelspotbase = "base",
    atelspotpeak = "peak",
    atelspotbasevol = "baseVol",
    atelspotpeakvol = "peakVol"
)

saveMacrobondData(c.series, "price-electricity")
