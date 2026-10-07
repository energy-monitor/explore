# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/agrarforschung/_shared.r")


# - DOIT -----------------------------------------------------------------------

# Title	Source	Unit	Frequency	Start date	contentId
# Preise für Brennholz in Österreich	Statistik Austria	EURO/RM	Monthly	01.2010	brennholz_monatl

# Land- und forstwirtschaftliche Erzeugerpreisstatistik, the prices are per
# Raummeter (RM, stacked cubic metre) of hard resp. soft firewood.

c.attrs = c(
    at_brennholz_h_monat = "hard",
    at_brennholz_w_monat = "soft"
)

saveAgrarforschungData("brennholz_monatl", "price-firewood", c.attrs)
