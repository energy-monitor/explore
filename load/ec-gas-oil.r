# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("_shared.r")
loadPackages(
    openxlsx
)


# - DOWN -----------------------------------------------------------------------
url = "https://ec.europa.eu/energy/observatory/reports/Oil_Bulletin_Prices_History.xlsx"
t = tempfile(fileext = ".xlsx")
download.file(url, t, mode = "wb")


# - PREP -----------------------------------------------------------------------
d.raw = as.data.table(openxlsx::read.xlsx(t, sheet = "Prices with taxes, per CTR", startRow = 7))
d.raw = d.raw[-1]
d.raw = d.raw[1:(which(d.raw$Date == "BE") - 1)]

d.base = d.raw[, .(
    date = convertToDate(Date),
    euroSuper95 = as.numeric(sub(',', '', `Euro-super.95.(I)`))/1000,
    gasOil = as.numeric(sub(',', '', `Gas.oil.automobile.Automotive.gas.oil.Dieselkraftstoff.(I)`))/1000
)]

d.full = melt(d.base, id.vars = "date")[order(date), ]


# - STORAGE --------------------------------------------------------------------
saveToStorages(d.full, list(
    id = "price-gas-oil",
    source = "ec",
    format = "csv"
))
