# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("_shared.r")
loadPackages(
    openxlsx
)


# - DOWN -----------------------------------------------------------------------
# url = "https://ec.europa.eu/energy/observatory/reports/Oil_Bulletin_Prices_History.xlsx"
url = "https://energy.ec.europa.eu/document/download/906e60ca-8b6a-44e7-8589-652854d2fd3f_en?filename=Weekly_Oil_Bulletin_Prices_History_maticni_4web.xlsx"
t = tempfile(fileext = ".xlsx")
update.time = now()
download.file(url, t, mode = "wb")


# - PREP -----------------------------------------------------------------------
c.cols = as.character(openxlsx::read.xlsx(t, sheet = "Prices with taxes", rows = 1, colNames = FALSE, skipEmptyCols = FALSE))
c.cols.at = startsWith(c.cols, "AT_")
c.cols.at[1] = TRUE
d.raw = as.data.table(openxlsx::read.xlsx(t, sheet = "Prices with taxes", startRow = 4, colNames = FALSE, skipEmptyCols = FALSE))
d.at = d.raw[, ..c.cols.at]
c.labels = c.cols[c.cols.at]
c.labels[1] = "Date"
setnames(d.at, c.labels)

d.base = d.at[, .(
    date = convertToDate(Date),
    euroSuper95 = as.numeric(sub(',', '', AT_price_with_tax_euro95))/1000,
    gasOil = as.numeric(sub(',', '', AT_price_with_tax_diesel))/1000
)][!is.na(date)]

d.full = melt(d.base, id.vars = "date")[order(date), ]


# - STORAGE --------------------------------------------------------------------
saveToStorages(d.full, list(
    id = "price-gas-oil",
    source = "ec",
    format = "csv",
    update.time = update.time
))
