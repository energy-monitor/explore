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
c.products = c(
    euro95 = "euroSuper95",
    diesel = "gasOil",
    heating_oil = "heatingOil"
)

c.cols = as.character(openxlsx::read.xlsx(t, sheet = "Prices with taxes", rows = 1, colNames = FALSE, skipEmptyCols = FALSE))
d.raw = as.data.table(openxlsx::read.xlsx(t, sheet = "Prices with taxes", startRow = 4, colNames = FALSE, skipEmptyCols = FALSE))
# trailing columns without any data are dropped by read.xlsx
c.cols = c.cols[seq_len(ncol(d.raw))]
c.cols[1] = "Date"
setnames(d.raw, make.unique(c.cols))

# drop the notes below the data
d.raw = d.raw[grepl("^[0-9]+$", Date)]

# one column per country (incl. the EU and EUR aggregates) and product, e.g. 'AT_price_with_tax_diesel'
c.pattern = glue("^([A-Z]+)_price_with_tax_({paste(names(c.products), collapse = '|')})$")
c.cols.price = grep(c.pattern, names(d.raw), value = TRUE)
d.raw[, (c.cols.price) := lapply(.SD, function(x) {
    if (is.character(x)) x = gsub(",", "", x)
    as.numeric(x)/1000
}), .SDcols = c.cols.price]

d.full = melt(
    d.raw[, c("Date", c.cols.price), with = FALSE],
    id.vars = "Date", variable.factor = FALSE
)[, .(
    date = convertToDate(as.numeric(Date)),
    country = sub(c.pattern, "\\1", variable),
    variable = unname(c.products[sub(c.pattern, "\\2", variable)]),
    value
)][!is.na(value)][order(date, country, variable)]


# - STORAGE --------------------------------------------------------------------
saveToStorages(d.full, list(
    id = "price-gas-oil",
    source = "ec",
    format = "csv",
    update.time = update.time
))
