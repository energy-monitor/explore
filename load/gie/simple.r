# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/gie/_shared.r")
loadPackages(
    countrycode
)

c.euCountries = as.data.table(codelist)[eu28 == "EU", iso2c]

# colnames()


# - DOIT -----------------------------------------------------------------------
# Load

# d.base = loadGieDataAllPages()
# d.base = rbind(d.base, loadGieDataAllPages(country = "EU"))
