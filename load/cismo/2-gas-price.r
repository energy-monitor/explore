# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("_shared.r")
# loadPackages()


# - DOIT -----------------------------------------------------------------------
# Load
update.time = now()
j.raw = read_json(file.path(g$d$tmp, "gas-price.json"))
l.base = j.raw$values$ChartPlot$x$hc_opts$series
sapply(l.base, `[[`, "name")
idx = which(sapply(l.base, `[[`, "name") == "CEGH")

date = sapply(l.base[[idx]]$data, `[[`, 1)
price = sapply(l.base[[idx]]$data, `[[`, 2)
price[sapply(price, is.null)] = list(NA_real_)

# days without a price are left out instead of being stored as 0
d.raw = data.table(date = as.Date(as_datetime(date / 1000)), price = unlist(price))[!is.na(price)]


# - STORE ----------------------------------------------------------------------
saveToStorages(d.raw, list(
    id = "price-gas",
    source = "cismo",
    format = "csv",
    update.time = update.time
))
