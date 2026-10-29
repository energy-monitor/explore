# Climate series of Austria, its states and districts from the daily district
# data of the GeoSphere grids, see `load/geosphere/districts.py`:
# - `precipitation-snow`: daily values of Austria (and Vienna), the area mean
# - `climate-regions-monthly`: monthly values of all regions (country, states,
#   districts), e.g. for the climate change section of the web page
#
# The states and the country are the cell weighted means of their districts.

# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("_shared.r")
loadPackages("arrow")


# - LOAD -----------------------------------------------------------------------
update.time = now()

d.daily = as.data.table(read_parquet(file.path(g$d$geosphere, "districts-daily.parquet")))
d.daily[, date := as.Date(date)]

# A partial load (e.g. a test with `--from`) must not replace the full series
# in the storages
if (min(d.daily$date) > as.Date("1961-12-31") && Sys.getenv("CLIMATE_ALLOW_PARTIAL") == "") {
    stop("the daily district data starts at ", min(d.daily$date), ", run the full load first")
}
d.districts = fread(file.path(g$d$geosphere, "districts.csv"), colClasses = list(character = c("district", "state")))

c.states = c(
    "1" = "Burgenland", "2" = "Kärnten", "3" = "Niederösterreich", "4" = "Oberösterreich",
    "5" = "Salzburg", "6" = "Steiermark", "7" = "Tirol", "8" = "Vorarlberg", "9" = "Wien"
)

c.vars = setdiff(names(d.daily), c("date", "district", "n"))


# - REGIONS --------------------------------------------------------------------
# All variables are means over the cells of a district, so a region is the
# mean of its districts weighted by their number of cells
weightedMean = function(d, level, region) {
    d = copy(d)[, `:=`(level = level, region = region)]
    d[, c(list(n = sum(n)), lapply(.SD, function(v) sum(v * n) / sum(n))),
        by = .(date, level, region), .SDcols = c.vars]
}

d.regions = rbind(
    weightedMean(d.daily, "country", "AT")[, name := "Österreich"],
    weightedMean(d.daily, "state", substr(d.daily$district, 1, 1))[, name := c.states[region]],
    weightedMean(d.daily, "district", d.daily$district)[
        , name := d.districts[match(region, district), name]
    ]
)


# - DAILY ----------------------------------------------------------------------
# The national series of the hydro indicator, Vienna in addition
d.at = d.regions[region == "AT", c("date", c.vars), with = FALSE]
setnames(d.at, c("rr"), c("precipitation"))

d.vienna = d.regions[region == "900", .(date, precipitation.vienna = rr, swe.vienna = swe)]

d.daily.final = merge(d.at, d.vienna, by = "date")[order(date)]

saveToStorages(d.daily.final, list(
    id = "precipitation-snow",
    source = "geosphere",
    format = "csv",
    update.time = update.time
))


# - MONTHLY --------------------------------------------------------------------
# Sums for precipitation and sunshine, the indicator shares add up to days,
# means for temperature and snow (plus the maximum of the snow water equivalent)
c.shares = grep("^share_", c.vars, value = TRUE)

d.monthly = d.regions[, c(
    list(
        days = .N,
        rr = sum(rr),
        tm = mean(tm), tn = mean(tn), tx = mean(tx),
        sa = sum(sa),
        swe = mean(swe), swe.max = max(swe),
        snow_depth = mean(snow_depth)
    ),
    lapply(.SD, sum)
), by = .(level, region, name, year = year(date), month = month(date)), .SDcols = c.shares]

setnames(d.monthly, c.shares, sub("^share_", "days.", c.shares))

saveToStorages(d.monthly[order(level, region, year, month)], list(
    id = "climate-regions-monthly",
    source = "geosphere",
    format = "csv",
    update.time = update.time
))
