# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/web-monitor/_shared.r")


# - LOAD -----------------------------------------------------------------------
d.base = loadFromStorage(id = "electricity-load")[, 
    date := as.Date(date)
]



# - AT -------------------------------------------------------------------------
d.at = d.base[country == "AT", .(date, value)]

# Plot, Preparation
addRollMean(d.at, 7)
addCum(d.at)
d.plot = meltAndRemove(d.at)
dates2PlotDates(d.plot)

# Save
fwrite(d.plot, file.path(g$d$wd, "electricity", "load.csv"))




# - INT -------------------------------------------------------------------------

# Plot, Preparation

prep = function(di, l = 7, g = character(0)) {
    d = copy(d.base)

    addRollMean(d, 7, g)
    # addCum(d, g)
 
    d = meltAndRemove(d, g)

    dates2PlotDates(d)

    d[]
}

d.plot = prep(d.base, l = 7, g = "country")
d.plot = d.plot[variable %in% c("rm7")]
# d.plot = d.plot[country %in% c("AT", "DE", "PT", "FR") & date >= "2022-01-01"]
# d.plot = d.plot[as.integer(date - min(date)) %% 5 == 0]


# Save
fwrite(d.plot, file.path(g$d$wd, "electricity", "load-international.csv"))



# loadPackages(countrycode, jsonlite)
#
# d.countries = data.table(iso2 = unique(d.plot$country), selected = FALSE)
# d.countries[, name := countrycode(iso2, "iso2c", "country.name.de")]
# d.countries[iso2 == "XK", name := "Kosovo"]
#
# d.countries = d.countries[order(name)]
#
# d.countries[iso2 %in% c("AT", "DE", "IT", "FR", "CH", "HU"), selected := TRUE]
#
# j = toJSON(sapply(as.character(d.countries$iso2), function(c) list(
#     name = d.countries[iso2 == c]$name,
#     visible = d.countries[iso2 == c]$selected
# ), simplify = FALSE), auto_unbox = TRUE, pretty = 4)

# writeLines(j, "clipboard")

# d.plot[, country := factor(country, levels = d.countries[order(name)]$iso2)]
# d.plot = d.plot[order(country, date)]






