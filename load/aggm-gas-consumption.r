# - INIT -----------------------------------------------------------------------
rm(list = ls())
source('_shared.r')

headers <- c("Content-Type" = "text/xml",
             "Accept" = "text/xml")

body_get_token <- glue("<request>",
             "<metadata>",
             "<action>authenticate</action>",
             "</metadata><payload>",
             "<username>{g$aggm$params$user}</username>",
             "<password>{g$aggm$params$pass}</password>",
             "</payload>",
             "</request>")

return_token <- POST("https://agimos.aggm.at/datenmanagementApi/Authenticate",
          body = body_get_token,
          add_headers(headers))

token <- xml_text(xml_children(xml_children(content(return_token))))[2]

body_get_timeseries <- glue("<request>",
             "<metadata>",
             "<action>publicInterface</action>",
             "<accessToken>{token}</accessToken>",
             "</metadata>",
             "<payload>",
             "<timeseriesData>",
             "<from>2018-12-01T00:00:00Z</from>",
             "<to>{Sys.Date()}T00:00:00Z</to>",
             "<timeseries>",
             "<name>ErmittelterEKVOst</name>",
             "<name>ErmittelterEKVTirol</name>",
             "<name>ErmittelterEKVVorarlberg</name>",
             "</timeseries>",
             "</timeseriesData>",
             "</payload>",
             "</request>")

return_timeseries <- POST("https://agimos.aggm.at/datenmanagementApi/PublicInterface",
          body = body_get_timeseries,
          add_headers(headers))

xml_timeseries <- content(return_timeseries)

from <- as.POSIXct(
    xml_text(
        xml_find_all(xml_timeseries, "/response/payload/timeseriesData/timeseries/dataSet/data/from")))

value <- as.numeric(
    xml_text(
        xml_find_all(xml_timeseries, "/response/payload/timeseriesData/timeseries/dataSet/data/value")))

d.agg.aggm <- data.table(from, value)[, .(
    value = sum(value)/10^9
), by=.(date = as.Date(from))][order(date)]


#### comparison of two data sources, uses tidyverse
#d.agg.aggm[d.agg, on = .(date)] %>%
#    gather(source, value, -date) %>%
#    ggplot(aes(x=date, y=value))+
#    geom_line(aes(col=source))

# Save
fwrite(d.agg.aggm, file.path(g$d$o, 'consumption-gas-aggm.csv'))
# d.agg.aggm = fread(file.path(g$d$o, 'consumption-gas-aggm.csv'))
# d.agg.aggm[, date := as.Date(date)]

# Plot, Preparation
addRollMean(d.agg.aggm, 7)
addCum(d.agg.aggm)
d.plot = meltAndRemove(d.agg.aggm)
dates2PlotDates(d.plot)

# Save
fwrite(d.plot, file.path(g$d$wd, 'gas/consumption', 'data-aggm.csv'))
