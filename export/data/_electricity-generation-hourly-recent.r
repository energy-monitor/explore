# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")


# - LOAD -----------------------------------------------------------------------
d.gen = loadFromStorage(id = "electricity-generation-hourly-recent-g1")
d.load = loadFromStorage(id = "electricity-load-recent")
d.exports = loadFromStorage(id = "physical-flows-entsoe-recent")


setnames(d.load, "value", "load")
d.joined = merge(d.load, d.exports, by = c("date", "hour"))
d.joined = melt(d.joined, id.vars = c("date", "hour"))
# d.

fromDate = "2025-01-10"



# # Save
# d.plot = d.base[year == 2021, .(
#     hour, source.group, value
# )]
# fwrite(d.plot, file.path(g$d$wd, "electricity", "generation-hourly.csv"))


loadPackages("ggplot2")


d.gen$time = as.POSIXct(d.gen$date)
hour(d.gen$time) = d.gen$hour

d.joined$time = as.POSIXct(d.joined$date)
hour(d.joined$time) = d.joined$hour

# d.load$time = as.POSIXct(d.load$date)
# hour(d.load$time) = d.load$hour

# d.exports$time = as.POSIXct(d.exports$date)
# hour(d.exports$time) = d.exports$hour

d.gen = d.gen[time >= fromDate]
d.joined = d.joined[time >= fromDate]
# d.load = d.load[time >= fromDate]
# d.exports = d.exports[time >= fromDate]


d.joined = rbind(
    d.joined,
    d.joined[, .(value = sum(value), variable = "netto") , by = .(time, date, hour)]
)


used.sources = d.gen[, .(t = sum(value)), by =source.group][t > 0, source.group]
d.gen = d.gen[source.group %in% used.sources]



ggplot(d.gen, aes(x = time, y = value)) +
    geom_area(aes(fill = source.group)) +
    geom_line(data = d.joined, aes(y = value/4, linetype = variable), size = 0.5)
    # geom_line(data = d.load, aes(y = value/4)) + 
    # geom_line(data = d.exports, aes(y = exports/4))

ggsave("generation-recent.png", width = 10, height = 5)
