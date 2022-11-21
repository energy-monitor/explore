# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")


# - LOAD -----------------------------------------------------------------------
d.base = loadFromStorage(id = "electricity-load-hourly-year")

# ggplot(d.agg, aes(y = value, group = year, x = hour, color = year)) +
#     geom_line()

# Save
fwrite(d.base, file.path(g$d$wd, "electricity", "load-hourly.csv"))
