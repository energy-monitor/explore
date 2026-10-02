# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/asfinag/_shared.r")


# - LOAD -----------------------------------------------------------------------
# average daily traffic per month at the permanent counting stations on
# motorways and expressways, since 2012
update.time = now()
urls = getAsfinagLinks()
l(glue("{length(urls)} files"))

# zips first, so that the revised months of a finished year are kept if a
# month is still linked as a single file as well
urls = urls[order(!grepl("\\.zip$", urls, ignore.case = TRUE))]
files = unlist(lapply(urls, function(url) listAsfinagMonthlyFiles(downloadAsfinagFile(url))))
files = files[!duplicated(substr(basename(files), 1, 4))]

d.base = rbindlist(lapply(files, readAsfinagMonthlyFile))
stopifnot(!anyNA(d.base$vehicle))


# - PREP -----------------------------------------------------------------------
# a row is listed twice in some months (same values, different quality)
d.final = unique(d.base, by = c("date", "station.id", "section", "direction", "vehicle"))
l(glue("{nrow(d.base) - nrow(d.final)} duplicated rows removed"))


# - STORAGE --------------------------------------------------------------------
saveToStorages(d.final[order(date, road, km, station.id)], list(
    id = "traffic-asfinag",
    source = "asfinag",
    format = "csv",
    update.time = update.time
))
