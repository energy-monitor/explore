# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("_shared.r")

# Austrian greenhouse gas inventory (OLI) by CRT sector, yearly, from the
# Umweltbundesamt via data.gv.at
# https://www.data.gv.at/datasets/78bd7b69-c1a7-456b-8698-fac3b24f7aa5


# - LOAD -----------------------------------------------------------------------
# The download link may change with every release, so it is taken from the
# dataset's metadata ("... nach CRT - long")
update.time = now()
meta = fromJSON("https://www.data.gv.at/api/hub/search/datasets/78bd7b69-c1a7-456b-8698-fac3b24f7aa5")
dists = meta$result$distributions
url = unlist(dists$access_url[grepl("long$", dists$title$de)])
stopifnot(length(url) == 1)
l(url)

f = tempfile(fileext = ".csv")
download.file(paste0(url, "/download"), dest = f, mode = "wb", quiet = TRUE)
d.base = fread(f, sep = ";", dec = ",", encoding = "Latin-1")
unlink(f)
for (c in names(d.base)[sapply(d.base, is.character)]) set(d.base, j = c, value = enc2utf8(d.base[[c]]))


# - PREP -----------------------------------------------------------------------
d.prep = d.base[, .(
    year = Jahr,
    pollutant = Schadstoff,
    unit = Einheit,
    code = CRT_Code,
    sector = CRT_Sektor,
    edition = Quelle,
    value = Werte
)]
stopifnot(nrow(d.prep) > 0, "Memo 1 D 1 a" %in% d.prep$code)

saveToStorages(d.prep[order(pollutant, code, year)], list(
    id = "uba-thg-crt",
    source = "Umweltbundesamt",
    format = "csv",
    update.time = update.time
))
