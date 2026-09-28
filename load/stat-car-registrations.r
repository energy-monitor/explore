# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("_shared.r")
loadPackages(
    readODS
)

c.months = c(
    "Jänner", "Februar", "März", "April", "Mai", "Juni",
    "Juli", "August", "September", "Oktober", "November", "Dezember"
)

# "Tabelle 2: Pkw-Neuzulassungen nach Kraftstoffart bzw. Energiequelle" of a monthly sheet
readFuelTable = function(f, sheet) {
    x = read_ods(f, sheet = sheet, col_names = FALSE, col_types = NA, .name_repair = "minimal")
    label = trimws(x[[1]])
    i.start = grep("^Tabelle 2:", label)
    if (length(i.start) == 0)
        return(NULL)
    i.end = grep("^Pkw insgesamt", label)
    i.end = i.end[i.end > i.start][1]
    i.rows = (i.start + 2):i.end

    # header row holds the month, e.g. "August 2026"
    month = strsplit(trimws(x[[2]][i.start + 1]), "\\s+")[[1]]
    value = trimws(x[[2]][i.rows])

    data.table(
        date = make_date(as.integer(month[2]), match(month[1], c.months), 1),
        label = label[i.rows],
        cars = as.integer(fifelse(value == "-", "0", value))
    )
}


# - LOAD -----------------------------------------------------------------------
# one file per year with a sheet per month, the file of the current year is
# renamed with every release, so the links are taken from the page
update.time = now()
url.base = "https://www.statistik.at"
page = read_html(glue("{url.base}/statistiken/tourismus-und-verkehr/fahrzeuge/kfz-neuzulassungen"))
urls = unique(xml_attr(xml_find_all(page, "//a[@href]"), "href"))
urls = urls[grepl("NeuzulassungenFahrzeugeJaennerBis\\w+\\d{4}\\.ods$", urls)]

d.base = rbindlist(lapply(urls, function(url) {
    l(url)
    f = tempfile(fileext = ".ods")
    download.file(paste0(url.base, url), dest = f, mode = "wb", quiet = TRUE)
    d = rbindlist(lapply(list_ods_sheets(f), function(s) readFuelTable(f, s)))
    unlink(f)
    d
}))
stopifnot(!anyDuplicated(d.base[, .(date, label)]))


# - PREP -----------------------------------------------------------------------
# hybrids include plug-ins ("darunter ... Plug-In"), everything else (natural
# gas, lpg, hydrogen) is other
d.base[, type := factor(fcase(
    label == "Benzin", "petrol",
    label == "Diesel", "diesel",
    label == "Elektro", "bev",
    startsWith(label, "Benzin/Elektro"), "hybrid.pet",
    startsWith(label, "Diesel/Elektro"), "hybrid.die",
    startsWith(label, "darunter Benzin/Elektro"), "phev.pet",
    startsWith(label, "darunter Diesel/Elektro"), "phev.die",
    label == "Pkw insgesamt", "total",
    default = "other"
), levels = c("petrol", "diesel", "bev", "hybrid.pet", "hybrid.die", "phev.pet", "phev.die", "other", "total"))]

d.wide = dcast(d.base, date ~ type, value.var = "cars", fun.aggregate = sum, drop = FALSE)
stopifnot(d.wide[, petrol + diesel + bev + hybrid.pet + hybrid.die + other == total])

d.final = melt(d.wide[, .(
    date,
    petrol,
    diesel,
    hybrid = hybrid.pet + hybrid.die - phev.pet - phev.die,
    phev = phev.pet + phev.die,
    bev,
    other,
    total
)], id.vars = "date", variable.name = "type", value.name = "cars")


# - STORAGE --------------------------------------------------------------------
saveToStorages(d.final[order(date, type)], list(
    id = "car-registrations",
    source = "stat",
    format = "csv",
    update.time = update.time
))
