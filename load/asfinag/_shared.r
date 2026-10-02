# - INIT -----------------------------------------------------------------------
source("_shared.r")
loadPackages(
    readxl
)

c.asfinag.url = "https://www.asfinag.at/verkehr-sicherheit/verkehrszaehlung/"
c.asfinag.vehicles = c(
    "Kfz" = "all",
    "Kfz > 3,5t hzG" = "heavy",
    "Kfz <= 3,5t hzG" = "light"
)

# Links to the files of the traffic statistics, a zip per finished year and a
# monthly file per month of the current year. The links contain a random id
# and are not stable, so they are taken from the page.
getAsfinagLinks = function() {
    page = read_html(c.asfinag.url)
    urls = unique(xml_attr(xml_find_all(page, "//a[@href]"), "href"))
    urls[grepl("^https://media\\.asfinag\\.at/media/.+\\.(zip|xlsx?)$", urls, ignore.case = TRUE)]
}

# Downloads to the tmp folder, a file is downloaded again only if its link
# changed (corrections are published under a new link)
downloadAsfinagFile = function(url, dir = file.path(g$d$tmp, "asfinag")) {
    dir = file.path(dir, basename(dirname(url)))
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    f = file.path(dir, basename(url))
    if (!file.exists(f)) {
        l(url, iL = 2)
        download.file(url, dest = f, mode = "wb", quiet = TRUE)
    }
    f
}

# Monthly files, named "yymm_asfinag_verkehrsstatistik*" (case and suffix
# vary), the annual values ("Jahr..") are skipped
listAsfinagMonthlyFiles = function(f) {
    if (grepl("\\.zip$", f, ignore.case = TRUE)) {
        dir = file.path(dirname(f), sub("\\.zip$", "", basename(f), ignore.case = TRUE))
        unzip(f, exdir = dir)
        f = list.files(dir, full.names = TRUE)
    }
    f[grepl("^\\d{4}_", basename(f))]
}

# Sheet "Daten" of a monthly file, three header rows, the layout is the same
# for all months since 2012
readAsfinagMonthlyFile = function(f) {
    x = read_excel(f, sheet = "Daten", col_names = FALSE, col_types = "text", .name_repair = "minimal")
    stopifnot(
        x[[1]][1] == "Autobahn",
        x[[8]][1] == "DTVMS",
        x[[14]][1] == "DTVSF",
        x[[15]][1] == "Datengüte"
    )
    x = x[-(1:3), ]

    # vehicles per day (older files partly with decimals), failures are -1
    num = function(v) {
        v = round(as.numeric(v))
        fifelse(v < 0, NA_real_, v)
    }
    total = function(v) fifelse(v == "gesamt", "total", v)

    # share of the estimated days: "Messung" (none), "x,y% Tage geschätzt"
    # or "Ausfall" (failure, NA)
    quality = x[[15]]
    estimated = fifelse(quality == "Messung", 0, NA_real_)
    i = grepl("% Tage geschätzt$", quality)
    estimated[i] = as.numeric(sub(",", ".", sub("%.*", "", quality[i]))) / 100

    data.table(
        date = make_date(2000 + as.integer(substr(basename(f), 1, 2)), as.integer(substr(basename(f), 3, 4)), 1),
        road = x[[1]],
        km = round(as.numeric(x[[2]]), 3),
        station.id = as.integer(x[[4]]),
        station = x[[3]],
        section = total(x[[5]]),
        direction = total(x[[6]]),
        vehicle = unname(c.asfinag.vehicles[gsub("\\s+", " ", x[[7]])]),
        dtv.ms = num(x[[8]]),
        dtv.mf = num(x[[9]]),
        dtv.mo = num(x[[10]]),
        dtv.dd = num(x[[11]]),
        dtv.fr = num(x[[12]]),
        dtv.sa = num(x[[13]]),
        dtv.sf = num(x[[14]]),
        estimated = estimated
    )
}
