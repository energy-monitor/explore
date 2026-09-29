# - INIT -----------------------------------------------------------------------
options(repos = "https://cran.wu.ac.at/")
if (!'librarian' %in% rownames(installed.packages())) install.packages("librarian")
loadPackages = librarian::shelf
loadPackages(
    'data.table', 'zoo', 'glue', 'lubridate', 'jsonlite', 'httr', 'xml2', 'clock', 'digest'
)

source("_storage.r")

# - GLOB -----------------------------------------------------------------------
# Merges the settings of `y` into `x`, objects are merged recursively, all
# other values (incl. arrays) are replaced
mergeConfig = function(x, y) {
    isObject = function(v) is.list(v) && !is.null(names(v))
    for (n in names(y)) {
        x[[n]] = if (isObject(x[[n]]) && isObject(y[[n]])) mergeConfig(x[[n]], y[[n]]) else y[[n]]
    }
    x
}

# Settings specific to a machine (storage, paths) go into the untracked
# config.local.json, which overrides config.json
l.config = read_json("config.json")
if (file.exists("config.local.json")) {
    l.config = mergeConfig(l.config, read_json("config.local.json"))
}

g = modifyList(l.config, list(
    entsoe = list(
        params = list(
            protocol = "sftp",
            server = "sftp-transparency.entsoe.eu/",
            # File Library API endpoints
            fms_base_url = "https://fms.tp.entsoe.eu",
            keycloak_url = "https://keycloak.tp.entsoe.eu/realms/tp/protocol/openid-connect/token"
        ),
        fileTypes = list(
            generation = "AggregatedGenerationPerType_16.1.B_C",
            load = "ActualTotalLoad_6.1.A",
            dayAheadPrices = "EnergyPrices_12.1.D",
            netPositions = "ImplicitAllocationsNetPositionsDaily_12.1.E",
            physicalFlows = "PhysicalFlows_12.1.G"
        )
    ), gie = list(
        params = list()
    ), aggm = list(
        params = list()
    ), sftp = list(
        # key passphrase of the sftp storage, see `_storage.r`
        params = list()
    )
))


# - CREDS ----------------------------------------------------------------------
creds = read_json(g$f$creds)
invisible(sapply(names(creds), function(n) {
    g[[n]]$params <<- modifyList(g[[n]]$params, creds[[n]])
}))
rm(creds, l.config)


# - HELPERS --------------------------------------------------------------------
removeLastDays = function(d, days = 1) {
    d[date <= max(date, na.rm = TRUE) - days, ]
}

# logging
l = function (..., iL = 0, nL = TRUE)  {
    iS = paste(rep(" ", iL), collapse = "")
    m = paste0(iS, paste0(...))
    if (nL)
        m = paste0(m, "\n")
    cat(m)
}

