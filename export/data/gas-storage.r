# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")


# - LOAD/PREP ------------------------------------------------------------------
countries = c("AT", "EU")

# The reported stock (TWh) sometimes jumps away and back within a few days
# while the reported flows (GWh) do not. Such glitches are detected via the
# residual between the stock change and the net withdrawal: a large residual
# (> threshold, share of the working gas volume) that is cancelled out again by
# the following days (within max.days). The affected days are rebuilt from the
# previous stock and the reported flows.
fixStorageGlitches = function(d, threshold = 0.003, max.days = 3, tolerance = 0.2) {
    d = d[order(gasDayStart)]
    s = d$gasInStorage
    nw = fifelse(is.na(d$netWithdrawal), 0, d$netWithdrawal) / 1000
    thr = threshold * max(d$workingGasVolume, na.rm = TRUE)
    r = c(0, diff(s)) + nw
    n = length(s)

    i = 2
    while (i < n) {
        if (!is.na(r[i]) && abs(r[i]) > thr) {
            for (j in (i + 1):min(i + max.days, n)) {
                if (is.na(r[j]) || sign(r[j]) == sign(r[i]) || abs(r[j]) <= thr / 2) next
                if (abs(sum(r[i:j])) < tolerance * abs(r[i])) {
                    for (k in i:(j - 1)) s[k] = s[k - 1] - nw[k]
                    l(glue("fixed storage glitch: {paste(d$gasDayStart[i:(j - 1)], collapse = ', ')}"), iL = 2)
                    i = j
                    break
                }
            }
        }
        i = i + 1
    }

    d[, gasInStorage := s][]
}

for (country in countries) {
    d.base = loadFromStorage(id = glue("storage-{country}"))[,
        gasDayStart := as.Date(gasDayStart)
    ]
    d.base = fixStorageGlitches(d.base)

    # - PLOT -------------------------------------------------------------------
    # Preparation
    d.plot = d.base[, .(
        type = "stock",
        date = gasDayStart,
        value = gasInStorage
    )][order(date)]

    d.plot = rbind(d.plot, d.plot[, .(
        type = "flow",
        date,
        value = value - shift(value, 7)
    )])
    dates2PlotDates(d.plot)

    # Save
    fwrite(d.plot[year >= min(yearsShown())], file.path(g$d$wd, "gas", if (country == "AT") "storage.csv" else glue("storage-{tolower(country)}.csv")))
}
