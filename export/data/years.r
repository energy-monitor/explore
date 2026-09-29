# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("export/data/_shared.r")

# Rotates the years in the chart definitions, so that the newest year is always
# the current one (colours, opacity and visibility move with the relative
# position of a year). Only the lines of the year keys are changed, the files
# are left as they are otherwise.

c.files = c("shared.json", "electricity/price-entsoe.json")
re.year = '^(\\s*)"(\\d{4})":'


# - DOIT -----------------------------------------------------------------------
readYears = function(lines) as.integer(sub(paste0(re.year, ".*$"), "\\2", grep(re.year, lines, value = TRUE)))

c.shared = readLines(file.path(g$d$wd, "shared.json"), warn = FALSE)
shift = max(yearsShown()) - max(readYears(c.shared))

if (shift != 0) {
    for (f in c.files) {
        p = file.path(g$d$wd, f)
        c.lines = readLines(p, warn = FALSE)
        i = grep(re.year, c.lines)
        c.years = readYears(c.lines)
        c.lines[i] = mapply(function(line, y) sub('"\\d{4}":', glue('"{y + shift}":'), line), c.lines[i], c.years, USE.NAMES = FALSE)
        writeLines(c.lines, p)
        l(glue("{f}: years shifted by {shift}"), iL = 2)
    }
}
