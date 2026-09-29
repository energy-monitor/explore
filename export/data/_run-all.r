#!/usr/bin/env Rscript
# - FILES ----------------------------------------------------------------------
path = "export/data"
c.scripts = grep("^[0-9a-z].*", list.files(path), value = TRUE)

# - RUN IT ---------------------------------------------------------------------
# An error in one script does not stop the others, the failed ones are listed
# at the end (and the exit status is set). The scripts clear the global
# environment, so the state of the run is kept within the function.
runScripts = function(files) {
    failed = character(0)
    for (f in files) {
        cat("-", f, "\n")
        tryCatch(source(f), error = function(e) {
            cat("  ERROR:", conditionMessage(e), "\n")
            failed <<- c(failed, f)
        })
    }
    failed
}

c.failed = runScripts(file.path(path, c.scripts))

if (length(c.failed) > 0) {
    stop("failed scripts:\n", paste("-", c.failed, collapse = "\n"), call. = FALSE)
}
