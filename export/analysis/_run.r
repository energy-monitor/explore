# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("_shared.r")
loadPackages(rmarkdown, knitr)


# - CONF -----------------------------------------------------------------------
output.folder = file.path(g$d$wd, "md")

input.file = file.path("analysis", "gas-savings.rmd")
output.file = "gas-savings"


# - RENDER ---------------------------------------------------------------------
opts_knit$set(base.dir = normalizePath(file.path(getwd(), output.folder)))

e = rlang::env()

e$output.file = output.file
e$output.folder = normalizePath(file.path(getwd(), output.folder))

render(
    input = input.file,
    output_dir = output.folder,
    envir = e,
    intermediates_dir = tempdir(),
    knit_root_dir = getwd()
)
