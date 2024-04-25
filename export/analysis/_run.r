#!/usr/bin/env Rscript
# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("_shared.r")
loadPackages(rmarkdown, knitr)


# - CONF -----------------------------------------------------------------------
args = commandArgs(trailingOnly=TRUE)
# args = "value-renewables" # "gas-storage"
if (length(args) == 0) {
    stop("Specify the markdown to render as arg", call. = FALSE)
} else if (length(args) == 1) {
    print(args)
    name = args[1]
    input.file = file.path("analysis", glue("{name}.rmd"))
}

output.file = name
output.folder = file.path(g$d$wd, "md")


# - RENDER ---------------------------------------------------------------------
opts_knit$set(base.dir = normalizePath(file.path(getwd(), output.folder)))

e = rlang::env()

e$output.file = output.file
e$output.folder = normalizePath(file.path(getwd(), output.folder))


# print(list(
#     output.folder = normalizePath(file.path(getwd(), output.folder)),
#     output.file = output.file,
#     wd = getwd()
# ))

render(
    input = input.file,
    output_dir = output.folder,
    envir = e,
    intermediates_dir = tempdir(),
    knit_root_dir = getwd()
)
