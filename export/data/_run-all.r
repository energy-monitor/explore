# - FILES ----------------------------------------------------------------------
path = "export/data"
c.scripts = grep("^[0-9a-z].*", list.files(path), value = TRUE)

# - RUN IT ---------------------------------------------------------------------
invisible(lapply(file.path(path, c.scripts), function(f) {
    cat('-', f, '\n'); source(f)
}))
