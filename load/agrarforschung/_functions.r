# Price portal of the Bundesanstalt für Agrarwirtschaft und Bergbauernfragen
# (BAB). `contentId` is the `publicName` of a node of `/api/getprojects`,
# the numeric ids of the same call do not work.
c.agrarforschungUrl = "https://preise.agrarforschung.at/api/getcontent?contentId={contentId}"


getAgrarforschungData = function(contentId) {
    j.raw = read_json(glue(c.agrarforschungUrl))
    if (!is.null(j.raw$error)) stop(glue("agrarforschung: '{contentId}': {j.raw$error}"))
    j.base = j.raw$data

    d.raw = rbindlist(lapply(j.base$records, function(r) data.table(
        date = as.Date(r$ts_start),
        attr = r$attr_code,
        unit = r$unit_name,
        value = r$value
    )))

    list(
        data = d.raw,
        title = j.base$title,
        # the source is delivered as html
        source = trimws(gsub("<[^>]*>", "", j.base$source))
    )
}


# `c.attrs` maps the `attr_code`s of the API to the column names to store
saveAgrarforschungData = function(contentId, name, c.attrs) {
    update.time = now()
    l.raw = getAgrarforschungData(contentId)

    l("'", l.raw$title, "'", iL = 2)
    l(l.raw$source, iL = 2)

    d.base = l.raw$data[attr %in% names(c.attrs), ]
    if (!all(names(c.attrs) %in% d.base$attr))
        stop(glue("agrarforschung: '{contentId}': missing: {paste(setdiff(names(c.attrs), d.base$attr), collapse = ', ')}"))
    if (uniqueN(d.base$unit) != 1)
        stop(glue("agrarforschung: '{contentId}': mixed units: {paste(unique(d.base$unit), collapse = ', ')}"))
    l("unit: ", d.base$unit[1], iL = 2)

    d.full = dcast(d.base, date ~ attr, value.var = "value")
    setnames(d.full, names(c.attrs), unname(c.attrs))
    setcolorder(d.full, c("date", unname(c.attrs)))

    # - STORAGE ----------------------------------------------------------------
    saveToStorages(d.full[order(date), ], list(
        id = name,
        source = "agrarforschung",
        format = "csv",
        update.time = update.time
    ))

    NULL
}
