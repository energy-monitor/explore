# OAuth2 authentication for File Library API
get_entsoe_token = function() {
    token.cache = g$entsoe$token.cache
    # Check if we have a cached token that's still valid
    if (!is.null(token.cache)) {
        if (Sys.time() < token.cache$expires_at) {
            return(token.cache$token)
        }
    }

    # Get fresh token from Keycloak
    tryCatch({
        response = POST(
            url = g$entsoe$params$keycloak_url,
            body = list(
                grant_type = "password",
                client_id = "tp-fms-public",
                username = g$entsoe$params$user,
                password = g$entsoe$params$pass
            ),
            encode = "form",
            timeout(30)
        )

        if (status_code(response) == 200) {
            token.data = content(response)
            token = token.data$access_token

            # Cache token with expiration
            g$entsoe$token.cache = list(
                token = token,
                expires_at = Sys.time() + seconds(token.data$expires_in - 60)  # 60s buffer
            )

            return(token)
        } else {
            warning(glue("Failed to get token: {status_code(response)}"))
            return(NULL)
        }
    }, error = function(e) {
        warning(glue("Error getting token: {e$message}"))
        return(NULL)
    })
}

# Download files from File Library API (batch download with timestamp checking)
download_entsoe_files_batch = function(query.path, c.target.files, f.data, d.toc, iL) {
    # Get OAuth2 token
    token = get_entsoe_token()
    if (is.null(token)) {
        l(glue("- Failed to obtain authentication token"), iL=iL+1)
        return(NULL)
    }

    tryCatch({
        # Step 1: List folder contents
        resp.list = POST(
            url = glue("{g$entsoe$params$fms_base_url}/listFolder"),
            add_headers(Authorization = glue("Bearer {token}")),
            body = toJSON(list(
                path = glue("/{query.path}/"),
                pageInfo = list(
                    pageIndex = 0,
                    pageSize = 5000
                )
            ), auto_unbox = TRUE),
            content_type("application/json"),
            timeout(60)
        )

        if (status_code(resp.list) != 200) {
            warning(glue("List folder failed: {status_code(resp.list)} - {content(resp.list, 'text')}"))
            return(list(success = FALSE, d.toc = d.toc))
        }

        folder_content = content(resp.list)

        # Step 2: Check which files need downloading
        l.files.to.download = list()
        for (item in folder_content$contentItemList) {
            filename = item$name

            # Check if this is one of our target files
            if (filename %in% c.target.files) {
                f.file = file.path(f.data, filename)
                last_modified = as.POSIXct(item$lastUpdatedTimestamp, format="%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")

                # Check TOC for existing entry
                should_download = TRUE

                if (!is.null(d.toc)) {
                    d.toc.row = d.toc[name == filename]
                    if (nrow(d.toc.row) == 1 && file.exists(f.file)) {
                        # l(glue("toc timestamp ({d.toc.row$last_modified}) vs source ({last_modified})"))
                        # Compare timestamps
                        if (!is.na(d.toc.row$last_modified)) {
                            if (d.toc.row$last_modified == last_modified) {
                                should_download = FALSE
                            } else if (d.toc.row$last_modified > last_modified){
                                warning(glue("Strange: toc timestamp ({d.toc.row$last_modified}) newer than source ({last_modified})"))
                            }
                        }
                    }
                }
                if (should_download) {
                    l.files.to.download[[length(l.files.to.download) + 1]] = list(
                        fileId = item$fileId,
                        filename = filename,
                        lastModified = last_modified,
                        localPath = f.file
                    )
                }
            }
        }

        l(glue("- updating {length(l.files.to.download)} files ..."), iL = iL)

        if (length(l.files.to.download) == 0) {
            return(list(success = TRUE, d.toc.updated = d.toc))
        }


        chunk.size = 100
        c.file.ids = sapply(l.files.to.download, function(x) x$fileId)
        
        for (ids in split(c.file.ids, ceiling(seq_along(c.file.ids)/chunk.size))) {
            resp.download = POST(
                url = glue("{g$entsoe$params$fms_base_url}/downloadFileContent"),
                add_headers(Authorization = glue("Bearer {token}")),
                body = toJSON(list(
                    fileIdList = I(ids),
                    topLevelFolder = "TP_export",
                    downloadAsZip = TRUE
                ), auto_unbox = TRUE),
                content_type("application/json"),
                timeout(300)  # Longer timeout for batch download
            )

            if (status_code(resp.download) != 200) {
                warning(glue("Batch download failed: {status_code(resp.download)} - {content(resp.download, 'text')}"))
                return(list(success = FALSE, d.toc.updated = d.toc))
            }

            t.zip = tempfile(fileext = ".zip")
            writeBin(content(resp.download, "raw"), t.zip)
            t.dir = tempdir()
            unzip(t.zip, exdir = t.dir, overwrite = TRUE)
            file.remove(t.zip)
        }

        for (f in l.files.to.download) {
            extracted_file = file.path(t.dir, f$filename)

            if (file.exists(extracted_file)) {
                file.copy(extracted_file, f$localPath, overwrite = TRUE)
                file.remove(extracted_file)

                # Update TOC
                d.new.row = data.table(
                    name = f$filename,
                    check = as.character(file.info(f$localPath)$size),
                    last_modified = f$lastModified
                )

                if (is.null(d.toc)) {
                    d.toc = d.new.row
                }

                d.toc = rbind(d.toc[name != f$filename], d.new.row)
            }
        }

        # Cleanup
        
        return(list(success = TRUE, d.toc.updated = d.toc))

    }, error = function(e) {
        warning(glue("Error in batch download: {e$message}"))
        return(list(success = FALSE, d.toc.updated = d.toc))
    })
}

# New File Library-based function (alternative to SFTP-based loadEntsoeComb)
loadEntsoeComb = function(
    type, check.updates = TRUE,
    month.start = "2022-01", month.end = "2022-08",
    f.data = g$d$entsoe, iL = 1
) {
    c.months = ymd(seq(ym(month.start), ym(month.end), by = "month"))

    folder_map = list(
        generation = "AggregatedGenerationPerType_16.1.B_C",
        load = "ActualTotalLoad_6.1.A",
        dayAheadPrices = "EnergyPrices_12.1.D",
        netPositions = "ImplicitAllocationsNetPositions_12.1.E",
        physicalFlows = "PhysicalFlows_12.1.G"
    )

    folder_name = folder_map[[type]]
    if (is.null(folder_name)) {
        stop(glue("Unknown data type: {type}. Available types: {paste(names(folder_map), collapse=', ')}"))
    }

    if (check.updates) {
        l(glue('- looking for ENTSOE updates'), iL = iL)

        f.toc = file.path(f.data, glue("{type}-toc.csv"))  
        d.toc = NULL
        if (file.exists(f.toc)) {
            d.toc = fread(f.toc)
        }

        # Build list of target filenames for all months
        c.target.files = sapply(c.months, function(m) {
            month2.str = format.Date(ymd(m), "%m")
            year.str = year(m)
            glue("{year.str}_{month2.str}_{folder_name}.csv")
        })

        l(glue('- checking {length(c.target.files)} files for updates'), iL = iL + 1)

        # Batch download all files that need updating

        download_result = download_entsoe_files_batch(
            query.path = glue("TP_export/{folder_name}"),
            c.target.files = c.target.files,
            f.data = f.data,
            d.toc = d.toc, iL = iL + 1
        )

        if (download_result$success) {
            d.toc = download_result$d.toc.updated
            l(glue('  completed'), iL=iL+1)
        } else {
            l(glue('  FAILED'), iL=iL+1)
        }

        fwrite(d.toc, f.toc)
    }

    l(glue('- loading and concatenate files'), iL = iL)
    d.full = rbindlist(lapply(c.months, function(m) {
        month2.str = format.Date(m, "%m")
        year.str = year(m)

        # l(glue('month {year.str}-{month2.str}'), iL=iL+1)

        # Use the same naming convention as SFTP
        folder_name = folder_map[[type]]
        entsoe.filename = glue("{year.str}_{month2.str}_{folder_name}.csv")
        local.filepath = file.path(f.data, entsoe.filename)

        if (file.exists(local.filepath)) {
            fread(local.filepath)
        } else {
            l(glue('- file not found: {entsoe.filename}'), iL=iL+2)
            data.table()
        }
    }))

    # Apply same post-processing as original function
    names(d.full)[grepl("DateTime", names(d.full))] = "DateTime"

    d.full[, `:=`(
        DateTimeHourly = DateTime,
        DateTime = as.Date(DateTime)
        # UpdateTime = NULL
    )]

    d.full[]
}
