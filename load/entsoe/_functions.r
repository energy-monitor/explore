# OAuth2 authentication for File Library API
get_entsoe_token = function() {
    token.cache =g$entsoe$token.cache
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
download_entsoe_files_batch = function(folder_path, target_filenames, data_folder, d_toc) {
    print(d_toc)
    # Get OAuth2 token
    token = get_entsoe_token()
    if (is.null(token)) {
        l(glue("- Failed to obtain authentication token"), iL=iL+1)
        return(NULL)
    }

    tryCatch({
        # Step 1: List folder contents
        listfolder_response = POST(
            url = glue("{g$entsoe$params$fms_base_url}/listFolder"),
            add_headers(Authorization = glue("Bearer {token}")),
            body = toJSON(list(
                path = glue("/{folder_path}/"),
                pageInfo = list(
                    pageIndex = 0,
                    pageSize = 5000
                )
            ), auto_unbox = TRUE),
            content_type("application/json"),
            timeout(60)
        )

        if (status_code(listfolder_response) != 200) {
            warning(glue("List folder failed: {status_code(listfolder_response)} - {content(listfolder_response, 'text')}"))
            return(list(success = FALSE, updated_toc = d_toc))
        }

        folder_content = content(listfolder_response)

        # Step 2: Check which files need downloading
        files_to_download = list()
        updated_toc = d_toc

        for (item in folder_content$contentItemList) {
            filename = item$name

            # Check if this is one of our target files
            if (filename %in% target_filenames) {
                local_filepath = file.path(data_folder, filename)
                last_modified = as.POSIXct(item$lastUpdatedTimestamp, format="%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")

                # Check TOC for existing entry
                toc_entry = d_toc[name == filename]
                should_download = TRUE

                if (nrow(toc_entry) == 1 && file.exists(local_filepath)) {
                    # Compare timestamps
                    if (!is.na(toc_entry$last_modified)) {
                        if (toc_entry$last_modified == last_modified) {
                            should_download = FALSE
                        } else if (toc_entry$last_modified > last_modified){
                            warning(glue("Strange: toc timestamp ({toc_entry$last_modified}) newer than source ({last_modified})"))
                        }
                    }
                }

                if (should_download) {
                    files_to_download[[length(files_to_download) + 1]] = list(
                        fileId = item$fileId,
                        filename = filename,
                        lastModified = last_modified,
                        localPath = local_filepath
                    )
                }
            }
        }

        l(glue("Updating {length(files_to_download)} files ..."))

        if (length(files_to_download) == 0) {
            return(list(success = TRUE, updated_toc = updated_toc))
        }

        # Step 3: Batch download files
        file_ids = sapply(files_to_download, function(x) x$fileId)

        download_response = POST(
            url = glue("{g$entsoe$params$fms_base_url}/downloadFileContent"),
            add_headers(Authorization = glue("Bearer {token}")),
            body = toJSON(list(
                fileIdList = I(file_ids),
                topLevelFolder = "TP_export",
                downloadAsZip = TRUE
            ), auto_unbox = TRUE),
            content_type("application/json"),
            timeout(300)  # Longer timeout for batch download
        )

        if (status_code(download_response) != 200) {
            warning(glue("Batch download failed: {status_code(download_response)} - {content(download_response, 'text')}"))
            return(list(success = FALSE, updated_toc = updated_toc))
        }

        # Step 4: Save and extract ZIP file
        temp_zip = tempfile(fileext = ".zip")
        writeBin(content(download_response, "raw"), temp_zip)

        # Extract ZIP to temporary directory
        temp_dir = tempdir()
        unzip(temp_zip, exdir = temp_dir, overwrite = TRUE)

        # Step 5: Move extracted files to final locations and update TOC
        for (file_info in files_to_download) {
            extracted_file = file.path(temp_dir, file_info$filename)

            if (file.exists(extracted_file)) {
                file.copy(extracted_file, file_info$localPath, overwrite = TRUE)
                file.remove(extracted_file)

                # Update TOC
                updated_toc = rbind(
                    updated_toc[name != file_info$filename],
                    data.table(
                        name = file_info$filename,
                        check = as.character(file.info(file_info$localPath)$size),
                        last_modified = file_info$lastModified
                    )
                )
            }
        }

        # Cleanup
        file.remove(temp_zip)
        return(list(success = TRUE, updated_toc = updated_toc))

    }, error = function(e) {
        warning(glue("Error in batch download: {e$message}"))
        return(list(success = FALSE, updated_toc = d_toc))
    })
}

# New File Library-based function (alternative to SFTP-based loadEntsoeComb)
loadEntsoeComb = function(
    type, check.updates = TRUE,
    month.start = "2022-01", month.end = "2022-08",
    data.folder = g$d$entsoe, iL = 1
) {
    months = ymd(seq(ym(month.start), ym(month.end), by = "month"))

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
        l(glue('CHECK AND DOWNLOAD IF NECESSARY (via File Library API)'), iL=iL)

        toc.file = file.path(data.folder, glue("{type}-fl-toc.csv"))  # Different TOC file to avoid conflicts
        d.toc = if (file.exists(toc.file)) fread(toc.file) else data.table(name = character(0), check = character(0), last_modified = character(0))

        # Build list of target filenames for all months
        target_filenames = sapply(months, function(m) {
            month2.str = format.Date(ymd(m), "%m")
            year.str = year(m)
            glue("{year.str}_{month2.str}_{folder_name}.csv")
        })

        l(glue('- checking {length(target_filenames)} files for updates'), iL=iL+1)

        # Batch download all files that need updating
        download_result = download_entsoe_files_batch(
            folder_path = glue("TP_export/{folder_name}"),
            target_filenames = target_filenames,
            data_folder = data.folder,
            d_toc = d.toc
        )

        if (download_result$success) {
            d.toc = download_result$updated_toc
            l(glue('- batch download completed'), iL=iL+1)
        } else {
            l(glue('- batch download failed'), iL=iL+1)
        }

        fwrite(d.toc, toc.file)
    }

    l(glue('LOADING'), iL=iL)
    d.full = rbindlist(lapply(months, function(m) {
        month2.str = format.Date(m, "%m")
        year.str = year(m)

        l(glue('month {year.str}-{month2.str}'), iL=iL+1)

        # Use the same naming convention as SFTP
        folder_name = folder_map[[type]]
        entsoe.filename = glue("{year.str}_{month2.str}_{folder_name}.csv")
        local.filepath = file.path(data.folder, entsoe.filename)

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
