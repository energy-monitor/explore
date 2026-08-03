loadPackages(
    'googledrive', 'curl'
)

saveToStorages = function(data, meta, storages = g$storage$default$save) {
    fileName = glue("{meta$id}.{meta$format}")
    if (meta$format == 'csv') {
        fileWriteFunction = function(d, f) fwrite(d, f)
    } else {
        stop(glue("Format type '{meta$format}' not implemented!"))
    }
    for (storage in storages) {
        l(glue("Storage '{storage}':"))
        if (storage == "local") {
            filePath = file.path(g$storage$local$path, fileName)
            l(glue("-> '{filePath}'"), iL = 2)
            fileWriteFunction(data, filePath)
        } else if (storage == "googledrive") {
            fileTemp = tempfile()
            fileWriteFunction(data, fileTemp)
            l(glue("-> '{fileName}'"), iL = 2)
            uploadGoogleDrive(fileTemp, fileName)
        } else if (storage == "sftp") {
            fileTemp = tempfile()
            fileWriteFunction(data, fileTemp)
            l(glue("-> '{sftpUrl(fileName)}'"), iL = 2)
            uploadSftp(fileTemp, fileName)
        } else {
            stop(glue("Storage type '{storage}' not implemented!"))
        }
    }
}

loadFromStorage = function(id, format = 'csv', storage = g$storage$default$load) {
    # format = 'csv'
    # id = "temperature-hdd"
    fileName = glue("{id}.{format}")

    if (format == 'csv') {
        fileReadFunction = function(f) fread(f)
    } else {
        stop(glue("Format type '{format}' not implemented!"))
    }

    if (storage == "local") {
        file = file.path(g$storage$local$path, fileName)
        return(fileReadFunction(file))
    } else if (storage == "googledrive") {
        file = tempfile()
        drive_download(file.path(g$storage$googledrive$path, fileName), file)
        return(fileReadFunction(file))
    } else if (storage == "sftp") {
        file = tempfile()
        downloadSftp(fileName, file)
        return(fileReadFunction(file))
    } else {
        stop(glue("Storage type '{storage}' not implemented!"))
    }
}


uploadGoogleDrive = function(file, fileName) {
    drive_put(file, path = file.path(g$storage$googledrive$path, fileName))
}


# - SFTP -----------------------------------------------------------------------
# Connection settings are read from the `sftp` section of `creds.json`, so that
# the server stays out of the repository. Settings in `config.json`
# (storage$sftp) are used as defaults. Transfers use libcurl's sftp support,
# authentication is public key only.

sftpConfig = function() {
    cfg = g$storage$sftp
    if (is.null(cfg)) cfg = list()
    if (!is.null(g$sftp$params)) cfg = modifyList(cfg, g$sftp$params)
    if (!length(cfg)) {
        stop("SFTP storage is not configured, add an 'sftp' section to 'creds.json'!")
    }
    for (field in c("host", "user", "keyfile", "path")) {
        if (is.null(cfg[[field]]) || !nzchar(cfg[[field]])) {
            stop(glue("SFTP storage is not configured, '{field}' is missing!"))
        }
    }
    cfg$keyfile = path.expand(cfg$keyfile)
    if (!file.exists(cfg$keyfile)) {
        stop(glue("SFTP private key '{cfg$keyfile}' does not exist!"))
    }
    cfg
}

sftpUrl = function(fileName, cfg = sftpConfig()) {
    port = if (is.null(cfg$port)) 22 else cfg$port
    # a path that is not absolute is taken relative to the login home directory
    remoteDir = sub("^\\./", "", cfg$path)
    if (!startsWith(remoteDir, "/")) remoteDir = file.path("/~", remoteDir)
    glue("sftp://{cfg$host}:{port}{remoteDir}/{fileName}")
}

# curl options shared by up- and download, see `curl::curl_options()`
sftpOptions = function(cfg = sftpConfig()) {
    opts = list(
        username = cfg$user,
        ssh_private_keyfile = cfg$keyfile,
        ssh_auth_types = 1 # CURLSSH_AUTH_PUBLICKEY, see `curl_symbols`
    )
    # libssh2 cannot derive the public key from every key type
    keyfilePub = paste0(cfg$keyfile, ".pub")
    if (file.exists(keyfilePub)) opts$ssh_public_keyfile = keyfilePub
    if (!is.null(cfg$keypass) && nzchar(cfg$keypass)) opts$keypasswd = cfg$keypass
    # without a known_hosts file the host key is not verified at all
    if (!is.null(cfg$knownHosts) && nzchar(cfg$knownHosts)) {
        knownHosts = path.expand(cfg$knownHosts)
        if (!file.exists(knownHosts)) {
            stop(glue("SFTP known_hosts file '{knownHosts}' does not exist!"))
        }
        opts$ssh_knownhosts = knownHosts
    }
    opts
}

uploadSftp = function(file, fileName) {
    cfg = sftpConfig()
    opts = sftpOptions(cfg)
    opts$ftp_create_missing_dirs = 1 # applies to sftp as well
    invisible(do.call(curl_upload, c(
        list(file, sftpUrl(fileName, cfg), verbose = FALSE, reuse = FALSE), opts
    )))
}

downloadSftp = function(fileName, file) {
    cfg = sftpConfig()
    curl_fetch_disk(sftpUrl(fileName, cfg), file, do.call(new_handle, sftpOptions(cfg)))
    invisible(file)
}
