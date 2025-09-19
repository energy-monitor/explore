# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/entsoe/_shared.r")
# loadPackages()


# - DOIT -----------------------------------------------------------------------
update.time = now()

d.base = loadEntsoeComb(
    type = "load", month.start = "2025-01", month.end = month.end
)


# token = get_entsoe_token()



# metadata_response = POST(
#     url = glue("{g$entsoe$params$fms_base_url}/listFileMetadata"),
#     add_headers(Authorization = glue("Bearer {token}")),
#     body = toJSON(list(
#         topLevelFolder = "TP_export",
#         typeSpecificAttributeMap = list(
#             path = glue("/TP_export/ActualTotalLoad_6.1.A/")
#         ),
#         sorterList = list(list(
#             key = "lastUpdatedTimestamp",
#             ascending = FALSE
#         )),
#         pageInfo = list(
#             pageIndex = 0,
#             pageSize = 5000
#         )
#     ), auto_unbox = TRUE),
#     content_type("application/json"),
#     timeout(60)
# )


# metadata = content(metadata_response)

# toJSON(metadata$itemList[[1]], pretty = TRUE, auto_unbox = TRUE)
# # toJSON(metadata$itemList[[1]]$content, pretty = TRUE)


# item = metadata$itemList[[1]]

# l.file = list(
#     filename = item$content$filename,
#     folder = item$typeSpecificAttributeMap$path
# )

# file_response = POST(
#     url = glue("{g$entsoe$params$fms_base_url}"),
#     add_headers(Authorization = glue("Bearer {token}")),
#     body = toJSON(list(
#         # folder = l.file$folder, 
#         # filename = l.file$filename, 
#         # lastUpdateTimestamp = "2021-06-02T10:38:15.909Z",
#         fileIdList = I("3974b94d-8556-4fe7-b0a3-45382d4961f3"),
#         topLevelFolder = "TP_export",
#         downloadAsZip = TRUE
#     ), auto_unbox = TRUE),
#     timeout(60)
# )

# "{\"folder\": \"/TP_export/ActualTotalLoad_6.1.A/\", \"filename\": \"2014_12_ActualTotalLoad_6.1.A.csv\",\"topLevelFolder\": \"TP_export\", \"downloadAsZip\": false }"











token = get_entsoe_token()

path = "/TP_export/ActualTotalLoad_6.1.A/"

listfolder.response = POST(
    url = glue("{g$entsoe$params$fms_base_url}/listFolder"),
    add_headers(Authorization = glue("Bearer {token}")),
    body = toJSON(list(
        path = path,
        pageInfo = list(
            pageIndex = 0,
            pageSize = 5000
        )
    ), auto_unbox = TRUE),
    content_type("application/json"),
    timeout(60)
)

status_code(listfolder.response)
listfolder.content = content(listfolder.response)


item = listfolder.content$contentItemList[[1]]
toJSON(item, pretty = TRUE, auto_unbox = TRUE)


getfile.response = POST(
    url = glue("{g$entsoe$params$fms_base_url}/downloadFileContent"),
    add_headers(Authorization = glue("Bearer {token}")),
    body = toJSON(list(
        fileIdList = I(item$fileId),
        # folder = path,
        # filename = item$name,
        topLevelFolder = "TP_export",
        downloadAsZip = TRUE
    ), auto_unbox = TRUE),
    content_type("application/json"),
    timeout(60)
)

status_code(getfile.response)


writeBin(content(getfile.response), "tmp.zip")
