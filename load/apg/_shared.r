# - INIT -----------------------------------------------------------------------
source("_shared.r")

load_apg_capacity_at = function(start_year = 2023, end_year = as.integer(format(Sys.Date(), "%Y")) + 1) {
    url = paste0(
        "https://transparency.apg.at/api/v1/IGCA/Download/English/P1Y/",
        start_year, "-01-01T000000/",
        end_year, "-01-01T000000"
    )

    fread(url, sep = ",")
}
