library(tidyverse)
rm(list = ls())
#source("load/entsoe/_shared.r")
source("_shared.r")
source("calc/prediction-gas-consumption/_functions.r")

download_ninja = TRUE
download_pv_gis = TRUE

PV_FILE_NINJA <- "data/ninja/pv.zip"
PV_GIS_OPT = "data/ninja/pv-gis-opt-opt.csv"
PV_GIS_EAST = "data/ninja/pv-gis-38-east.csv"
PV_GIS_WEST = "data/ninja/pv-gis-38-west.csv"
PV_GIS_VERTICAL_EAST = "data/ninja/pv-gis-vertical-east.csv"
PV_GIS_VERTICAL_WEST = "data/ninja/pv-gis-vertical-west.csv"
PV_GIS_VERTICAL_NORTH = "data/ninja/pv-gis-vertical-north.csv"
PV_GIS_VERTICAL_SOUTH = "data/ninja/pv-gis-vertical-south.csv"
PV_GIS_TWO_AXIS = "data/ninja/pv-gis-two-axis.csv"

if(file.exists(PV_FILE_NINJA)) {
    download_ninja = FALSE
}

if(file.exists(PV_GIS_OPT)) {
    download_pv_gis = FALSE
}

if(download_ninja){
    PV_FILE <- PV_FILE_NINJA
    WIND_FILE <- "data/ninja/wind.zip"
    EX_DIR <- "data/ninja/"
    download.file("https://renewables.ninja/downloads/ninja_europe_pv_v1.1.zip",
              PV_FILE)

    options(timeout=120)
    download.file("https://renewables.ninja/downloads/ninja_europe_wind_v1.1.zip",
              WIND_FILE)
    unzip(PV_FILE, exdir = EX_DIR)
    unzip(WIND_FILE, exdir = EX_DIR)
}

if(download_pv_gis){

    download.file("https://re.jrc.ec.europa.eu/api/v5_2/seriescalc?pvcalculation=1&peakpower=1&loss=0.1&lat=48.32&lon=16.54&outputformat=csv&outputformat=1&optimalangles=1",
                  PV_GIS_OPT)
    download.file("https://re.jrc.ec.europa.eu/api/v5_2/seriescalc?pvcalculation=1&peakpower=1&loss=0.1&lat=48.32&lon=16.54&outputformat=csv&outputformat=1&angle=38&aspect=-90",
                  PV_GIS_EAST)
    download.file("https://re.jrc.ec.europa.eu/api/v5_2/seriescalc?pvcalculation=1&peakpower=1&loss=0.1&lat=48.32&lon=16.54&outputformat=csv&outputformat=1&angle=38&aspect=90",
                  PV_GIS_WEST)
    download.file("https://re.jrc.ec.europa.eu/api/v5_2/seriescalc?pvcalculation=1&peakpower=1&loss=0.1&lat=48.32&lon=16.54&outputformat=csv&outputformat=1&angle=90&aspect=-90",
                  PV_GIS_VERTICAL_EAST)
    download.file("https://re.jrc.ec.europa.eu/api/v5_2/seriescalc?pvcalculation=1&peakpower=1&loss=0.1&lat=48.32&lon=16.54&outputformat=csv&outputformat=1&angle=90&aspect=90",
                  PV_GIS_VERTICAL_WEST)

    download.file("https://re.jrc.ec.europa.eu/api/v5_2/seriescalc?pvcalculation=1&peakpower=1&loss=0.1&lat=48.32&lon=16.54&outputformat=csv&outputformat=1&angle=90&aspect=0",
                  PV_GIS_VERTICAL_SOUTH)

    download.file("https://re.jrc.ec.europa.eu/api/v5_2/seriescalc?pvcalculation=1&peakpower=1&loss=0.1&lat=48.32&lon=16.54&outputformat=csv&outputformat=1&angle=90&aspect=180",
                  PV_GIS_VERTICAL_NORTH)

    download.file("https://re.jrc.ec.europa.eu/api/v5_2/seriescalc?pvcalculation=1&peakpower=1&loss=0.1&lat=48.32&lon=16.54&outputformat=csv&outputformat=1&trackingtype=2",
                  PV_GIS_TWO_AXIS)

}

COUNTRIES <- c("AT", "ES", "DE", "FR")

d.generation = loadFromStorage(id = "electricity-generation-hourly")

d.prices = loadFromStorage(id = "electricity-price-entsoe-hourly") %>%
    mutate(country = substr(AreaName, 1,2))

d.gen.sel <- d.generation %>%
    filter(country %in% COUNTRIES) %>%
    group_by(year = year(DateTime), source, country) %>%
    mutate(hour_of_year = 1:n()) %>%
    ungroup()

d.prices.filtered <- d.prices %>%
    mutate(year = year(DateTime)) %>%
    filter(ResolutionCode == "PT60M") %>%
    filter(year > 2018) %>%
    filter(country %in% COUNTRIES) %>%
    arrange(DateTime) %>%
    dplyr::select(DateTime, year, mean, country)

### CAP FACT AVERAGE ###
ninja_pv <- read_csv("data/ninja/ninja_pv_europe_v1.1_sarah.csv") %>%
    gather(country, value, -time) %>%
    filter(country %in% COUNTRIES) %>%
    group_by(year(time), country) %>%
    mutate(t = 1:n()) %>%
    ungroup() %>%
    group_by(country, t) %>%
    summarize(pv = mean(value))

ninja_wind <- read_csv("data/ninja/ninja_wind_europe_v1.1_future_nearterm_national.csv") %>%
    gather(country, value, -time) %>%
    filter(country %in% COUNTRIES) %>%
    group_by(year(time), country) %>%
    mutate(t = 1:n()) %>%
    ungroup() %>%
    group_by(country, t) %>%
    summarize(wind = mean(value))

d.prices.filtered <- d.prices.filtered %>%
    group_by(year, country) %>%
    mutate(t = 1:n()) %>%
    ungroup()

max_date <- max(d.prices.filtered$DateTime)

########## different pv system values for austria
list.csv.files <- c(PV_GIS_OPT,
                    PV_GIS_EAST,
                    PV_GIS_WEST,
                    PV_GIS_VERTICAL_EAST,
                    PV_GIS_VERTICAL_WEST,
                    PV_GIS_VERTICAL_NORTH,
                    PV_GIS_VERTICAL_SOUTH,
                    PV_GIS_TWO_AXIS)

d.pv.gis <- read_csv(list.csv.files, skip=19, id="type") %>%
    mutate(time=ymd_hm(time)) %>%
    mutate(type = str_remove(type, "pv-gis")) %>%
    mutate(type = str_remove(type, "\\.csv")) %>%
    mutate(type = str_remove(type, "data")) %>%
    mutate(type = str_remove(type, "ninja")) %>%
    mutate(type = str_remove_all(type, "\\\\")) %>%
    na.omit()

d.pv.gis.2018 = d.pv.gis %>%
    filter(year(time) == 2018) %>%
    dplyr::select(time, type, P) %>%
    spread(type, P) %>%
    mutate(vertical.north.south = 0.5 * `//-vertical-north` + 0.5 * `//-vertical-south`) %>%
    mutate(vertical.east.west = 0.5 * `//-vertical-east` + 0.5 * `//-vertical-west`) %>%
    mutate(angle.east.west = 0.5 * `//-38-east` + 0.5 * `//-38-west`) %>%
    dplyr::select(time,
                  vertical.north.south,
                  vertical.east.west,
                  angle.east.west,
                  opt.opt=`//-opt-opt`,
                  two.axis=`//-two-axis`) %>%
    gather(type, P, -time) %>%
    group_by(type) %>%
    mutate(t=1:n()) %>%
    ungroup()


d.join.prices.pv = full_join(d.prices.filtered %>%
              filter(country == "AT"),
          d.pv.gis.2018,
          by = c("t" = "t"),
          relationship = "many-to-many") %>%
    mutate(value = P / 1000 * mean) %>%
    mutate(month = month(DateTime)) %>%
    mutate(year = year(DateTime)) %>%
    filter(year > 2018) %>%
    dplyr::select(year, month, DateTime, type, value, country) %>%
    group_by(year, month, country, type) %>%
    summarize(value = sum(value, na.rm = TRUE),
              DateTime = min(DateTime)) %>%
    ungroup() %>%
    group_by(country, type) %>%
    #mutate(rollvalue = rollsum(value, 12, fill = NA, align = "right") / 1000) %>%
    mutate(rollvalue = value / 1000) %>%
    ungroup() %>%
    #na.omit() %>%
    filter(year < max(year) | (year == max(year) & month < month(max_date)))









