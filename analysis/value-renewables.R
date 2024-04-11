library(tidyverse)
rm(list = ls())
#source("load/entsoe/_shared.r")
source("_shared.r")
source("calc/prediction-gas-consumption/_functions.r")

download_ninja = FALSE
download_pv_gis = FALSE


if(download_ninja){
    PV_FILE <- "data/ninja/pv.zip"
    WIND_FILE <- "data/ninja/wind.zip"
    EX_DIR <- "data/ninja/"
    download.file("https://renewables.ninja/downloads/ninja_europe_pv_v1.1.zip",
              PV_FILE)
    download.file("https://renewables.ninja/downloads/ninja_europe_wind_v1.1.zip",
              WIND_FILE)
    unzip(PV_FILE, exdir = EX_DIR)
    unzip(WIND_FILE, exdir = EX_DIR)
}

if(download_pv_gis){

    PV_GIS_OPT = "data/ninja/pv-gis-opt-opt.csv"
    PV_GIS_EAST = "data/ninja/pv-gis-38-east.csv"
    PV_GIS_WEST = "data/ninja/pv-gis-38-west.csv"
    PV_GIS_VERTICAL_EAST = "data/ninja/pv-gis-vertical-east.csv"
    PV_GIS_VERTICAL_WEST = "data/ninja/pv-gis-vertical-west.csv"
    PV_GIS_VERTICAL_NORTH = "data/ninja/pv-gis-vertical-north.csv"
    PV_GIS_VERTICAL_SOUTH = "data/ninja/pv-gis-vertical-south.csv"
    PV_GIS_TWO_AXIS = "data/ninja/pv-gis-two-axis.csv"


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

d.prices.filtered %>%
    mutate(hour = hour(DateTime)) %>%
    group_by(year, country) %>%
    mutate(n = 1:n()) %>%
    ungroup() %>%
    group_by(year, hour, country) %>%
    mutate(mean = mean(mean)) %>%
    ungroup %>%
    group_by(year, country) %>%
    mutate(mean = mean / mean(mean)) %>%
    ungroup() %>%
    ggplot(aes(x = hour, y = mean)) +
    geom_line(aes(col = as.character(year))) +
    facet_wrap(.~country)

### VALUE ###
full_join(d.gen.sel,
          d.prices.filtered,
          by = c("DateTime" = "DateTime",
                 "country" = "country")) %>%
    filter(source %in% c("Solar", "Wind Onshore", "Fossil Gas", "Nuclear")) %>%
    filter(!is.na(source)) %>%
    mutate(value.power = value * mean) %>%
    mutate(month = month(DateTime)) %>%
    mutate(year = year(DateTime)) %>%
    filter(year > 2018) %>%
    group_by(year, month, country, source) %>%
    summarize(value = sum(value.power, na.rm = TRUE) / sum(value, na.rm = TRUE),
              DateTime = min(DateTime)) %>%
    ungroup() %>%
    mutate(Technology = source) %>%
    ggplot(aes(x = DateTime, y = value)) +
    geom_line(aes(col = Technology)) +
    facet_wrap(.~country) +
    xlab("Date") +
    ylab("Value of Electricity (€/MWh)")

### CAP FACT ###
ninja_pv <- read_csv("data/ninja/ninja_pv_europe_v1.1_sarah.csv") %>%
    gather(country, value, -time) %>%
    filter(country %in% COUNTRIES) %>%
    group_by(year(time), country) %>%
    mutate(t = 1:n()) %>%
    ungroup() %>%
    group_by(country, t) %>%
    summarize(pv = mean(value))

ninja_pv %>%
    ggplot(aes(x = t, y = pv)) +
    geom_line(aes(col = country)) +
    facet_wrap(.~country)

ninja_wind <- read_csv("data/ninja/ninja_wind_europe_v1.1_future_nearterm_national.csv") %>%
    gather(country, value, -time) %>%
    filter(country %in% COUNTRIES) %>%
    group_by(year(time), country) %>%
    mutate(t = 1:n()) %>%
    ungroup() %>%
    group_by(country, t) %>%
    summarize(wind = mean(value))

ninja_wind %>%
    ggplot(aes(x = t, y = wind)) +
    geom_line(aes(col = country))


d.prices.filtered <- d.prices.filtered %>%
    group_by(year, country) %>%
    mutate(t = 1:n()) %>%
    ungroup()


max_date <- max(d.prices.filtered$DateTime)

full_join(d.prices.filtered,
          ninja_pv,
          by = c("t" = "t", "country" = "country")) %>%
    full_join(ninja_wind,
              by = c("t" = "t", "country" = "country")) %>%
    gather(source, cap_fact, -DateTime, -year, -mean, -country, -t) %>%
    mutate(value = cap_fact * mean) %>%
    mutate(month = month(DateTime)) %>%
    mutate(year = year(DateTime)) %>%
    filter(year > 2018) %>%
    dplyr::select(year, month, DateTime, source, value, country) %>%
    group_by(year, month, country, source) %>%
    summarize(value = sum(value, na.rm = TRUE),
              DateTime = min(DateTime)) %>%
    ungroup() %>%
    group_by(country, source) %>%
    #mutate(rollvalue = rollsum(value, 12, fill = NA, align = "right") / 1000) %>%
    mutate(rollvalue = value / 1000) %>%
    ungroup() %>%
    #na.omit() %>%
    filter(year < max(year) | (year == max(year) & month < month(max_date))) %>%
    ggplot(aes(x = DateTime, y = rollvalue)) +
    geom_line(aes(col = source)) +
    geom_smooth(aes(col = source), method = "gam") +
    facet_wrap(.~country)







########## different system values for austria
list.csv.files <- c(PV_GIS_OPT,
                    PV_GIS_EAST,
                    PV_GIS_WEST,
                    PV_GIS_VERTICAL_EAST,
                    PV_GIS_VERTICAL_WEST,
                    PV_GIS_VERTICAL_NORTH,
                    PV_GIS_VERTICAL_SOUTH,
                    PV_GIS_TWO_AXIS)

d.pv.gis <- read_csv(list_csv_files, skip=19, id="type") %>%
    mutate(time=ymd_hm(time)) %>%
    mutate(type = str_remove(type, "pv-gis")) %>%
    mutate(type = str_remove(type, "\\.csv")) %>%
    mutate(type = str_remove(type, "data")) %>%
    mutate(type = str_remove(type, "ninja")) %>%
    mutate(type = str_remove_all(type, "\\\\")) %>%
    na.omit()



### test on pv_gis
d.pv.gis %>%
    group_by(year = year(time), month=month(time)) %>%
    summarize(P=mean(P)) %>%
    ggplot(aes(x=year, y=P)) +
    geom_bar(stat="identity") +
    facet_wrap(.~month)

d.pv.gis %>%
    filter(year(time) == 2018) %>%
    group_by(month=month(time),time=hour(time), type) %>%
    summarize(P=mean(P)) %>%
    ggplot(aes(x=time, y=P)) +
    geom_line(aes(col=type)) +
    facet_wrap(.~month)

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


d.pv.gis.2018 %>%
    group_by(month=month(time),time=hour(time), type) %>%
    summarize(P=mean(P)) %>%
    ggplot(aes(x=time, y=P)) +
    geom_line(aes(col=type)) +
    facet_wrap(.~month)



d.join.prices.pv <- full_join(d.prices.filtered %>%
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

d.join.prices.pv %>%
    filter(year > 2022) %>%
    ggplot(aes(x = DateTime, y = rollvalue)) +
    geom_line(aes(col = type)) +
    #geom_smooth(aes(col = type), method = "gam")
    ylab("Income for 1kw_peak (€/Month)")

d.join.prices.pv %>%
    group_by(year, type) %>%
    mutate(c_value=cumsum(rollvalue)) %>%
    ungroup() %>%
    ggplot(aes(x = month(DateTime), y = c_value)) +
    geom_line(aes(col = type)) +
    facet_wrap(.~year) +
    theme_bw() +
    xlab("Month") +
    ylab("Cumulative income (€/kw_peak)")

d.join.prices.pv %>%
    group_by(type) %>%
    mutate(c_value=cumsum(rollvalue)) %>%
    ungroup() %>%
    ggplot(aes(x = DateTime, y = c_value)) +
    geom_line(aes(col = type)) +
    theme_bw() +
    xlab("Month") +
    ylab("Cumulative income (€/kw_peak)")











