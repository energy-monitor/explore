library(tidyverse)
rm(list = ls())
#source("load/entsoe/_shared.r")
source("_shared.r")
source("calc/prediction-gas-consumption/_functions.r")

download_ninja = FALSE



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
    filter(source %in% c("Solar", "Wind Onshore", "Fossil Gas")) %>%
    filter(!is.na(source)) %>%
    mutate(value.power = value * mean) %>%
    mutate(month = month(DateTime)) %>%
    mutate(year = year(DateTime)) %>%
    filter(year > 2022) %>%
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



