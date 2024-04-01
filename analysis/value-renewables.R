library(tidyverse)
rm(list = ls())
#source("load/entsoe/_shared.r")
source("_shared.r")
source("calc/prediction-gas-consumption/_functions.r")

d.generation = loadFromStorage(id = "electricity-generation-hourly")
d.prices = loadFromStorage(id = "electricity-price-entsoe-hourly") %>%
    mutate(country = substr(AreaName, 1,2))

d.cap.fact <- d.generation %>%
    filter(country %in% c("AT", "ES")) %>%
    group_by(year = year(DateTime), source, country) %>%
    mutate(cap_fact = value / max(value)) %>%
    mutate(hour_of_year = 1:n()) %>%
    ungroup()

d.cap.fact %>%
    filter(source %in% c("Solar", "Wind Onshore")) %>%
    filter(year > 2014) %>%
    ggplot(aes(x = DateTime, y = cap_fact)) +
    geom_smooth(aes(col = source)) +
    facet_wrap(. ~ country)

d.prices.filtered <- d.prices %>%
    mutate(year = year(DateTime)) %>%
    filter(ResolutionCode == "PT60M") %>%
    filter(year > 2018) %>%
    filter(AreaName %in% c("AT BZN", "ES BZN")) %>%
    arrange(DateTime)

full_join(d.cap.fact,
          d.prices.filtered,
          by = c("DateTime" = "DateTime", "country" = "country")) %>%
    filter(source %in% c("Solar", "Wind Onshore")) %>%
    mutate(value = cap_fact * mean) %>%
    mutate(month = month(DateTime)) %>%
    mutate(year = year(DateTime)) %>%
    filter(year > 2018) %>%
    dplyr::select(year, month, DateTime, source, value, country) %>%
    filter(! (source %in% c("Fossil Hard coal",
                            "Other",
                            "Other renewable",
                            "Fossil Oil"))) %>%
    filter(!is.na(source)) %>%
    group_by(year, month, country, source) %>%
    summarize(value = sum(value, na.rm = TRUE),
              DateTime = min(DateTime)) %>%
    ungroup() %>%
    group_by(country, source) %>%
    mutate(rollvalue = rollsum(value, 12, fill = NA, align = "right") / 1000) %>%
    ungroup() %>%
    #na.omit() %>%
    ggplot(aes(x = DateTime, y = rollvalue)) +
    geom_line(aes(col = source)) +
    facet_wrap(.~country)

full_join(d.cap.fact,
          d.prices.filtered,
          by = c("DateTime" = "DateTime",
                 "country" = "country")) %>%
    filter(! (source %in% c("Fossil Hard coal",
                            "Other",
                            "Other renewable",
                            "Fossil Oil",
                            "Geothermal",
                            "Waste",
                            "Biomass"))) %>%
    filter(source %in% c("Solar", "Wind Onshore", "Fossil Gas")) %>%
    filter(!is.na(source)) %>%
    mutate(value.power = value * mean) %>%
    mutate(month = month(DateTime)) %>%
    mutate(year = year(DateTime)) %>%
    filter(year > 2018) %>%

#    dplyr::select(year, month, DateTime, source, value) %>%
    group_by(year, month, country, source) %>%
    summarize(value = sum(value.power, na.rm = TRUE) / sum(value, na.rm = TRUE),
              DateTime = min(DateTime)) %>%
    ungroup() %>%
    ggplot(aes(x = DateTime, y = value)) +
    geom_line(aes(col = source)) +
    facet_wrap(.~country)
    #geom_smooth(aes(col = source))
    #facet_wrap(. ~ source) +
    #ylim(c(0, 100))

