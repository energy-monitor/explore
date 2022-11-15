# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("calc/prediction-gas-consumption/_shared.r")
library(tidyverse)


# - CONF -----------------------------------------------------------------------
update.data = FALSE


# - LOAD -----------------------------------------------------------------------
d.base = loadBase(update.data)
d.economic.activity = loadFromStorage(id = "economic-activity")[, .(
    year, month, economic.activity = value
)]


#################test!!!
months = tibble(number = c(1:12),
                name = c("Jänner", "Februar", "März", "April", "Mai", "Juni", "Juli",
                         "August", "September", "Oktober", "November", "Dezember"))

days_of_months <- c(1, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30)

annotation <- tibble(label=c("Gasverfügbarkeit Winter: Szenario mit russischem Gas", "Gasverfügbarkeit Winter: Szenario ohne russisches Gas"),
                     x = c(42, 42),
                     y = c(64,54))

d.base %>%
    mutate(date_shift = date - months(9)) %>%
    mutate(year_shift = year(date_shift)) %>%
    filter(month(date_shift) < 7) %>%
    filter(year_shift > 2018) %>%
    group_by(year_shift) %>%
    mutate(cumvalue = cumsum(value)) %>%
    ungroup() %>%
    mutate(day_shift = yday(date_shift)) %>%
    mutate(`Winter half year`=as.character(year_shift)) %>%
    ggplot(aes(x=day_shift, y=cumvalue)) +
    geom_line(aes(col=`Winter half year`), size = 1) +
    theme_bw() +
    xlab("Monat") +
    ylab("Kumulativer Gaskonsum Winterperiode (Oktober - März)\n (TWh)") +
    scale_x_continuous(breaks=cumsum(days_of_months),
                       labels=months$number) +
    geom_abline(intercept = 62, slope = 0, linetype = 2) +
    geom_abline(intercept = 52, slope = 0, linetype = 2) +
    geom_label(data=annotation, aes( x=x, y=y, label=label),                 ,
               size=3 , fontface="bold" )






