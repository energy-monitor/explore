rm(list = ls())
source("_shared.r")
library(tidyverse)
COLORS = c("#c3423f", "#2aace3", "#b1d327")


###### An analysis of 2022 climate data for posting on Twitter #####


### Load and plot climate data ###
d.temperature.hdd = loadFromStorage("temperature-hdd")

### winter analysis ###
d.temperature.winter = d.temperature.hdd |>
    filter(month(date) > 9) |>
    filter(year(date) > 1969) |>
    group_by(year = year(date)) |>
    summarize(temp = mean(temp)) |>
    mutate(t = 1:n())

d.temperature.winter |>
    mutate(is.2022 = ifelse(year == 2022, "yes", "no")) |>
    ggplot(aes(x = year, y = temp)) +
    geom_point(aes(col = is.2022)) +
    theme_bw() +
    scale_color_manual(values = COLORS[c(3, 1)]) +
    theme_bw() +
    theme(legend.position = "none") +
    xlab("Jahr") +
    ylab("Durschnittstemperatur Oktober - Dezember (°C)")


temp.trend = summary(lm(temp ~ t, d.temperature.winter))$coefficients[2, 1]

temp.trend * 10

d.temperature.winter.corrected = d.temperature.winter |>
    mutate(temp = temp + ((max(t) - t) * temp.trend))

d.temperature.winter.corrected |>
    ggplot(aes(x = year, y = temp)) +
    geom_point() +
    theme_bw()

d.temperature.joined = d.temperature.winter.corrected |>
    mutate(Type = "Corrected") |>
    bind_rows(d.temperature.winter |>
                  mutate(Type = "Observed")) |>
    mutate(smaller.1994 = ifelse(year < 1997, "< 1997", ">=1997"))


d.temperature.joined |>
    group_by(Type, smaller.1994) |>
    summarize(min = min(temp),
              max = max(temp),
              mean = mean(temp))

d.temperature.joined |>
    ggplot(aes(x = Type, y = temp)) +
    geom_boxplot() +
    theme_bw() +
    scale_fill_manual(values = COLORS)

d.temperature.joined |>
    ggplot(aes(x = year, y = temp)) +
    geom_line(aes(col = Type)) +
    scale_color_manual(values = COLORS) +
    theme_bw() +
    xlab("Jahr") +
    ylab("Temperatur(°C)")



d.temperature.hdd.annual = d.temperature.hdd |>
    group_by(year = year(date)) |>
    filter(year < 2023) |>
    summarize(temp = mean(temp)) |>
    mutate(is.2022 = ifelse(year == 2022, "yes", "no")) |>
    mutate(t = year - 1949)

d.temperature.hdd.annual |>
    ggplot(aes(x = year, y = temp)) +
    geom_point(aes(col = is.2022)) +
    #geom_smooth(method = "lm") +
    scale_color_manual(values = c("black", COLORS[1])) +
    xlab("Jahr") +
    ylab("Jährliche Durchschnittstemperatur\n(°C)") +
    theme_bw() +
    theme(legend.position = "none")

d.temperature.hdd.annual.short = d.temperature.hdd.annual |>
    filter(year > 1969)

round(summary(lm(temp ~ t, data = d.temperature.hdd.annual.short))$coefficients[2, 1]*10, 2)
round(confint(lm(temp ~ t, data = d.temperature.hdd.annual.short), "t", 0.95)*10, 2)
summary(lm(temp ~ t, data = d.temperature.hdd.annual.short))$coefficients[2, 1] * 72


d.temperature.hdd.month = d.temperature.hdd |>
    group_by(year = year(date), month = month(date)) |>
    summarize(temp = mean(temp)) |>
    ungroup() |>
    mutate(t = year - 1949)

d.temperature.hdd.month |>
    mutate(last.year = ifelse(year == max(year), "2022", "davor")) |>
    ggplot(aes(x = year, y = temp)) +
    geom_point(aes(col = last.year)) +
    geom_smooth(method = "lm") +
    xlab("Jahr") +
    ylab("Monatliche Durchschnittstemperatur\n(°C)") +
    theme_bw() +
    facet_wrap(. ~ month, scale = "free") +
    theme(legend.position = "none") +
    scale_color_manual(values = c(COLORS[1], "black"))

l = d.temperature.hdd.month |>
    filter(year > 1969) |>
    ungroup() |>
    group_by(month) |>
    group_map(~ lm(temp ~ t, data = .))

confints = t(mapply(confint, l, list("t"), list(0.95))) |>
    as_tibble()

names(confints) = c("lower", "upper")

l.summary = mapply(function(x){summary(x)$coefficients}, l, SIMPLIFY = FALSE)

mapply(as.data.frame, l.summary, SIMPLIFY = FALSE) |>
    bind_rows() |>
    slice(which(row_number() %% 2 == 0)) |>
    mutate(month = 1:12) |>
    bind_cols(confints) |>
    ggplot(aes(x = month, y = 72 * Estimate)) +
    geom_bar(stat = "identity", fill = COLORS[1]) +
    geom_errorbar(aes(ymin = 72 * lower, ymax = 72 * upper), width=.2,
                  position=position_dodge(.9)) +
    scale_x_continuous(label = ~ scales::comma(.x, accuracy = 1, big.mark = "")) +
    theme_bw() +
    xlab("Monat") +
    ylab("Temperaturzunahme 72 Jahre (°C)")

d.temperature.hdd.max = d.temperature.hdd |>
    mutate(day = yday(date)) |>
    mutate(year = year(date)) |>
    group_by(day) |>
    mutate(is.2022 = ifelse(year == 2022, "2022", "< 2022")) |>
    mutate(is.2022.max = ifelse(year == 2022 & temp == max(temp), "2022 max", is.2022))

d.temperature.hdd.max |>
    group_by(is.2022.max) |>
    summarize(n = n())

d.temperature.hdd.max |>
    ggplot(aes(x = day, y = temp)) +
    geom_line(aes(col = is.2022, alpha = is.2022)) +
    geom_point(aes(col = is.2022.max, size = is.2022.max, alpha = is.2022.max)) +
    theme_bw() +
    scale_color_manual(values = rev(COLORS)) +
    scale_size_manual(values = c(0.1, 1.5, 1.5)) +
    scale_alpha_manual(values = c(0.2, 1, 1)) +
    xlab("Tag") +
    ylab("Temperatur (°C)") +
    theme(legend.title = element_blank())

