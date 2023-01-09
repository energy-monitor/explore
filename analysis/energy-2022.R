rm(list = ls())
source("_shared.r")
library(tidyverse)
COLORS = c("#c3423f", "#2aace3", "#b1d327")


###### An analysis of 2022 energy data for posting on Twitter #####

### Functions ###

url_all_results = function (original_url){
    if (!grepl("\\?", original_url)) {
        original_url <- paste0(original_url, "?format=json")
    }
    total_results <- original_url %>% fromJSON %>% .[[1]] %>%
        .$total
    cat("Generating URL to request all", total_results, "results\n")
    url_with_all_results <- paste0(original_url, "&per_page=",
                                   total_results)
    url_with_all_results
}

adjust.prices.inflation = function(d, to.year = 2021){


    inflation_url <- paste0("http://api.worldbank.org/v2/country/",
                            "AT", "/indicator/FP.CPI.TOTL")
    inflation_url <- inflation_url %>% url_all_results
    inflation_data <- inflation_url %>% fromJSON(.)
    inflation_data <- inflation_data[[2]]
    inflation_data = inflation_data |>
        dplyr::select(date, value) |>
        arrange(date) |>
        mutate(date = as.numeric(date)) |>
        bind_rows(tibble(date = c(2022), value = c(135.3))) |>
        dplyr::select(year = date, inflation.index = value)


    d = d |>
        mutate(year = year(date)) |>
        merge(inflation_data, by = "year") |>
        mutate(price.adjusted = price / (inflation.index / 100))

    return(d)
}

plot.annual.quantities = function(d, ylab, filenames = c("")) {
    p = d |>
        group_by(year = year(date)) |>
        summarize(value = sum(value)) |>
        ungroup() |>
        mutate(value = value / mean(value)) |>
        ggplot(aes(x = year, y = value)) +
        geom_line(size = 1) +
        xlab("Jahr") +
        ylab(ylab)
    plot(p)

    p = d |>
        filter(date > as.Date("2021-01-01") & date  < as.Date("2022-12-31")) |>
        ggplot(aes(x = date, y = value)) +
        geom_line(size = 1) +
        geom_vline(xintercept = as.Date("2022-02-24"), linetype = 2) +
        xlab("Datum") +
        ylab(ylab)

    plot(p)
}


### Load price data ###
d.gas.price = loadFromStorage("price-gas") |>
    adjust.prices.inflation() |>
    dplyr::select(date, price, price.adjusted) |>
    mutate(Type = "(a) Gaspreis\n(€/MWh)") |>
    filter(year(date) > 2012)

d.power.price = loadFromStorage("price-electricity") |>
    mutate(price = base) |>
    adjust.prices.inflation() |>
    dplyr::select(date, price, price.adjusted) |>
    mutate(Type = "(b) Strompreis\n(€/MWh)") |>
    filter(year(date) > 2018)

d.oil.price = loadFromStorage("price-brent") |>
    adjust.prices.inflation() |>
    dplyr::select(date, price, price.adjusted) |>
    mutate(Type = "(c) Ölpreis\n(€/Barrel)")

d.gasoline.price = loadFromStorage("price-gas-oil") |>
    filter(date > as.Date("2006-12-31")) |>
    filter(variable == "euroSuper95") |>
    mutate(price = value) |>
    adjust.prices.inflation() |>
    dplyr::select(date, price, price.adjusted) |>
    mutate(Type = "(d) Benzinpreis\n(€/Liter)") |>
    filter(year(date) > 2007)

d.eua.price = loadFromStorage("price-eua") |>
    filter(year(date) > 2015) |>
    mutate(price = value) |>
    adjust.prices.inflation() |>
    dplyr::select(date, price, price.adjusted) |>
    mutate(Type = "(f) CO2-Emissionspreis\n(€/tCO2)") |>
    filter(year(date) > 2016)

d.coal.price = loadFromStorage("price-coal") |>
    filter(year(date) > 2006) |>
    adjust.prices.inflation() |>
    dplyr::select(date, price, price.adjusted) |>
    mutate(Type = "(e) Kohlepreis\n(€/tC)")

d.all.prices = bind_rows(d.gas.price,
          d.power.price,
          d.oil.price,
          d.gasoline.price,
          d.eua.price,
          d.coal.price)

### Plot price data ###
d.all.prices |>
    group_by(year = year(date), Type) |>
    summarize(price = mean(price.adjusted)) |>
    filter(year > 1999) |>
    filter(year < 2023) |>
    mutate(year = as.integer(year)) |>
    ggplot(aes(x = year, y = price)) +
    geom_line(size = 1) +
    xlab("Jahr") +
    ylab("Preis") +
    facet_wrap(. ~ Type, scale = "free") +
    scale_x_continuous(label = ~ scales::comma(.x, accuracy = 1, big.mark = "")) +
    theme_bw() +
    ylim(c(0, NA))

d.all.prices |>
    filter(date > as.Date("2021-01-01") & date  < as.Date("2022-12-31")) |>
    ggplot(aes(x = date, y = price.adjusted)) +
    geom_line(size = 0.5) +
    geom_vline(xintercept = as.Date("2022-02-24"), linetype = 2, col = COLORS[1], size =  0.5) +
    xlab("Datum") +
    ylab("Preis") +
    facet_wrap(. ~ Type, scale = "free") +
    theme_bw()


### Load consumption data + simple temperature correction ###
d.gas.consumption = loadFromStorage("consumption-gas-aggm") |>
    filter(year(date) > 2015) |>
    mutate(Type = "(a) Gas")

d.temperature.hdd = loadFromStorage("temperature-hdd")

d.temperature.hdd.sum = d.temperature.hdd |>
    group_by(year = year(date)) |>
    summarize(hdd.sum = sum(hdd))

hdd.long.term = d.temperature.hdd.sum |>
    filter(year > 2009) |>
    summarize(hdd.sum = mean(hdd.sum)) |>
    unlist()

d.gas.consumption.corrected = loadFromStorage("consumption-gas-aggm") |>
    filter(year(date) > 2015) |>
    group_by(year = year(date)) |>
    summarize(value = sum(value)) |>
    mutate(Type = "(b) Gas - Temperaturkorrigiert") |>
    merge(d.temperature.hdd.sum, by = "year") |>
    mutate(value = value/hdd.sum*hdd.long.term)

d.electricity.consumption = loadFromStorage("electricity-load") |>
    filter(year(date) > 2015) |>
    filter(country == "AT") |>
    mutate(Type = "(c) Strom")


d.cons.mean = bind_rows(d.gas.consumption,
          d.electricity.consumption) |>
    mutate(year = year(date)) |>
    filter(year > 2016 & year < 2022) |>
    group_by(year, Type) |>
    summarize(value = sum(value)) |>
    ungroup() |>
    group_by(Type) |>
    summarize(mean = mean(value))

### Plot consumption data ###
bind_rows(d.gas.consumption,
          d.electricity.consumption) |>
    group_by(year = year(date), Type) |>
    summarize(value = sum(value)) |>
    ungroup() |>
    filter(year > 2016) |>
    filter(year < 2023) |>
    #bind_rows(d.gas.consumption.corrected,
    #          d.temperature.hdd.sum |>
    #              filter(year > 2015) |>
    #              mutate(value = hdd.sum) |>
    #              mutate(Type = "(d) Heizgradtage")
    #              ) |>
    merge(d.cons.mean, by = "Type") |>
    mutate(value = (value - mean) / mean) |>
    ggplot(aes(x = year, y = 100 * value)) +
    geom_line(size = 1) +
    xlab("Jahr") +
    ylab("Differenz zum Verbrauch 2017 - 2021 (%)") +
    facet_wrap(. ~ Type) +
    theme_bw()
    #geom_hline(yintercept = 100, linetype = 2)
    ylim(c(91, 109))

### Load and plot generation data ###

colors = tibble(source = c("Wind Onshore",
                                 "Solar",
                                 "Hydro",
                                 "Fossil Gas",
                                 "Coal",
                                 "Other"),
                colors = c("#BBD7EE",
                           "#F3D039",
                           "#5BC5F2",
                           "#C3423F",
                           "#8B4513",
                           "#C0C0C0"
                ))


d.generation = loadFromStorage("electricity-generation") |>
    filter(country == "AT") |>
    mutate(source = ifelse(source %in% c("Fossil Oil",
                                         "Fossil Hard coal",
                                         "Other",
                                         "Other renewable",
                                         "Waste",
                                         "Geothermal",
                                         "Biomass"), "Other", source)) |>
    mutate(source = ifelse(source %in% c("Hydro Run-of-river and poundage",
                                         "Hydro Water Reservoir",
                                         "Hydro Pumped Storage"), "Hydro", source)) |>
    group_by(year = year(date), source) |>
    summarize(generation = sum(value)) |>
    ungroup() |>
    filter(year > 2015) |>
    filter(year < 2023) |>
    group_by(source) |>
    mutate(generation.mean = mean(generation)) |>
    mutate(generation.dev = (generation - generation.mean)) |>
    mutate(generation.dev.relative = (generation - generation.mean) / generation.mean) |>
    ungroup()

d.generation |>
    filter(source %in% c("Fossil Gas", "Hydro", "Wind Onshore")) |>
    ggplot(aes(x = year, y = generation.dev)) +
    geom_bar(stat="identity", aes(fill = source)) +
    scale_fill_manual(values = colors$colors[c(2, 3, 1, 4, 5)]) +
    theme_bw() +
    xlab("Jahr") +
    ylab("Abweichung von 7-jährigem Mittel (TWh)") +
    theme(legend.title = element_blank())

d.generation |>
    filter(source %in% c("Fossil Gas", "Hydro", "Wind Onshore")) |>
    ggplot(aes(x = year, y = 100 * generation.dev.relative)) +
    geom_bar(stat="identity", aes(fill = source), position = "dodge") +
    scale_fill_manual(values = colors$colors[c(2, 3, 1, 4, 5)]) +
    theme_bw() +
    xlab("Jahr") +
    ylab("Abweichung von 7-jährigem Mittel (%)") +
    theme(legend.title = element_blank())

### Emission data + plot ###

thg.emissions.at = tibble(emissions = c(80085, 83989, 85747, 91210, 90856,
                                        92029, 89607, 86841, 86259, 79585,
                                        84150, 82007, 79310, 79772, 76235,
                                        78487, 79468, 81792, 78558, 79741,
                                        73592, 77100, 74470),
                          years = 2000:2022,
                          Quelle = c(rep("Treibhasgasinventur (UBA)", 21),
                          "Nowcast (UBA)", "Schätzung (WIFO)"))

thg.emissions.at |>
    ggplot(aes(x = years, y = emissions / 1000)) +
    geom_line(size = 1) +
    geom_point(aes(col = Quelle), size = 3) +
    ylim(c(0, NA)) +
    theme_bw() +
    xlab("Jahre") +
    ylab("Treibhausgasemissionen \n(MtCO2-Äquivalent)") +
    scale_color_manual(values = c("green", "red", "black"))


### Savings estimate ###
source("calc/prediction-gas-consumption/_shared.r")
c.trans = c(
    month.name = "2022",
    value.21   = "to 2021",
    avg        = glue("to period {min(c.years.avg)}-{max(c.years.avg)}"),
    pred.base  = "in model 1",
    pred.power = "in model 2"
)

setnames(d.comp.f, names(c.trans), c.trans)
library(knitr)
kable(d.comp.f, align = "r", escape = FALSE)

### Storage prediction ###
source("calc/setup-storage-prediction.r")
library(imputeTS)

d.t.storage = d.base[date >= l.options$period$start & !is.na(storage.dom), .(
    date, value = storage.dom + l.gas.info$l.storage$strategic
)]

c.store.names = c(
    store.with.russia = "Mit russischem Gas",
    store.without.russia = "Ohne russisches Gas"
)

d.t.pred = d.all.years[variable %in% c("store.with.russia", "store.without.russia")][type == "observed"][, .(
    date, season, value,
    variable = factor(variable, names(c.store.names), c.store.names),
    year.before = substring(season, 1, 4) < 1990
)]

# Add start point
d.t.pred = rbind(
    d.t.pred,
    d.t.pred[date == min(date), .(
        date = tail(d.t.storage, 1)$date, season, value = tail(d.t.storage, 1)$value,
        variable, year.before
    )])

annotation <- tibble(
    label = c(
        "Strategische Gasreserve"
    ),
    x = c(as.Date("2023-01-01")),
    y = c(19)
)

ggplot(d.t.pred, aes(x = date, y = value)) +
    geom_line(aes(color = year.before, group = season), linewidth = 0.5, alpha = 0.3) +
    geom_smooth(aes(color = year.before), linewidth = 1.5, alpha = 1, fill = NA) +
    geom_line(data = d.t.storage, linewidth = 0.8, color = "black") +
    facet_wrap(variable ~ .) +
    scale_color_manual(
        values = COLORS, name = "Temperaturen der Periode:",
        labels = function(t) c("seit 1990", "bis 1989")[t + 1]
    ) +
    xlab(NULL) + ylab(NULL) +
    geom_abline(slope = 0, intercept = l.gas.info$l.storage$strategic, linetype = 2) +
    geom_text(data = annotation, aes(x = x, y = y, label = label), size = 4) +
    labs(
        title = "Entwicklung Gasspeicher",
        subtitle = "Heizsaison 22/23, Speicherniveau in TWh"
    )+
    theme_bw()

d.eval = reforecast.consumption.model(2022, d.temp, d.base, as.Date("2022-11-15"), max.date)

variables.joined = l.gas.info$l.storage$d.domestic %>%
    mutate(variable = "storage.with.russia") %>%
    mutate(model = "observation") %>%
    mutate(level = level + l.gas.info$l.storage$strategic) %>%
    dplyr::select(date, variable, model, value = level) %>%
    mutate(cumulative = cumsum(value)) %>%
    bind_rows(d.eval) %>%
    mutate(value.final = ifelse(variable %in% c("gas.cons"), cumulative, value)) %>%
    dplyr::select(date, variable, model, value.final)

variables.joined.fill = variables.joined %>%
    filter(!(variable == "storage.with.russia" & model == "observation")) %>%
    bind_rows(
        variables.joined %>%
            filter(variable == "storage.with.russia" & model == "observation") %>%
            merge(variables.joined %>% dplyr::select("date") %>% unique(), by = "date", all = T) %>%
            fill(variable, model) %>%
            mutate(value.final = na_interpolation(value.final))
    )


variables.joined.upt = variables.joined.fill %>%
    filter(variable != "storage.without.russia") %>%
    group_by(variable, model) %>%
    arrange(date) %>%
    mutate(diff = value.final - lag(value.final)) %>%
    na.omit() %>%
    dplyr::select(date, variable, model, diff) %>%
    spread(variable, diff) %>%
    mutate(sum.supply = gas.cons + storage.with.russia) %>%
    mutate(variable = "sum.supply") %>%
    group_by(model) %>%
    mutate(cumulative = cumsum(sum.supply)) %>%
    dplyr::select(date, variable, model, value.final = cumulative) %>%
    bind_rows(variables.joined) %>%
    ungroup()


variables.joined.upt %>%
    filter(model %in% c("observation", "model.from.start")) |>
    filter(variable == "sum.supply") |> filter(variable != "storage.without.russia") %>%
    mutate(Quelle = ifelse(model == "observation", "Beobachtung", "Annahmen energie.gv.at")) |>
    ggplot(aes(x = date, y = value.final)) +
    geom_line(aes(col = Quelle), size = 1) +
    #    facet_wrap(. ~ variable, scale = "free") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    xlab("Monat") +
    ylab("Gasverfügbarkeit kumulativ (TWh)") +
    scale_color_manual(values = COLORS) +
    theme_bw()


### oil price vs. primary energy
### https://inflationdata.com/articles/inflation-adjusted-prices/historical-crude-oil-prices-table/
d.oil.price.long.term = tibble(year = 1946:2022,
                              `Ölpreis ($/Barrel)` =
                                    c(24.49,28.79,34.34,34.67,34.31,31.8,31.09,32.46,33.19,
                                      32.56,32.26,33.29,30.98,30.67,29.35,28.41,28.08,28.34,
                                      28.82,28.45,28.47,27.88,27.2,26.98,26.03,26.5,25.66,
                                      31.62,56.42,67.58,68.62,70.77,68.34,102.1,135.5,117.34,
                                      98.37,87.04,82.5,74.58,39.25,46.55,37.52,44.05,52.74,
                                      44.2,40.88,34.57,31.48,32.76,38.85,34.61,21.78,29.55,
                                      47.38,38.73,37.76,44.87,59.37,76.29,86.16,92.11,126.21,
                                      74.15,97.31,115.33,112.25,116.62,107.71,52.62,45.07,
                                      53.45,68.55,58.29,37.1,66.78,91.92)
)

d.primary.energy.at = tibble(year = 1965:2020,
                             `Primärenergieverbrauch (TWh)` = 1000 * c(0.67,0.7,0.71,0.76,0.79,0.9,0.9,0.93,1,1,1,
                             1.03,1.03,1.08,1.14,1.14,1.1,1.06,1.06,1.05,1.11,1.12,1.18,
                             1.16,1.17,1.18,1.24,1.22,1.24,1.24,1.29,1.3,1.33,1.35,1.38,
                             1.38,1.43,1.43,1.45,1.48,1.5,1.49,1.45,1.48,1.43,1.48,1.38,
                             1.45,1.44,1.38,1.38,1.43,1.46,1.43,1.5,1.38) / 3.6)
d.primary.energy.at |>
    merge(d.oil.price.long.term, by = "year") |>
    gather(variable, value, -year) |>
    filter(year < 1990) |>
    filter(year > 1969) |>
    ggplot(aes(x = year, y = value)) +
    geom_line(aes(col = variable), size = 1) +
    scale_color_manual(values = COLORS, name = "") +
    theme_bw() +
    geom_vline(xintercept = 1979, linetype = 2) +
    xlab("Jahr") +
    ylab("") +
    labs(caption = "Quelle:\nBP Statistical Review of World Energy\ninflationdata.com")





