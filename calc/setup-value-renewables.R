# - INIT -----------------------------------------------------------------------
source("_shared.r")
loadPackages('tidyverse')

COUNTRIES <- c("AT", "DE", "FR", "ES")
COUNTRIES_PRICE <- c("AT", "DE", "FR", "ES")

d.generation = loadFromStorage(id = "electricity-generation-hourly")

d.prices = loadFromStorage(id = "electricity-price-entsoe-hourly") %>%
    mutate(country = ifelse(country == "DE_LU", "DE", country)) %>%
    filter(price >= 0)

d.prices %>% filter(country =="FR")


d.load = loadFromStorage(id = "electricity-load-hourly-res") %>%
    filter(country %in% COUNTRIES)

d.gen.sel <- d.generation %>%
    filter(country %in% COUNTRIES) %>%
    group_by(year = year(DateTime), source, country) %>%
    mutate(hour_of_year = seq_len(n())) %>%
    ungroup()

d.gen.sel.ren <- d.gen.sel %>%
    filter(
        source %in% c(
            "Wind Onshore",
            "Solar",
            "Hydro Run-of-river and poundage",
            "Nuclear",
            "Wind Offshore"
        )
    ) %>%
    group_by(DateTime, country) %>%
    summarize(generation = sum(value, na.rm = TRUE))

d.gen.sel.ren.wo.nuclear <- d.gen.sel %>%
    filter(
        source %in% c(
            "Wind Onshore",
            "Solar",
            "Hydro Run-of-river and poundage",
            "Wind Offshore"
        )
    ) %>%
    group_by(DateTime, country) %>%
    summarize(generation = sum(value, na.rm = TRUE))

d.residual <- d.gen.sel.ren %>%
    dplyr::select(DateTime, country, generation)  %>%
    full_join(
        d.load %>% dplyr::select(DateTime, country, value),
        by = c("DateTime" = "DateTime", "country" = "country")
    ) %>%
    mutate(residual = value - generation)

d.residual.wo.nuclear <- d.gen.sel.ren.wo.nuclear %>%
    mutate(value = ifelse(is.na(generation), 0, generation)) %>%
    dplyr::select(DateTime, country, generation)  %>%
    full_join(
        d.load %>% dplyr::select(DateTime, country, value),
        by = c("DateTime" = "DateTime", "country" = "country")
    ) %>%
    mutate(residual = value - generation)

d.prices.filtered <- d.prices %>%
    mutate(year = year(DateTime)) %>%
    filter(year > 2018) %>%
    filter(country %in% COUNTRIES_PRICE) %>%
    arrange(DateTime) %>%
    dplyr::select(DateTime, year, price, country)

d.capacity = loadFromStorage(id = "capacity-at")

d.capacity.at <- d.capacity %>%
    transmute(
        year = year(`Time from [CET/CEST]`),
        capacity_pv_mw = as.numeric(`Solar [MW]`),
        capacity_wind_mw = as.numeric(`Wind [MW]`)
    )

d.generation.at <- d.generation %>%
    filter(country == "AT", source %in% c("Solar", "Wind Onshore")) %>%
    mutate(technology = factor(ifelse(source == "Solar", "pv", "wind"), levels = c("pv", "wind"))) %>%
    group_by(DateTime, technology) %>%
    summarize(generation_mw = sum(value, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(
        names_from = technology,
        values_from = generation_mw,
        names_prefix = "generation_",
        names_expand = TRUE,
        values_fill = 0
    )

d.production.1mw.at <- d.generation.at %>%
    mutate(year = year(DateTime)) %>%
    full_join(d.capacity.at, by = "year") %>%
    transmute(
        DateTime = DateTime,
        year = year,
        country = "AT",
        production_pv_1mw = ifelse(capacity_pv_mw > 0, generation_pv / capacity_pv_mw, NA_real_),
        production_wind_1mw = ifelse(capacity_wind_mw > 0, generation_wind / capacity_wind_mw, NA_real_)
    ) %>%
    filter(year > 2022)

d.market.value.1mw.at <- d.production.1mw.at %>%
    inner_join(
        d.prices.filtered %>%
            filter(country == "AT", year >= 2023) %>%
            dplyr::select(DateTime, price),
        by = "DateTime"
    ) %>%
    transmute(
        DateTime = DateTime,
        year = year,
        country = country,
        production_pv_1mw = production_pv_1mw,
        production_wind_1mw = production_wind_1mw,
        price = price,
        market_value_pv_1mw = production_pv_1mw * ifelse(price > 0, price, 0),
        market_value_wind_1mw = production_wind_1mw * ifelse(price > 0, price, 0)
    )

d.market.value.cumsum.1mw.at <- d.market.value.1mw.at %>%
    group_by(year) %>%
    arrange(DateTime, .by_group = TRUE) %>%
    transmute(
        DateTime = DateTime,
        year = year,
        day_of_year = yday(DateTime),
        cumsum_market_value_pv = cumsum(ifelse(is.na(market_value_pv_1mw), 0, market_value_pv_1mw)),
        cumsum_market_value_wind = cumsum(ifelse(is.na(market_value_wind_1mw), 0, market_value_wind_1mw))
    ) %>%
    ungroup() %>%
    tidyr::pivot_longer(
        cols = starts_with("cumsum_"),
        names_to = "technology",
        names_prefix = "cumsum_market_value_",
        values_to = "cumulative_market_value"
    ) %>%
    mutate(technology = toupper(technology))

# - PLOTTING FUNCTIONS ---------------------------------------------------------



# Common theme for all plots
theme_energy <- function(base_size = 12) {
    theme_minimal() +
    theme(
        plot.title = element_text(size = base_size * 1.17, face = "bold", margin = margin(b = base_size * 0.8)),
        axis.title = element_text(size = base_size * 0.92),
        axis.text = element_text(size = base_size * 0.83),
        strip.text = element_text(size = base_size * 0.83, face = "bold"),
        legend.text = element_text(size = base_size * 0.83),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major = element_line(color = "#f0f0f0", linewidth = 0.3),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA)
    )
}

# Plot residual load by country and year
plot_residual_load <- function(data, window = 28, years_back = 6, base_size = 14) {
    data %>%
        group_by(Jahr = year(DateTime), Tag = yday(DateTime), Land = country) %>%
        summarize(residual = mean(residual, na.rm = TRUE), DateTime = min(DateTime), .groups = "drop") %>%
        mutate(Land = factor(Land, levels = COUNTRIES)) %>%
        mutate(Jahr = as.character(Jahr)) %>%
        filter(Jahr > as.character(year(Sys.Date()) - years_back)) %>%
        mutate(is_current = ifelse(Jahr == as.character(year(Sys.Date())), "Aktuell", "Archiv")) %>%
        group_by(Land) %>%
        mutate(residual_smooth = zoo::rollapply(residual / 1000, window, mean, align = "right", fill = NA)) %>%
        ungroup() %>%
        ggplot(aes(x = Tag, y = residual_smooth, color = Jahr, linewidth = is_current)) +
        geom_line() +
        facet_wrap(~Land, scales = "free", nrow = 2, drop = FALSE) +
        scale_linewidth_manual(values = c("Aktuell" = 1.2, "Archiv" = 0.5), guide = "none") +
        scale_color_viridis_d(end = 0.9) +
        labs(
            title = "Residuallast nach Abzug erneuerbarer Energien",
            x = "Tag des Jahres",
            y = glue("Residuallast (GW)\nRollierender {window}-Tage Durchschnitt"),
            color = "Jahr"
        ) +
        theme_energy(base_size = base_size)
}

# Plot market prices by country and year
    plot_market_prices <- function(data, window = 28, years_back = 6, base_size = 14) {
    data %>%
        group_by(Jahr = year(DateTime), Tag = yday(DateTime), Land = country) %>%
        summarize(Preis = mean(price, na.rm = TRUE), DateTime = min(DateTime), .groups = "drop") %>%
        mutate(Land = factor(Land, levels = COUNTRIES_PRICE)) %>%
        mutate(Jahr = as.character(Jahr)) %>%
        filter(Jahr > as.character(year(Sys.Date()) - years_back)) %>%
        mutate(is_current = ifelse(Jahr == as.character(year(Sys.Date())), "Aktuell", "Archiv")) %>%
        group_by(Land) %>%
        mutate(Preis_smooth = zoo::rollapply(Preis, window, mean, align = "right", fill = NA)) %>%
        ungroup() %>%
        ggplot(aes(x = Tag, y = Preis_smooth, color = Jahr, linewidth = is_current)) +
        geom_line() +
        facet_wrap(~Land, scales = "fixed", nrow = 2, drop = FALSE) +
        scale_linewidth_manual(values = c("Aktuell" = 1.2, "Archiv" = 0.5), guide = "none") +
        scale_color_viridis_d(end = 0.9) +
        labs(
            title = "Großhandelspreise für Elektrizität",
            x = "Tag des Jahres",
            y = glue("Preis (€/MWh)\nRollierender {window}-Tage Durchschnitt"),
            color = "Jahr"
        ) +
        theme_energy(base_size = base_size)
}

# Plot market value (price-weighted generation) by technology
    plot_market_value <- function(gen_data, price_data, sources = c("Solar", "Wind Onshore", "Fossil Gas"), window = 40, years_back = 5, base_size = 14) {
    gen_data %>%
        inner_join(price_data, by = c("DateTime", "country")) %>%
        filter(source %in% sources, !is.na(source)) %>%
        mutate(value_weighted = value * price) %>%
        group_by(Jahr = year(DateTime), Tag = yday(DateTime), country, source) %>%
        summarize(value = sum(value_weighted, na.rm = TRUE) / sum(value, na.rm = TRUE), 
                  DateTime = min(DateTime), .groups = "drop") %>%
        filter(Jahr > year(Sys.Date()) - years_back) %>%
        mutate(Jahr = as.character(Jahr)) %>%
        group_by(source, country) %>%
        mutate(value_smooth = zoo::rollapply(value, window, mean, na.rm = TRUE, align = "right", fill = NA)) %>%
        ungroup() %>%
        ggplot(aes(x = Tag, y = value_smooth, color = source)) +
        geom_line(linewidth = 0.7) +
        facet_grid(Jahr ~ country, scales = "fixed") +
        scale_color_manual(
            values = c("Solar" = "#FDB462", "Wind Onshore" = "#80B1D3", "Fossil Gas" = "#FB8072"),
            name = "Technologie"
        ) +
        labs(
            title = "Marktwert der Stromerzeugung nach Technologie",
            x = "Tag des Jahres",
            y = glue("Marktwert (€/MWh)\nRollierender {window}-Tage Durchschnitt"),
            color = "Technologie"
        ) +
        theme_energy(base_size = base_size)
}

# Plot cumulative market value by year and technology (1 MW systems)
    plot_cumsum_market_value <- function(data, title = "Kumulativer Marktwert: 1 MW Systeme in Österreich", base_size = 14) {
    data %>%
        ggplot(aes(x = day_of_year, y = cumulative_market_value, color = technology, linewidth = technology)) +
        geom_line() +
        facet_wrap(~year, scales = "fixed") +
        scale_linewidth_manual(values = c("PV" = 0.9, "WIND" = 0.9), guide = "none") +
        scale_color_manual(
            values = c("PV" = "#FDB462", "WIND" = "#80B1D3"),
            labels = c("PV" = "Photovoltaik", "WIND" = "Windkraft")
        ) +
        labs(
            title = title,
            x = "Tag des Jahres",
            y = "Kumulativer Erlös (€/MW)",
            color = "Technologie"
        ) +
        theme_energy(base_size = base_size)
}

# Plot price spread (flexibility opportunity) by country and year
    plot_flexibility_spread <- function(data, window = 28, years_back = 6, base_size = 14) {
    data %>%
        mutate(Tag = yday(DateTime), Monat = month(DateTime)) %>%
        group_by(year, Monat, Tag, country) %>%
        summarize(spread = max(price, na.rm = TRUE) - min(price, na.rm = TRUE), 
                  DateTime = min(DateTime), .groups = "drop") %>%
        mutate(country = factor(country, levels = COUNTRIES_PRICE)) %>%
        group_by(country) %>%
        mutate(spread_smooth = zoo::rollapply(spread, window, mean, align = "right", fill = NA)) %>%
        ungroup() %>%
        mutate(Jahr = as.character(year)) %>%
        filter(Jahr > as.character(year(Sys.Date()) - years_back)) %>%
        mutate(is_current = ifelse(Jahr == as.character(year(Sys.Date())), "Aktuell", "Archiv")) %>%
        ggplot(aes(x = Tag, y = spread_smooth, color = Jahr, linewidth = is_current)) +
        geom_line() +
        facet_wrap(~country, scales = "free", nrow = 2, drop = FALSE) +
        scale_linewidth_manual(values = c("Aktuell" = 1.2, "Archiv" = 0.5), guide = "none") +
        scale_color_viridis_d(end = 0.9) +
        labs(
            title = "Preisvolatilität und Flexibilitätsmöglichkeiten",
            x = "Tag des Jahres",
            y = glue("Max. täglicher Preisunterschied (€/MWh)\nRollierender {window}-Tage Durchschnitt"),
            color = "Jahr"
        ) +
        theme_energy(base_size = base_size)
}

# Plot cumulative price spread (flexibility revenue potential)
    plot_flexibility_cumulative <- function(data, years_back = 6, base_size = 14) {
    data %>%
        mutate(Tag = yday(DateTime), Monat = month(DateTime)) %>%
        group_by(year, Monat, Tag, country) %>%
        summarize(spread = max(price, na.rm = TRUE) - min(price, na.rm = TRUE), 
                  DateTime = min(DateTime), .groups = "drop") %>%
        mutate(country = factor(country, levels = COUNTRIES_PRICE)) %>%
        group_by(year, country) %>%
        mutate(spread_cumsum = cumsum(spread)) %>%
        ungroup() %>%
        mutate(Jahr = as.character(year)) %>%
        filter(Jahr > as.character(year(Sys.Date()) - years_back)) %>%
        mutate(is_current = ifelse(Jahr == as.character(year(Sys.Date())), "Aktuell", "Archiv")) %>%
        ggplot(aes(x = Tag, y = spread_cumsum, color = Jahr, linewidth = is_current)) +
        geom_line() +
        facet_wrap(~country, scales = "free", nrow = 2, drop = FALSE) +
        scale_linewidth_manual(values = c("Aktuell" = 1.2, "Archiv" = 0.5), guide = "none") +
        scale_color_viridis_d(end = 0.9) +
        labs(
            title = "Kumulatives Erlöspotential für Speichersysteme (1 MWh)",
            x = "Tag des Jahres",
            y = "Kumulativer Preisunterschied (€/MWh)",
            color = "Jahr"
        ) +
        theme_energy(base_size = base_size)
}

CURRENT_YEAR = year(Sys.Date())
YEARS_BACK = 6

plot_residual_load(d.residual.wo.nuclear, window = 28, years_back = YEARS_BACK)

plot_market_prices(d.prices.filtered, window = 28, years_back = YEARS_BACK)


