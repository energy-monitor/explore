# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("calc/prediction-gas-consumption/_shared.r")
# source("export/analysis/_theme.r")

Sys.setlocale("LC_ALL", "de_AT.UTF-8")

library(wifo.theme)

showColors(getWifoColors(10))

d.loess = get.d.loess(d.base)
d.lines = get.d.lines(d.loess)

ggplot(d.loess, aes(x = temp, y = gas.consumption)) +
    geom_point(size = 0.8, stroke = 0, alpha = 0.5, fill = getWifoColors(1), color = getWifoColors(1)) +
    geom_line(data = d.lines, linewidth = 0.5, alpha = 1, color = "#C3423F") +
    ylab(NULL) + xlab("Temperatur in °C")  +
    # theme(legend.position="top")
    # labs(title = "Gaskonsum vs Temperatur", subtitle = "in GWh, LOESS Linie") +
    theme_wifo_base(font.size = 8)

ggsave("loess.png", dpi = 300, height = 8, width = 16, units = "cm")



d.plot = rbind(
    l.model.base$d.plot[variable %in% c("value")][date >= "2022-03-01"],
    l.model.base$d.plot[variable %in% c("value")][date >= "2021-03-01" & date <= "2021-12-31"][, .(
        date = as.Date(paste("2022", month(date), day(date), sep = "-")),
        variable = "value21", value, rollmean
    )],
    l.model.base$d.plot[variable %in% c("prediction")][date >= "2022-03-01"][, variable := 'mod1'],
    l.model.power$d.plot[variable %in% c("prediction")][date >= "2022-03-01"][, variable := 'mod2']
)

showColors(getWifoColors(10))

colors = c("#C3423F", "#F3D039", "#559BD5", "#72BB6F")

summary(l.model.base$m)
summary(l.model.power$m)

ggplot(d.plot, aes(x = date, y = rollmean, color = variable, group = variable)) +
    geom_line(linewidth = 0.5) +
    scale_color_manual(values = colors, name = NULL, labels = c(
        value = "Beobachtung",
        value21 = "Vorjahr",
        mod1 = "Schätzung - Modellvariante 1",
        mod2 = "Schätzung - Modellvariante 2")
    ) +
    # scale_y_continuous(labels = scales::comma())
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    xlab(NULL) + ylab(NULL) +
    theme_wifo_base(font.size = 8)

ggsave("comp.png", dpi = 300, height = 8, width = 16, units = "cm")




dcast(l.model.base$d.plot[date >= "2022-03-01"], date ~ variable, value.var = "rollmean") %>%
    ggplot(aes(x = date, y = difference * 100)) +
    geom_line(linewidth = 0.5, color = getWifoColors(1)) +
    geom_abline(slope = 0, intercept = 0, linewidth = 0.2, linetype = 2) +
    # geom_ribbon(aes(ymin = diff.pred.lower, ymax = diff.pred.upper), alpha=0.3) +
    geom_ribbon(aes(ymin = diff.conf.lower * 100, ymax = diff.conf.upper * 100), alpha = 0.1) +
    xlab(NULL) + ylab(NULL) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    scale_y_continuous(n.breaks = 10) +
    theme_wifo_base(font.size = 8)

ggsave("model1.png", dpi = 300, height = 6, width = 16, units = "cm")




dcast(l.model.power$d.plot[date >= "2022-03-01"], date ~ variable, value.var = "rollmean") %>%
    ggplot(aes(x = date, y = difference * 100)) +
    geom_line(linewidth = 0.5, color = getWifoColors(1)) +
    geom_abline(slope = 0, intercept = 0, linewidth = 0.2, linetype = 2) +
    # geom_ribbon(aes(ymin = diff.pred.lower, ymax = diff.pred.upper), alpha=0.3) +
    geom_ribbon(aes(ymin = diff.conf.lower * 100, ymax = diff.conf.upper * 100), alpha = 0.1) +
    xlab(NULL) + ylab(NULL) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    scale_y_continuous(n.breaks = 10) +
    theme_wifo_base(font.size = 8)

ggsave("model2.png", dpi = 300, height = 6, width = 16, units = "cm")



d.both = rbind(
    dcast(l.model.base$d.plot[date >= "2022-03-01"], date ~ variable, value.var = "rollmean")[, m := "Modell 1"],
    dcast(l.model.power$d.plot[date >= "2022-03-01"], date ~ variable, value.var = "rollmean")[, m := "Modell 2"]
)



ggplot(d.both, aes(x = date, y = difference * 100)) +
    facet_wrap(m ~ .) +
    geom_line(linewidth = 0.5, color = getWifoColors(1)) +
    geom_abline(slope = 0, intercept = 0, linewidth = 0.2, linetype = 2) +
    # geom_ribbon(aes(ymin = diff.pred.lower, ymax = diff.pred.upper), alpha=0.3) +
    geom_ribbon(aes(ymin = diff.conf.lower * 100, ymax = diff.conf.upper * 100), alpha = 0.1) +
    xlab(NULL) + ylab(NULL) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    scale_y_continuous(n.breaks = 10) +
    theme_wifo_base(font.size = 8)

ggsave("both.png", dpi = 300, height = 6, width = 16, units = "cm")




