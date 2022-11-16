# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("calc/prediction-gas-consumption/_shared.r")


# - CONF -----------------------------------------------------------------------
update.data = TRUE

cut.date = "2022-07-01"
cut.date.nice = "1. Juli '22"


# - LOAD -----------------------------------------------------------------------
d.base = loadBase(update.data)


# - NAIV LOESS -----------------------------------------------------------------
d.tmp = copy(d.base)[, `:=`(
    treated = date >= cut.date,
    value = value * 1000
)]

d.before = d.tmp[treated == FALSE]
d.after = d.tmp[treated == TRUE]

lo.before = loess(value ~ temp, d.before)
lo.after = loess(value ~ temp, d.after)

order.before = order(d.before$temp)
order.after = order(d.after$temp)

d.lines = rbind(
    data.table(temp = d.before$temp[order.before], value = lo.before$fitted[order.before], treated = FALSE),
    data.table(temp = d.after$temp[order.after], value = lo.after$fitted[order.after], treated = TRUE)
)

ggplot(d.tmp, aes(x = temp, y = value, color = treated, group = treated, alpha = treated)) +
    geom_point(size = 0.4) +
    geom_line(data = d.lines, size = 1, alpha = 1) +
    xlab("Temperatur in °C") + ylab("Gaskonsum in GWh") +
    scale_color_manual(values = c('#5B84B1FF', '#FC766AFF'), name = "", labels = c(glue("Vor dem {cut.date.nice}"), glue("Nach dem {cut.date.nice}"))) +
    scale_alpha_manual(values = c(0.3, 0.8), guide = NULL) +
    theme_light() + theme(legend.position="top")

ggsave('loess.png', height = 4, width = 6)
