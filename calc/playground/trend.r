# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("_shared.r")
load('m.rData')

l.model.base


d.base = l.model.base$d.pred[, .(t, t.squared, date, prediction)]
c.coeffs = l.model.base$m$coeff[c("(Intercept)", "t", "t.squared")]

c.coeffs

d.base[, t.total := t*c.coeffs["t"] + t.squared*c.coeffs["t.squared"]]


d.plot = melt(d.base, id.vars = c("date"), measure.vars = c("prediction", "t.total"))


ggplot(d.plot, aes(x = date, y = value, group = variable, color = variable)) +
    geom_line()


ggsave('trend_share.png')
