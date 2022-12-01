d.loess = d.base[, .(
    date, temp,
    value = value * 1000
)]

m.loess = loess(value ~ temp, d.loess)

order.loess = order(d.loess$temp)

d.lines = data.table(
    temp = d.loess$temp[order.loess],
    value = m.loess$fitted[order.loess]
)

