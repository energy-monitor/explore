# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/entsoe/_shared.r")
# loadPackages()


# - DOIT -----------------------------------------------------------------------
update.time = now()
d.base = load_entsoe_data(
    c.nice2entsoe["physicalFlows"], from = date.start
)

# unique(d.base[, .(ResolutionCode, AreaCode, AreaTypeCode, AreaName, MapCode)])
# d.t = unique(d.base[AreaTypeCode == "CTY", .(ResolutionCode, AreaCode, AreaName, MapCode)])

# d.base.f = copy(d.base)
d.base.f = d.base[InAreaTypeCode == "CTY" & OutAreaTypeCode == "CTY"]
d.base.f[, factor := c.resToFactor[ResolutionCode]]

# d.base.f 

# unique(d.base.f[, .(InMapCode , OutMapCode)][InMapCode == "AT"])

# d.comb = rbind(
#     d.base.f[InMapCode == "AT", .(value = -sum(FlowValue*factor)), by=.(iso2 = OutMapCode, code = OutAreaTypeCode)],
#     d.base.f[OutMapCode == "AT", .(value = sum(FlowValue*factor)), by=.(iso2 = InMapCode, code = InAreaTypeCode)]
# )

d.comb = rbind(
    d.base.f[InMapCode == "AT", .(value = -sum(FlowValue*factor)), by=.(iso2 = OutMapCode, date = as.Date(DateTime))],
    d.base.f[OutMapCode == "AT", .(value = sum(FlowValue*factor)), by=.(iso2 = InMapCode, date = as.Date(DateTime))]
)

d.agg = d.comb[, .(exports=sum(value)/10^3), by=.(iso2, date)]

# Delete last (most probably incomplete) obs
d.agg = removeLastDays(d.agg, 1)


# # - STORE --------------------------------------------------------------------
saveToStorages(d.agg, list(
    id = "physical-flows-entsoe",
    source = "entsoe",
    format = "csv",
    update.time = update.time
))
