# - INIT -----------------------------------------------------------------------
rm(list = ls())
source("load/entsoe/_shared.r")
# loadPackages()


# - DOIT -----------------------------------------------------------------------
update.time = now()
fromDate = today() %m-% months(1)


d.base = loadEntsoeComb(
    type = "physicalFlows",
    month.start = month.start, month.end = month.end #
    # month.start = "2024-10", month.end = "2024-10", check.updates = FALSE
)

# unique(d.base[, .(ResolutionCode, AreaCode, AreaTypeCode, AreaName, MapCode)])
# d.t = unique(d.base[AreaTypeCode == "CTY", .(ResolutionCode, AreaCode, AreaName, MapCode)])

# d.base.f = copy(d.base)
d.base.f = d.base[InAreaTypeCode == "CTY" & OutAreaTypeCode == "CTY" & DateTime >= fromDate]
d.base.f[, factor := resToFactor[ResolutionCode]]

# d.base.f 

# unique(d.base.f[, .(InMapCode , OutMapCode)][InMapCode == "AT"])

# d.comb = rbind(
#     d.base.f[InMapCode == "AT", .(value = -sum(FlowValue*factor)), by=.(iso2 = OutMapCode, code = OutAreaTypeCode)],
#     d.base.f[OutMapCode == "AT", .(value = sum(FlowValue*factor)), by=.(iso2 = InMapCode, code = InAreaTypeCode)]
# )

d.comb = rbind(
    d.base.f[InMapCode == "AT", .(value = -sum(FlowValue*factor)), by=.(iso2 = OutMapCode, dateTimeHourly = DateTimeHourly)],
    d.base.f[OutMapCode == "AT", .(value = sum(FlowValue*factor)), by=.(iso2 = InMapCode, dateTimeHourly = DateTimeHourly)]
)

d.agg = d.comb[, .(exports=sum(value)/10^3), by=.(
    date = as.Date(dateTimeHourly),
    hour = hour(dateTimeHourly)
)]



# # - STORE --------------------------------------------------------------------
saveToStorages(d.agg, list(
    id = "physical-flows-entsoe-recent",
    source = "entsoe",
    format = "csv",
    update.time = update.time
))
