AttData_GS <- estimation(
  country = "Denmark",
  wdir = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark",
  StartWeek = '2015-W27',
  EndWeek = '2020-W18',
  groups = c('00to14', '15to39', '40to64', '65to74', '75to84', '85P', 'Total'),
  pooled <- c('00to14', '15to39', '40to64', '65to74', '75to84', '85P'),
  indicators = c('GSInfl', 'GSCOVID19'),
  restrict = TRUE,
  population = TRUE,
  lags = 2,
  ptrend = 0.05,
  p26 = 0.05,
  p52 = 0.10,
  Rdata = TRUE
)
# Aggregate ovre 2019-W40 to EndWeek
Agg_GS <- AttData_GS[(ISOweek >= '2019-W40'),
                     .(EET = round(sum(EET), 2),
                       EET.low = round(sum(EET) - 2*sqrt(sum(VEET)), 2),
                       EET.high = round(sum(EET) + 2*sqrt(sum(VEET)), 2),
                       EGSInfl = round(sum(EGSInfl), 2),
                       EGSInfl.low = round(sum(EGSInfl) - 2*sqrt(sum(VEGSInfl)), 2),
                       EGSInfl.high = round(sum(EGSInfl) + 2*sqrt(sum(VEGSInfl)), 2),
                       EGSCOVID19 = round(sum(EGSCOVID19), 2),
                       EGSCOVID19.low = round(sum(EGSCOVID19) - 2*sqrt(sum(VEGSCOVID19)), 2),
                       EGSCOVID19.high = round(sum(EGSCOVID19) + 2*sqrt(sum(VEGSCOVID19)), 2)),
                     keyby = group]
