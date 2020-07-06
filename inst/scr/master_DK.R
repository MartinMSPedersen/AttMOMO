# Install required packages
# install.packages(c('data.table', 'ISOweek'))


### Clear enviromnet i.e. delete all data and variabels
rm(list = ls())
# Clear console
cat("\014")

source("R/Estimation_add.R")
AttData_PosPct <- estimation(
  country = "Denmark",
  wdir = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark",
  StartWeek = '2015-W27',
  EndWeek = '2020-W18',
  groups = c('00to14', '15to39', '40to64', '65to74', '75to84', '85P', 'Total'),
  pooled <- c('00to14', '15to39', '40to64', '65to74', '75to84', '85P'),
  indicators = c('InflPosInc', 'COVID19PosInc'),
  restrict = TRUE,
  population = TRUE,
  lags = 2,
  ptrend = 0.05,
  p26 = 0.05,
  p52 = 0.10,
  Rdata = TRUE
)
Y_PosPct <- AttData_PosPct[(ISOweek >= '2019-W40'),
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


source("R/Estimation_add.R")
AttData_SILS <- estimation(
  country = "Denmark",
  wdir = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark",
  StartWeek = '2015-W27',
  EndWeek = '2020-W18',
  groups = c('00to14', '15to39', '40to64', '65to74', '75to84', '85P', 'Total'),
  pooled <- c('00to14', '15to39', '40to64', '65to74', '75to84', '85P'),
  indicators = c('SRLSInc', 'SCLSInc'),
  restrict = TRUE,
  population = TRUE,
  lags = 2,
  ptrend = 0.05,
  p26 = 0.05,
  p52 = 0.10,
  Rdata = TRUE
)
Y_SILS <- AttData_SILS[(ISOweek >= '2019-W40'),
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


source("R/Estimation_add.R")
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



X <- merge(AttData_PosPct[(ISOweek >= '2019-W40') & ( group == 'TotalPooled'), c('ISOweek', 'EInflPosInc', 'ECOVID19PosInc')],
           AttData_SILS[(ISOweek >= '2019-W40') & ( group == 'TotalPooled'), c('ISOweek', 'ESRLSInc', 'ESCLSInc')], by = 'ISOweek')
X <- merge(X, AttData_GS[(ISOweek >= '2019-W40') & ( group == 'TotalPooled'), c('ISOweek', 'EGSInfl', 'EGSCOVID19')], by = 'ISOweek')

X <- rbind(X,
           X[, .(ISOweek = 'Total', EInflPosInc = sum(EInflPosInc),
                 ECOVID19PosInc = sum(ECOVID19PosInc),
                 ESRLSInc = sum(ESRLSInc),
                 ESCLSInc = sum(ESCLSInc),
                 EGSInfl = sum(EGSInfl),
                 EGSCOVID19 = sum(EGSCOVID19))]
)

X[, `:=`(EInflPosInc = round(EInflPosInc, 2),
         ECOVID19PosInc = round(ECOVID19PosInc, 2),
         ESRLSInc = round(ESRLSInc, 2),
         ESCLSInc = round(ESCLSInc, 2),
         EGSInfl = round(EGSInfl, 2),
         EGSCOVID19 = round(EGSCOVID19, 2))]


Y <- AttData_GS[(ISOweek >= '2019-W40') & ( group == 'TotalPooled'), c('ISOweek', 'EET', 'EGSInfl', 'EGSCOVID19')]
Y <- rbind(Y,
           Y[, .(ISOweek = 'TotalPooled',
                 EET = sum(EET),
                 EGSInfl = sum(EGSInfl),
                 EGSCOVID19 = sum(EGSCOVID19))]
)

Y[, `:=`(EET = round(EET, 2),
         EGSInfl = round(EGSInfl, 2),
         EGSCOVID19 = round(EGSCOVID19, 2))]

Y <- AttData_GS[(ISOweek >= '2019-W40'),
                .(EET = round(sum(EET), 2),
                  EGSInfl = round(sum(EGSInfl), 2),
                  EGSCOVID19 = round(sum(EGSCOVID19), 2)), keyby = group]

