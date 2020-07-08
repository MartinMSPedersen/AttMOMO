# Install required packages
# install.packages(c('data.table', 'ISOweek', 'glm2'))

### Clear environment i.e. delete all data and variables
rm(list = ls())
# Clear console
cat("\014")

source("R/AttMOMO.R")
AttData_GSILS <- AttMOMO(
  country = "Denmark",
  wdir = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark",
  StartWeek = '2014-W27',
  EndWeek = '2020-W22',
  groups = c('00to14', '15to44', '45to64', '65to74', '75to84', '85P', 'Total'),
  pooled <- c('00to14', '15to44', '45to64', '65to74', '75to84', '85P'),
  indicators = c('GSIPLS', 'GSCLS'),
  lags = 3,
  ptrend = 0.05,
  p26 = 0.05,
  p52 = 0.10,
  Rdata = TRUE
)

StartWeek <- '2019-W40'
EndWeek <- '2020-W20'
# 365.25/as.numeric(ISOweek::ISOweek2date(paste0(EndWeek, "-7")) - ISOweek::ISOweek2date(paste0(StartWeek, "-1")))

# Influenza aggregate 2019-W40 to 2020-W20 by group
Agg_GSIPLS <- AttData_GSILS[(StartWeek <= ISOweek) & (ISOweek <= EndWeek),
                            .(PersonYears = round(mean(N), 2),
                              EGSIPLS = round(sum(EGSIPLS), 2),
                              EGSIPLS.low = round(sum(EGSIPLS) - qnorm(0.975)*sqrt(sum(VEGSIPLS)), 2),
                              EGSIPLS.high = round(sum(EGSIPLS) + qnorm(0.975)*sqrt(sum(VEGSIPLS)), 2),
                              EGSIPLS_mr = round(100000*365.25/as.numeric(ISOweek::ISOweek2date(paste0(EndWeek, "-7")) - ISOweek::ISOweek2date(paste0(StartWeek, "-1")))*sum(EGSIPLS)/mean(N), 2),
                              EGSIPLS_mr.low = round(100000*365.25/as.numeric(ISOweek::ISOweek2date(paste0(EndWeek, "-7")) - ISOweek::ISOweek2date(paste0(StartWeek, "-1")))*(sum(EGSIPLS) - qnorm(0.975)*sqrt(sum(VEGSIPLS)))/mean(N), 2),
                              EGSIPLS_mr.high = round(100000*365.25/as.numeric(ISOweek::ISOweek2date(paste0(EndWeek, "-7")) - ISOweek::ISOweek2date(paste0(StartWeek, "-1")))*(sum(EGSIPLS) + qnorm(0.975)*sqrt(sum(VEGSIPLS)))/mean(N), 2)),
                            keyby = group]
Agg_GSIPLS <- rbind(Agg_GSIPLS,
                    AttData_GSILS[(StartWeek <= ISOweek) & (ISOweek <= EndWeek),
                                  .(PersonYears = round(mean(N), 2),
                                    EGSIPLS = round(sum(EAGSIPLS), 2),
                                    EGSIPLS.low = round(sum(EAGSIPLS) - qnorm(0.975)*sqrt(sum(VEAGSIPLS)), 2),
                                    EGSIPLS.high = round(sum(EAGSIPLS) + qnorm(0.975)*sqrt(sum(VEAGSIPLS)), 2),
                                    EGSIPLS_mr = round(100000*365.25/as.numeric(ISOweek::ISOweek2date(paste0(EndWeek, "-7")) - ISOweek::ISOweek2date(paste0(StartWeek, "-1")))*sum(EAGSIPLS)/mean(N), 2),
                                    EGSIPLS_mr.low = round(100000*365.25/as.numeric(ISOweek::ISOweek2date(paste0(EndWeek, "-7")) - ISOweek::ISOweek2date(paste0(StartWeek, "-1")))*(sum(EAGSIPLS) - qnorm(0.975)*sqrt(sum(VEAGSIPLS)))/mean(N), 2),
                                    EGSIPLS_mr.high = round(100000*365.25/as.numeric(ISOweek::ISOweek2date(paste0(EndWeek, "-7")) - ISOweek::ISOweek2date(paste0(StartWeek, "-1")))*(sum(EAGSIPLS) + qnorm(0.975)*sqrt(sum(VEAGSIPLS)))/mean(N), 2)),
                                  keyby = group])

# COVID-19 aggregate 2019-W40 to 2020-W20 by group
Agg_GSCLS <- AttData_GSILS[(StartWeek <= ISOweek) & (ISOweek <= EndWeek),
                           .(PersonYears = round(mean(N), 2),
                             EGSCLS = round(sum(EGSCLS), 2),
                             EGSCLS.low = round(sum(EGSCLS) - qnorm(0.975)*sqrt(sum(VEGSCLS)), 2),
                             EGSCLS.high = round(sum(EGSCLS) + qnorm(0.975)*sqrt(sum(VEGSCLS)), 2),
                             EGSCLS_mr = round(100000*365.25/as.numeric(ISOweek::ISOweek2date(paste0(EndWeek, "-7")) - ISOweek::ISOweek2date(paste0(StartWeek, "-1")))*sum(EGSCLS)/mean(N), 2),
                             EGSCLS_mr.low = round(100000*365.25/as.numeric(ISOweek::ISOweek2date(paste0(EndWeek, "-7")) - ISOweek::ISOweek2date(paste0(StartWeek, "-1")))*(sum(EGSCLS) - qnorm(0.975)*sqrt(sum(VEGSCLS)))/mean(N), 2),
                             EGSCLS_mr.high = round(100000*365.25/as.numeric(ISOweek::ISOweek2date(paste0(EndWeek, "-7")) - ISOweek::ISOweek2date(paste0(StartWeek, "-1")))*(sum(EGSCLS) + qnorm(0.975)*sqrt(sum(VEGSCLS)))/mean(N), 2)),
                           keyby = group]
Agg_GSCLS <- rbind(Agg_GSCLS,
                   AttData_GSILS[(StartWeek <= ISOweek) & (ISOweek <= EndWeek),
                                 .(PersonYears = round(mean(N), 2),
                                   EGSCLS = round(sum(EAGSCLS), 2),
                                   EGSCLS.low = round(sum(EAGSCLS) - qnorm(0.975)*sqrt(sum(VEAGSCLS)), 2),
                                   EGSCLS.high = round(sum(EAGSCLS) + qnorm(0.975)*sqrt(sum(VEAGSCLS)), 2),
                                   EGSCLS_mr = round(100000*365.25/as.numeric(ISOweek::ISOweek2date(paste0(EndWeek, "-7")) - ISOweek::ISOweek2date(paste0(StartWeek, "-1")))*sum(EAGSCLS)/mean(N), 2),
                                   EGSCLS_mr.low = round(100000*365.25/as.numeric(ISOweek::ISOweek2date(paste0(EndWeek, "-7")) - ISOweek::ISOweek2date(paste0(StartWeek, "-1")))*(sum(EAGSCLS) - qnorm(0.975)*sqrt(sum(VEAGSCLS)))/mean(N), 2),
                                   EGSCLS_mr.high = round(100000*365.25/as.numeric(ISOweek::ISOweek2date(paste0(EndWeek, "-7")) - ISOweek::ISOweek2date(paste0(StartWeek, "-1")))*(sum(EAGSCLS) + qnorm(0.975)*sqrt(sum(VEAGSCLS)))/mean(N), 2)),
                                 keyby = group])

