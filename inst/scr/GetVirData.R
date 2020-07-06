#' Influenza and COVID19 testede from EpiMiBa
#' by group and ISOweek
#'
#' @param StartWeek ISOweek format
#' @param EndWeek ISOweek format
#' @import data.table
#' @import ISOweek
#' @return data with number of Influenza and SARS-CoV-2 positive, and testede by group and ISOweek
GetVirData <- function(StartWeek, EndWeek) {
  library(data.table)
  source("inst/scr/GetDoB.R", encoding = "UTF-8")
  source("inst/scr/EpiMiBaDataCovid19.R", encoding = "UTF-8")

  MiBaData <- setDT(GetMiBaData(ISOweek::ISOweek2date(paste0(StartWeek, "-1")), ISOweek::ISOweek2date(paste0(EndWeek, "-7"))))
  MiBaData[, `:=`(age = as.integer(floor((as.Date(prdate) - GetDoB(cprnr))/365.25)),
                  ISOweek = ISOweek::ISOweek(as.Date(prdate)))]
  MiBaData[ (00 <= age) & (age <= 14), group := '00to14']
  MiBaData[ (15 <= age) & (age <= 44), group := '15to44']
  MiBaData[ (45 <= age) & (age <= 64), group := '45to64']
  MiBaData[ (65 <= age) & (age <= 74), group := '65to74']
  MiBaData[ (75 <= age) & (age <= 84), group := '75to84']
  MiBaData[ (85 <= age) & !is.na(age), group := '85P']
  X <- copy(MiBaData)
  X$group <- 'Total'
  MiBaData <- rbind(MiBaData, X)
  rm(X)
  MiBaData <- MiBaData[!is.na(group),]

  Infl_data <- MiBaData[(!is.na(A) | !is.na(B)),
                        .(InflPos = max(as.numeric((!is.na(A) & (A == 1)) | (!is.na(B) & (B == 1)))),
                          InflTested = 1),
                        keyby = .(group, ISOweek, cprnr)]
  Infl_data <- Infl_data[, .(InflPos = ifelse((is.na(sum(InflPos)) | (sum(InflPos) == sum(InflTested))), 0, sum(InflPos)), InflTested = sum(InflTested)),
                         keyby = .(group, ISOweek)]
  Infl_data <- merge(data.table(expand.grid(c(list(
    ISOweek = ISOweek::ISOweek(seq(as.Date(ISOweek::ISOweek2date(paste0(StartWeek, "-4"))),
                                   as.Date(ISOweek::ISOweek2date(paste0(EndWeek, "-4"))), by = 'week')),
    group = unique(Infl_data$group))))),
    Infl_data, by = c("group", "ISOweek"), all.x = TRUE)
  Infl_data[is.na(Infl_data)] <- 0

  COVID19_data <- MiBaData[!is.na(COVID19),
                           .(COVID19Pos = max(as.numeric(!is.na(COVID19) & (COVID19) == 1)),
                             COVID19Tested = 1),
                           keyby = .(cprnr, group, ISOweek)]
  COVID19_data <- COVID19_data[, .(COVID19Pos = ifelse((is.na(sum(COVID19Pos)) | (sum(COVID19Pos) == sum(COVID19Tested))), 0, sum(COVID19Pos)),
                                   COVID19Tested = sum(COVID19Tested)),
                               keyby = .(group, ISOweek)]
  COVID19_data <- merge(data.table(expand.grid(c(list(
    ISOweek = ISOweek::ISOweek(seq(as.Date(ISOweek::ISOweek2date(paste0(StartWeek, "-4"))),
                                   as.Date(ISOweek::ISOweek2date(paste0(EndWeek, "-4"))), by = 'week')),
    group = unique(Infl_data$group))))),
    COVID19_data, by = c("group", "ISOweek"), all.x = TRUE)
  COVID19_data[is.na(COVID19_data)] <- 0

  VirData <- merge(Infl_data, COVID19_data, by = c('group', 'ISOweek'))

  return(VirData)
}
