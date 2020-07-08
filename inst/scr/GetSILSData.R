#' Servere Illness (Respiratory and COVID-19) Like Symptoms (SRLS and SCLS)
#' Severe sentinel indicators based on diagnoses
#'
#' @param StartWeek
#' @param EndWeek
#' @import RODBC
#' @import data.table
#' @import ISOweek
#' @return data with number of SCLS, SRLS and diagnosed by group and ISOweek
#' @export
GetSILSData <- function(StartWeek, EndWeek) {
  library(data.table)

  # StartWeek <- '2020-W01'
  # EndWeek <- '2020-W15'

  con <- RODBC::odbcConnect("EpiLPR3_PROD", readOnlyOptimize = TRUE)
  SILSData <- RODBC::sqlQuery(con, paste0("
      set transaction isolation level read uncommitted;
      set deadlock_priority 5;
  	  select distinct cprnr, datediff(year, dob, cast(diagnosis_in as date)) as age, cast(diagnosis_in as date) as date,
  	    case when (diagnosis_in >= '2020-02-24') and (diagnosis in('DB342A','DB972A','DZ038PA1' /*,'DB342','DB972'*/)) then 1 else 0 end as SCLS,
  	    case when substring(diagnosis,1,4) in('DJ09','DJ10','DJ11','DJ12','DJ13','DJ14','DJ15','DJ16','DJ17','DJ18') then 1 else 0 end as SIPLS,
  	    case when substring(diagnosis,1,2) = 'DJ' then 1 else 0 end as SRLS
  	    from
        IB_EpiLPR.EpiLPR3.data_diagnoses with(nolock)
    	  where ('", ISOweek::ISOweek2date(paste0(StartWeek, '-1')),"' <= diagnosis_in) and (diagnosis_in <= '", ISOweek::ISOweek2date(paste0(EndWeek, '-7')),"')
      order by cprnr, date
    "), stringsAsFactors = FALSE, as.is = TRUE)
  RODBC::odbcClose(con)
  rm(con)

  SILSData <- setDT(SILSData)[, ISOweek := ISOweek::ISOweek(as.Date(date))]
  SILSData <- SILSData[, .(SCLS = max(SCLS), SRLS = max(SRLS), SIPLS = max(SIPLS), age = min(age)), keyby = .(cprnr, ISOweek)]
  SILSData[ (00 <= age) & (age <= 14), group := '00to14']
  SILSData[ (15 <= age) & (age <= 44), group := '15to44']
  SILSData[ (45 <= age) & (age <= 64), group := '45to64']
  SILSData[ (65 <= age) & (age <= 74), group := '65to74']
  SILSData[ (75 <= age) & (age <= 84), group := '75to84']
  SILSData[ (85 <= age) & !is.na(age), group := '85P']
  X <- copy(SILSData)
  X$group <- 'Total'
  SILSData <- rbind(SILSData, X)
  rm(X)
  SILSData <- SILSData[!is.na(group),]

  SILSData <- SILSData[, .(SCLS = sum(SCLS), SIPLS = sum(SIPLS), SRLS = sum(SRLS), diagnosed = .N), keyby = .(group, ISOweek)]

  return(SILSData)
}
