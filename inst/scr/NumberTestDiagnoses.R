source("inst/scr/EpiMiBaDataCovid19.R", encoding = "UTF-8")

StartWeek <- '2014-W27'
EndWeek <- '2020-W20'

MiBaData <- setDT(GetMiBaData(ISOweek::ISOweek2date(paste0(StartWeek, "-1")), ISOweek::ISOweek2date(paste0(EndWeek, "-7"))))
MiBaData[, ISOweek := ISOweek::ISOweek(as.Date(prdate))]
MiBaData[, season := as.numeric(substr(ISOweek,1,4)) - (as.numeric(substr(ISOweek,7,8)) < 27)]

Infl <- MiBaData[(A == 1) | (B == 1), .(Infl = 1), by = .(season, cprnr)]
Infl <- Infl[, .(Infl = sum(Infl)), keyby = season]
Infl
Infl[season < 2019, .(Infl = median(Infl), Infl.min = min(Infl), Infl.max = max(Infl))]
Infl[('2019-W40' <= ISOweek) & (ISOweek <= '2020-W20'), .(Infl = median(Infl), Infl.min = min(Infl), Infl.max = max(Infl))]


COVID19 <- MiBaData[COVID19 == 1, .(COVID19 = 1), by = .(season, cprnr)]
COVID19[, .(COVID19 = sum(COVID19)), keyby = season]


con <- RODBC::odbcConnect("EpiLPR3_PROD", readOnlyOptimize = TRUE)
SILSData <- data.table(RODBC::sqlQuery(con, paste0("
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
    "), stringsAsFactors = FALSE, as.is = TRUE))
RODBC::odbcClose(con)
rm(con)

SILSData <- data.table(SILSData)

SILSData[, ISOweek := ISOweek::ISOweek(as.Date(date))]
SILSData[, season := as.numeric(substr(ISOweek,1,4)) - (as.numeric(substr(ISOweek,7,8)) < 27)]

SIPLS <- SILSData[SIPLS == 1, .(SIPLS = 1), by = .(season, cprnr)]
SIPLS <- SIPLS[, .(SIPLS = sum(SIPLS)), keyby = season]
SIPLS
SIPLS[season < 2019, .(SIPLS = median(SIPLS), SIPLS.min = min(SIPLS), SIPLS.max = max(SIPLS))]
SIPLS[('2019-W40' <= ISOweek) & (ISOweek <= '2020-W20'), .(SIPLS = median(SIPLS), Infl.min = min(SIPLS), Infl.max = max(SIPLS))]


SCLS <- SILSData[SCLS == 1, .(SCLS = 1), by = .(season, cprnr)]
SCLS[, .(SCLS = sum(SCLS)), keyby = season]
