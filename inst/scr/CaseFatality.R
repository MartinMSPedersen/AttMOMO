#' COVID-19 case-fatality as of DoD
#' by ISOweek
#'
#' @param StartWeek ISOweek format
#' @param EndWeek ISOweek format
#' @param d days of pos test before DoD (default = 30)
#' @import data.table
#' @import ISOweek
#' @return data with number by ISOweek
GetCOVID19CF <- function(StartWeek, EndWeek, d = 30) {
  library(data.table)

  source("inst/scr/EpiMiBaDataCovid19.R", encoding = "UTF-8")
  CFData <- data.table(GetMiBaData(ISOweek::ISOweek2date(paste0(StartWeek, "-1")) - 2*d , ISOweek::ISOweek2date(paste0(EndWeek, "-7"))))
  CFData <- CFData[COVID19 == 1, .(COVID19 = max(COVID19)),
                   keyby = .(cprnr, date = as.Date(prdate))]

  con <- RODBC::odbcConnect("EpiLPR3_PROD", readOnlyOptimize = TRUE)
  DeathsData <- data.table(RODBC::sqlQuery(con, paste0("
      set transaction isolation level read uncommitted;
      set deadlock_priority 5;
  	  select cprnr, DoD from IB_DKMOMO.DKMOMO.DKMOMO with(nolock)
    	  where ('", ISOweek::ISOweek2date(paste0(StartWeek, '-1')),"' <= DoD)
      order by cprnr, DoD
    "), stringsAsFactors = FALSE, as.is = TRUE))
  RODBC::odbcClose(con)

  CFData <- merge(CFData, DeathsData, by = 'cprnr', all = FALSE)
  CFData <- CFData[, .(cf = min(as.numeric((as.Date(DoD) - date) <= d))), keyby = .(cprnr, DoD)]
  CFData <- CFData[, .(cf = sum(cf)), keyby = .(ISOweek = ISOweek::ISOweek(as.Date(DoD)))]

  CFData <- merge(data.table(ISOweek = ISOweek::ISOweek(seq(as.Date(ISOweek::ISOweek2date(paste0(StartWeek, "-4"))),
                                 as.Date(ISOweek::ISOweek2date(paste0(EndWeek, "-4"))), by = 'week'))), CFData, by = 'ISOweek', all.x = TRUE)
  CFData[is.na(CFData)] <- 0

  CFData <- rbind(CFData, CFData[, .(ISOweek = 'Total', cf = sum(cf)),])

  return(CFData)
}
