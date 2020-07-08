#' DK population data
#'
#' @param Startweek
#' @param Endweek
#' @import RODBC
#' @import data.table
#' @import ISOweek
#' @return data with SCLS and graphs on disk
#' @export
GetDKPopData <- function(StartWeek, EndWeek) {
  library(data.table)

  con <- RODBC::odbcConnect("EpiLPR3_PROD", readOnlyOptimize = TRUE)
  PopData <- data.table(RODBC::sqlQuery(con, paste0("
      set transaction isolation level read uncommitted;
      set deadlock_priority 5;
  	  select date, agegrp as age, sum(N) as N
  	    from
        IB_DKMOMO.DKMOMO.DKpopDateRegionSexAgegrp with(nolock)
    	  where ('", ISOweek::ISOweek2date(paste0(StartWeek, '-1')),"' <= date) and (date <= '", ISOweek::ISOweek2date(paste0(EndWeek, '-7')),"')
    	  group by date, agegrp
      order by date, agegrp
    "), stringsAsFactors = FALSE, as.is = TRUE))
  RODBC::odbcClose(con)
  rm(con)
  PopData[ (00 <= age) & (age <= 14), group := '00to14']
  PopData[ (15 <= age) & (age <= 44), group := '15to44']
  PopData[ (45 <= age) & (age <= 64), group := '45to64']
  PopData[ (65 <= age) & (age <= 74), group := '65to74']
  PopData[ (75 <= age) & (age <= 84), group := '75to84']
  PopData[ (85 <= age) & !is.na(age), group := '85P']
  X <- copy(PopData)
  X$group <- 'Total'
  PopData <- rbind(PopData, X)
  rm(X)
  PopData <- PopData[!is.na(group), .(N = sum(N)), keyby = .(group, date)]
  PopData <- PopData[!is.na(group), .(N = mean(N)), keyby = .(group, ISOweek::ISOweek(as.Date(date)))]

  return(PopData)
}
