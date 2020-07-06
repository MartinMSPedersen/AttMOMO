StartWeek <- '2014-W27'
EndWeek <- '2020-W18'

# A-MOMO data -------------------------------------------------------------
source("R/GetMOMOdata.R")
GetMOMOdata(
  indir = paste0("H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/DK-MOMO/MOMOv4-3-Denmark-", gsub('W', '', EndWeek),"/EUROMOMO-COMPLETE-Denmark-", gsub('W', '', EndWeek)),
  filename = paste0("EUROMOMOv4-3-COMPLETE-Denmark-", gsub('W', '', EndWeek), ".txt"),
  outdir = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark/data"
)
rm(GetMOMOdata)

# Rename group
deaths <- read.table(file = paste0("H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark/data", "/Death_data.txt"), sep = ";",
                     header = TRUE, as.is = TRUE)
deaths <- data.table::setDT(deaths)[, group := dplyr::recode(group, '0to14' = '00to14')][(group %in% c('00to14', '15to39', '40to64', '65to74', '75to84', '85P', 'Total')),]
write.table(deaths, file = paste0("H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark/data", "/Death_data.txt"), sep = ";", col.names = TRUE, row.names = FALSE)
rm(deaths)

# Extreme temperature data ------------------------------------------------
source("R/GetWdata.R")
source("R/GetET.R")
wdata <- GetWdata(CountryCode = "DK")
ET <- GetET(ET = wdata, StartWeek, EndWeek, tvar = "temp")
write.table(ET, file = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark/data/ET_data.txt", row.names = FALSE, sep =";", dec=".")
rm(wdata, ET, GetWdata, GetET)

# Population data ---------------------------------------------------------
source("inst/scr/GetDKPopData.R")
PopData <- GetDKPopData(StartWeek, EndWeek)

write.table(PopData,
            file = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark/data/Population_data.txt", row.names = FALSE, sep =";", dec=".")
rm(GetDKPopData)

# Virological data --------------------------------------------------------
source("inst/scr/GetVirData.R")
VirData <- data.table(merge(GetVirData(StartWeek, EndWeek), PopData, by = c('group', 'ISOweek')))

write.table(VirData[, .(InflPosInc = 10000*InflPos/N), keyby = .(group, ISOweek)],
            file = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark/data/InflPosInc_data.txt", row.names = FALSE, sep =";", dec=".")

write.table(VirData[, .(COVID19PosInc = 10000*COVID19Pos/N), keyby = .(group, ISOweek)],
            file = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark/data/COVID19PosInc_data.txt", row.names = FALSE, sep =";", dec=".")
rm(GetVirData, GetDoB, GetMiBaData)

# Clinical data -----------------------------------------------------------
source("inst/scr/GetSILSData.R")
SILSData <- data.table(merge(GetSILSData(StartWeek, EndWeek), PopData, by = c('group', 'ISOweek')))

write.table(SILSData[, .(SRLSInc = 10000*SRLS/N), keyby = .(group, ISOweek)],
            file = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark/data/SRLSInc_data.txt", row.names = FALSE, sep =";", dec=".")

write.table(SILSData[, .(SCLSInc = 10000*SCLS/N), keyby = .(group, ISOweek)],
            file = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark/data/SCLSInc_data.txt", row.names = FALSE, sep =";", dec=".")
rm(GetSILSData)

# Goldstein data ----------------------------------------------------------
VirData$N <- NULL
GSData <- merge(SILSData, VirData, by = c('group', 'ISOweek'))

write.table(GSData[, .(GSIP = (100*InflPos/N)*(10000*SIPLS/N)), keyby = .(group, ISOweek)],
            file = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark/data/GSIP_data.txt", row.names = FALSE, sep =";", dec=".")

write.table(GSData[, .(GSR = (100*InflPos/N)*(10000*SRLS/N)), keyby = .(group, ISOweek)],
            file = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark/data/GSR_data.txt", row.names = FALSE, sep =";", dec=".")

write.table(GSData[, .(GSCOVID19 = (100*COVID19Pos/N)*(10000*SCLS/N)), keyby = .(group, ISOweek)],
            file = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark/data/GSCOVID19_data.txt", row.names = FALSE, sep =";", dec=".")
