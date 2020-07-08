# Install required packages (if not already installed)
#install.packages("ISOweek")


### Clear enviromnet i.e. delete all data and variabels
rm(list = ls())
# Clear console
cat("\014")

### Country
country <- "Denmark"
country.code <- "DK"

### Work directory - directory where the R programs are placed
wdir <- "."

### Period: Start and end (both included)
start_year <- 2013
start_week <- 40
end_year <- 2018
end_week <- 50

### Deaths data source
### Must be placed in the subdirectory /data in the work directory
# 1 = A-MOMO complete file, renamed to A-MOMO data.txt
# 0 = you provide a ;-separated file: deaths.txt, containing at least the variable: agegrp, year, week, deaths
A_MOMO <- 1

### Weather data source
# 1 = automatic download from EuroMOMO website
# 0 = you provide a ;-separated file: wdata_'country.code'.txt, containing at least the variables: date, pop3, NUTS3, temp and placed in the subdirectory /data in the work directory
WeatherData <- 1

### Population data (TRUE/FALSE)
### A ;-separated file: population.txt, must be placed in the subdirectory /data in the work directory
population <- TRUE

# Restrict IA to only positive (TRUE/FALSE)
IArest <- TRUE

# Number of IA lags
# 0 = no lag, 1 = one week lag, ...(max=9)
IAlags <- 2

# Number of OF lags
# 0 = no lag, 1 = one week lag, ...(max=9)
ETlags <- 2

source("R/Estimation.R")
estimation(
  wdir="H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark",
  country = "Denmark",
  StartWeek = '2015-W27',
  EndWeek = '2020-W15',
  groups = c('00to04', '05to14', '15to64', '65P', '65to74', '75to84', '85P', 'Total'),
  indicators <- c('InflPosPct', 'COVID19PosPct'),
  restrict = TRUE,
  population = FALSE,
  lags <- 2,
  ptrend = 0.05,
  p26 = 0.05,
  p52 = 0.10
)




# Estimation
source(paste0(wdir,"/Estimation_v42.R"), echo = FALSE)
# Output: txt-files
source(paste0(wdir,"/Output_txt_v42.R"))
# Output: graphs IA and ET
source(paste0(wdir,"/Output_IA_ET_v42.R"))
# Output: graphs over calendar time
source(paste0(wdir,"/Output_calendar_v42.R"))
# Output: graphs cumulated IA
source(paste0(wdir,"/Output_cumulated_v42.R"))
