library(data.table)
library(ggplot2)

StartWeek <- '2014-W27'
EndWeek <- '2020-W16'

con <- RODBC::odbcConnect("EpiLPR3_PROD", readOnlyOptimize = TRUE)
ILIData <- RODBC::sqlQuery(con, paste0("
      set transaction isolation level read uncommitted;
      set deadlock_priority 5;
      select * from IB_sentinel.dbo.influenza with(nolock)
     where (", substr(StartWeek,1,4), "<= Meldeaar) and (1 <= Meldeuge) and (Meldeuge <= 53)
    "), stringsAsFactors = FALSE, as.is = TRUE)
RODBC::odbcClose(con)
rm(con)
ILIData <- setDT(ILIData)[, .(AntalILI = sum(`Inflsym ialt`, na.rm = TRUE), Antalkonsultationer = sum(Antalkonsultationer, na.rm = TRUE)),
                          keyby = .(ISOweek = paste0(Meldeaar, "-W", sprintf("%02d", Meldeuge)))]
ILIData[, ILI := 100* AntalILI/Antalkonsultationer]
ILIData <- merge(data.table(ISOweek = ISOweek::ISOweek(seq(as.Date(ISOweek::ISOweek2date(paste0(StartWeek, "-4"))),
                     as.Date(ISOweek::ISOweek2date(paste0(EndWeek, "-4"))), by = 'week'))), ILIData, by = "ISOweek", all.x = TRUE)
ILIData[is.na(ILIData$ILI),]$ILI <- 0



dt <- merge(ILIData,
            data.table(read.table(file = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark/data/InflPosInc_data.txt",
                           header = TRUE, sep = ";", dec = ".", as.is =  TRUE))[group == 'Total', .(ISOweek, InflPosInc)], by = 'ISOweek', all.x = TRUE)
dt <- merge(dt,
            data.table(read.table(file = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark/data/GSRLS_data.txt",
                                  header = TRUE, sep = ";", dec = ".", as.is =  TRUE))[group == 'Total', .(ISOweek, GSRLS)], by = 'ISOweek', all.x = TRUE)
dt <- merge(dt,
            data.table(read.table(file = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark/data/GSIPLS_data.txt",
                                  header = TRUE, sep = ";", dec = ".", as.is =  TRUE))[group == 'Total', .(ISOweek, GSIPLS)], by = 'ISOweek', all.x = TRUE)
dt[, `:=`(GILI = ILI*InflPosInc/10)]


dt$wk = as.numeric(as.factor(dt$ISOweek))

graph1 <- ggplot(dt, aes(x = wk)) +
  geom_line(aes(y = GILI, colour = "black"), linetype = "solid", size = 1) +
  geom_line(aes(y = GSIPLS, colour = "green3"), linetype = "solid", size = 1) +
  geom_line(aes(y = GSRLS, colour = "red"), linetype = "solid", size = 1) +
  # ggtitle(paste0(c, ", ", g)) + theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(name = "ISOWeek",
                     labels = dt[seq(min(dt$wk), max(dt$wk), by = 4),]$ISOweek,
                     breaks = seq(min(dt$wk), max(dt$wk), by = 4)) +
  scale_y_continuous(name = "Percent  or per 100.000 population") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
  scale_color_identity(name = "",
                       breaks = c('black', 'red', 'green3'),
                       labels = c("GILI", 'GSRLS', "GSIPLS"),
                       guide = "legend")
print(graph1)

graph1 <- ggplot(dt, aes(x = wk)) +
  geom_line(aes(y = GSILI, colour = "black"), linetype = "solid", size = 1) +
  geom_line(aes(y = GSInfl, colour = "green3"), linetype = "solid", size = 1.2) +
  # ggtitle(paste0(c, ", ", g)) + theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(name = "ISOWeek",
                     labels = dt[seq(min(dt$wk), max(dt$wk), by = 4),]$ISOweek,
                     breaks = seq(min(dt$wk), max(dt$wk), by = 4)) +
  scale_y_continuous(name = "Goldstein Index") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
  scale_color_identity(name = "",
                       breaks = c('black', 'green3'),
                       labels = c("GSILI", "GSSRLS"),
                       guide = "legend")
print(graph1)

graph1 <- ggplot(dt, aes(x = wk)) +
  geom_line(aes(y = GSILI, colour = "black"), linetype = "solid", size = 1) +
  geom_line(aes(y = GSInfl, colour = "green3"), linetype = "solid", size = 1.2) +
  # ggtitle(paste0(c, ", ", g)) + theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(name = "ISOWeek",
                     labels = dt[seq(min(dt$wk), max(dt$wk), by = 4),]$ISOweek,
                     breaks = seq(min(dt$wk), max(dt$wk), by = 4)) +
  scale_y_continuous(name = "Goldstein Index") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
  scale_color_identity(name = "",
                       breaks = c('black', 'green3'),
                       labels = c("GSILI/10", "GSSRLS"),
                       guide = "legend")
print(graph1)






dt[, `:=`(logGSInfl = log(GSInfl), logGSILI = log(GSILI))]

ggplot(dt[(GSIPLS > 0) & (GSRLS > 0),], aes(x = GSIPLS, y = GSRLS)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

summary(glm(GSILI ~ 0 + GSInfl, data = dt[(GSInfl > 0) & (GSILI > 0),]))


summary(glm(GSILI ~ 0 + GSInfl, data = dt))
# log(GSILI) = 1.2*log(GSSRLS) <=> GSILI = GSSRLS^1.2

# GSILI = exp(GSSRLS) <=> log(GSILI) = GSSRLS

ggplot(dt[!is.infinite(logGSILI),], aes(x = GSInfl, y = logGSILI)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

ggplot(dt[(!is.infinite(logGSInfl) & !is.infinite(logGSILI)),], aes(x = logGSInfl, y = logGSILI)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

summary(glm(logGSILI ~ logGSInfl, data = dt[(!is.infinite(logGSInfl) & !is.infinite(logGSILI)),]))
# log(GSILI) = 1.2*log(GSSRLS) <=> GSILI = GSSRLS^1.2

summary(glm(logGSInfl ~ logGSILI, data = dt[(!is.infinite(logGSInfl) & !is.infinite(logGSILI)),]))
# log(GSSRLS) = 0.7*log(GSSRLS) <=> GSSRLS = GSSRLS^0.7



# COVID-19 indicators -----------------------------------------------------

dt <- data.table(read.table(file = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark/AttMOMO_2020-W16/data/COVID19PosPct_data.txt",
                      header = TRUE, sep = ";", dec = ".", as.is =  TRUE))[(group == 'Total') & (ISOweek > '2020-01-01'),]
dt <- merge(dt,
            data.table(read.table(file = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark/AttMOMO_2020-W16/data/SCLS_data.txt",
                                  header = TRUE, sep = ";", dec = ".", as.is =  TRUE))[(group == 'Total') & (ISOweek > '2020-01-01'),], by = c('group', 'ISOweek'), all.x = TRUE)
dt <- merge(dt,
            data.table(read.table(file = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark/AttMOMO_2020-W16/data/COVID19PosInc_data.txt",
                                  header = TRUE, sep = ";", dec = ".", as.is =  TRUE))[(group == 'Total') & (ISOweek > '2020-01-01'),], by = c('group', 'ISOweek'), all.x = TRUE)
dt <- merge(dt,
            data.table(read.table(file = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark/AttMOMO_2020-W16/data/SCLSInc_data.txt",
                                  header = TRUE, sep = ";", dec = ".", as.is =  TRUE))[(group == 'Total') & (ISOweek > '2020-01-01'),], by = c('group', 'ISOweek'), all.x = TRUE)
dt <- merge(dt,
            data.table(read.table(file = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark/AttMOMO_2020-W16/data/GSCOVID19_data.txt",
                                  header = TRUE, sep = ";", dec = ".", as.is =  TRUE))[(group == 'Total') & (ISOweek > '2020-01-01'),], by = c('group', 'ISOweek'), all.x = TRUE)

dt$wk = as.numeric(as.factor(dt$ISOweek))

graph1 <- ggplot(dt, aes(x = wk)) +
  geom_line(aes(y = COVID19PosPct, colour = "black"), linetype = "solid", size = 1) +
  geom_line(aes(y = SCLS, colour = "green3"), linetype = "solid", size = 1) +
  # ggtitle(paste0(c, ", ", g)) + theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(name = "ISOWeek",
                     labels = dt[seq(min(dt$wk), max(dt$wk), by = 1),]$ISOweek,
                     breaks = seq(min(dt$wk), max(dt$wk), by = 1)) +
  scale_y_continuous(name = "Consultation percent") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
  scale_color_identity(name = "",
                       breaks = c('black', 'green3'),
                       labels = c("PosPct", "SCLS"),
                       guide = "legend")
print(graph1)

graph1 <- ggplot(dt, aes(x = wk)) +
  geom_line(aes(y = COVID19PosInc, colour = "black"), linetype = "solid", size = 1) +
  geom_line(aes(y = SCLSInc, colour = "green3"), linetype = "solid", size = 1) +
  geom_line(aes(y = 10*GSCOVID19, colour = "red"), linetype = "solid", size = 1) +
  # ggtitle(paste0(c, ", ", g)) + theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(name = "ISOWeek",
                     labels = dt[seq(min(dt$wk), max(dt$wk), by = 1),]$ISOweek,
                     breaks = seq(min(dt$wk), max(dt$wk), by = 1)) +
  scale_y_continuous(name = "per 100.000 population") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
  scale_color_identity(name = "",
                       breaks = c('black', 'green3', 'red'),
                       labels = c("PosInc", "SCLSInc", '10*GSCOVID19'),
                       guide = "legend")
print(graph1)


graph1 <- ggplot(dt, aes(x = wk)) +
  geom_line(aes(y = COVID19PosInc, colour = "black"), linetype = "solid", size = 1) +
  geom_line(aes(y = SCLSInc, colour = "green3"), linetype = "solid", size = 1) +
  geom_line(aes(y = SCLSInc, colour = "green3"), linetype = "solid", size = 1) +
  # ggtitle(paste0(c, ", ", g)) + theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(name = "ISOWeek",
                     labels = dt[seq(min(dt$wk), max(dt$wk), by = 1),]$ISOweek,
                     breaks = seq(min(dt$wk), max(dt$wk), by = 1)) +
  scale_y_continuous(name = "per 100.000 population") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
  scale_color_identity(name = "",
                       breaks = c('black', 'green3'),
                       labels = c("PosInc", "SCLSInc"),
                       guide = "legend")
print(graph1)
