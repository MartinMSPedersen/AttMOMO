library(data.table)
library(ggplot2)

AttData <- data.table(read.table(file = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark/AttMOMO_2020-W15/output/AttData_GSInfl_GSCOVID19.txt",
                                 sep = ";", header = TRUE, as.is = TRUE))

dt <- AttData_GS[(group == 'TotalPooled'), .(ISOweek, deaths, ET, GSInfl, GSCOVID19, VlogEB,
                                   EB, EB_2L = EB / exp(2*sqrt(VlogEB)), EB_2U = EB * exp(2*sqrt(VlogEB)), EB_4U = EB * exp(4*sqrt(VlogEB)),
                                   EBGSInfl = EB + EGSInfl,
                                   EBGSInflGSCOVID19 = EB + EGSInfl + EGSCOVID19,
                                   EBGSInflGSCOVID19ET = EB + EGSInfl + EGSCOVID19 + EET)]

dt$wk = as.numeric(as.factor(dt$ISOweek))
graph1 <- ggplot(dt, aes(x = wk)) +
  geom_line(aes(y = deaths, colour = "gray50"), linetype = "solid", size = 1.2) +
  geom_line(aes(y = EB_2L, colour = "black"), linetype = "dashed", size = 1) +
  geom_line(aes(y = EB_2U, colour = "black"), linetype = "dashed", size = 1) +
  geom_line(aes(y = EB_4U, colour = "black"), linetype = "dashed", size = 1) +
  geom_line(aes(y = EBGSInflGSCOVID19ET, colour = "green3"), linetype = "solid", size = 1) +
  geom_line(aes(y = EBGSInflGSCOVID19, colour = "chocolate4"), linetype = "solid", size = 1) +
  geom_line(aes(y = EBGSInfl, colour = "blue"), linetype = "solid", size = 1) +
  geom_line(aes(y = EB, colour = "grey0"), linetype = "solid", size = 1.2) +
  # ggtitle(paste0(c, ", ", g)) + theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(name = "ISOWeek",
                     labels = dt[seq(min(dt$wk), max(dt$wk), by = 4),]$ISOweek,
                     breaks = seq(min(dt$wk), max(dt$wk), by = 4)) +
  scale_y_continuous(name = "Number of deaths") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
  scale_color_identity(name = "",
                       breaks = c("gray50", "grey0", 'black', 'blue', 'chocolate4', 'green3'),
                       labels = c("Observed", "Baseline", "-2, +2, +4 z-scores", "Influenza", "COVID-19", "Extreme temperatures"),
                       guide = "legend")
print(graph1)

graph2 <- ggplot(dt, aes(x = wk)) +
  geom_line(aes(y = GSInfl, colour = "blue"), linetype = "solid", size = 1) +
  geom_line(aes(y = GSCOVID19, colour = "chocolate4"), linetype = "solid", size = 1) +
  # geom_line(aes(y = ET, colour = "green3"), linetype = "solid", size = 1) +
  # ggtitle(paste0(c, ", ", g)) + theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(name = "ISOWeek",
                     labels = dt[seq(min(dt$wk), max(dt$wk), by = 4),]$ISOweek,
                     breaks = seq(min(dt$wk), max(dt$wk), by = 4)) +
  scale_y_continuous(name = "Number of deaths") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
  scale_color_identity(name = "",
                       breaks = c('blue', 'chocolate4', 'green3'),
                       labels = c("GSInfl", "GSCOVID19", "Extreme temperatures"),
                       guide = "legend")
print(graph2)
