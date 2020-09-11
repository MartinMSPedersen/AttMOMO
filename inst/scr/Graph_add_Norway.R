library(data.table)
library(ggplot2)
library(cowplot)

# Model -------------------------------------------------------------------

# AttData <- data.table(read.table(file = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Norway/AttMOMO_2020-W22/output/AttData_pr100_ili_total_gCOVID19.txt",
#                                  sep = ";", header = TRUE, as.is = TRUE))

AttData <- data.table(read.table(file = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Norway/AttMOMO_2020-W22/output/AttData_pr100_ili_total_covid_hospital.txt",
                                 sep = ";", header = TRUE, as.is = TRUE))

AttData[, `:=`(EB_2L = EB - 2*sqrt(VEB),
               EB_2U = EB + 2*sqrt(VEB),
               EB_4U = EB + 4*sqrt(VEB),
               EBpr100_ili_total = EB + Epr100_ili_total,
               EBpr100_ili_totalcovid_hospital = EB + Epr100_ili_total + Ecovid_hospital,
               EBpr100_ili_totalcovid_hospitalET = EB + Epr100_ili_total + Ecovid_hospital + EET,
               EAB_2L = EAB - 2*sqrt(VEAB),
               EAB_2U = EAB + 2*sqrt(VEAB),
               EAB_4U = EAB + 4*sqrt(VEAB),
               EABpr100_ili_total = EAB + EApr100_ili_total,
               EABpr100_ili_totalcovid_hospital = EAB + EApr100_ili_total + EAcovid_hospital,
               wk = as.numeric(factor(ISOweek)))]
# Exact 95% (qchisq(0.025, 2*x)/2, qchisq(0.975, 2*(x+1))/2)
# http://ms.mcmaster.ca/peter/s743/poissonalpha.html

# Crude graphs ------------------------------------------------------------

graph <- ggplot(AttData[group != 'TotalPooled',], aes(x = wk)) +
  geom_line(aes(y = deaths, colour = "gray50"), linetype = "solid", size = 1.2) +
  geom_line(aes(y = EB_2L, colour = "black"), linetype = "dashed", size = 1) +
  geom_line(aes(y = EB_2U, colour = "black"), linetype = "dashed", size = 1) +
  # geom_line(aes(y = EB_4U, colour = "black"), linetype = "dashed", size = 1) +
  geom_line(aes(y = EBpr100_ili_totalcovid_hospitalET, colour = "green3"), linetype = "solid", size = 1) +
  geom_line(aes(y = EBpr100_ili_totalcovid_hospital, colour = "chocolate4"), linetype = "solid", size = 1) +
  geom_line(aes(y = EBpr100_ili_total, colour = "blue"), linetype = "solid", size = 1) +
  geom_line(aes(y = EB, colour = "grey0"), linetype = "solid", size = 1.2) +
  facet_wrap(vars(group), scales = "free_y", ncol = 1) +
  scale_x_continuous(name = "ISOWeek",
                     labels = AttData[seq(min(AttData$wk), max(AttData$wk), by = 8),]$ISOweek,
                     breaks = seq(min(AttData$wk), max(AttData$wk), by = 8)) +
  scale_y_continuous(name = "Number of deaths") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
  scale_color_identity(name = "",
                       breaks = c("gray50", "grey0", 'black', 'blue', 'chocolate4', 'green3'),
                       labels = c("Observed", "Baseline", "+/- 2 z-scores", "Baseline + Influenza", "Baseline + Influenza + COVID-19",
                                  "Baseline + Influenza + COVID-19 + Extreme temperatures"),
                       guide = "legend")
print(graph)

# Zoom on 2019-W40 <= ISOweek <= 2020-W20
graph_zoom <- ggplot(AttData[('2019-W40' <= ISOweek) & (ISOweek <= '2020-W20') & (group == 'Total'), ], aes(x = wk)) +
  geom_line(aes(y = deaths, colour = "gray50"), linetype = "solid", size = 1.2) +
  geom_line(aes(y = EB_2L, colour = "black"), linetype = "dashed", size = 1) +
  geom_line(aes(y = EB_2U, colour = "black"), linetype = "dashed", size = 1) +
  geom_line(aes(y = EBpr100_ili_totalcovid_hospitalET, colour = "green3"), linetype = "solid", size = 1) +
  geom_line(aes(y = EBpr100_ili_totalcovid_hospital, colour = "chocolate4"), linetype = "solid", size = 1) +
  geom_line(aes(y = EBpr100_ili_total, colour = "blue"), linetype = "solid", size = 1) +
  geom_line(aes(y = EB, colour = "grey0"), linetype = "solid", size = 1.2) +
  facet_wrap(vars(group), scales = "free_y", ncol = 1) +
  scale_x_continuous(name = "ISOWeek",
                     labels = AttData[seq(min(AttData$wk), max(AttData$wk), by = 4),]$ISOweek,
                     breaks = seq(min(AttData$wk), max(AttData$wk), by = 4)) +
  scale_y_continuous(name = "Number of deaths") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
  scale_color_identity(name = "",
                       breaks = c("gray50", "grey0", 'black', 'blue', 'chocolate4', 'green3'),
                       labels = c("Observed", "Baseline", "+/- 2 z-scores", "Baseline + Influenza", "Baseline + Influenza + COVID-19",
                                  "Baseline + Influenza + COVID-19 + Extreme temperatures"),
                       guide = "legend")
print(graph_zoom)

plot_grid(graph, graph_zoom, align = 'h', labels = c('2014-W27 to 2020-W20', '2019-W40 to 2020-W20'), label_size = 10, ncol = 2, rel_widths = c(2, 1))


# Adjsuted graphs ---------------------------------------------------------

graph <- ggplot(AttData[group != 'TotalPooled',], aes(x = wk)) +
  geom_line(aes(y = deaths, colour = "gray50"), linetype = "solid", size = 1.2) +
  geom_line(aes(y = EAB_2L, colour = "black"), linetype = "dashed", size = 1) +
  geom_line(aes(y = EAB_2U, colour = "black"), linetype = "dashed", size = 1) +
  # geom_line(aes(y = EAB_4U, colour = "black"), linetype = "dashed", size = 1) +
  geom_line(aes(y = EABpr100_ili_totalcovid_hospital, colour = "chocolate4"), linetype = "solid", size = 1) +
  geom_line(aes(y = EABpr100_ili_total, colour = "blue"), linetype = "solid", size = 1) +
  geom_line(aes(y = EAB, colour = "grey0"), linetype = "solid", size = 1.2) +
  facet_wrap(vars(group), scales = "free_y", ncol = 1) +
  scale_x_continuous(name = "ISOWeek",
                     labels = AttData[seq(min(AttData$wk), max(AttData$wk), by = 8),]$ISOweek,
                     breaks = seq(min(AttData$wk), max(AttData$wk), by = 8)) +
  scale_y_continuous(name = "Number of deaths") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
  scale_color_identity(name = "",
                       breaks = c("gray50", "grey0", 'black', 'blue', 'chocolate4'),
                       labels = c("Observed", "Adjusted baseline", "+/- 2 z-scores", "Adjusted baseline + Influenza", "Adjusted baseline + Influenza + COVID-19"),
                       guide = "legend")
print(graph)

# Zoom on 2019-W40 <= ISOweek <= 2020-W20
graph_zoom <- ggplot(AttData[('2019-W40' <= ISOweek) & (ISOweek <= '2020-W20') & (group == 'Total'), ], aes(x = wk)) +
  geom_line(aes(y = deaths, colour = "gray50"), linetype = "solid", size = 1.2) +
  geom_line(aes(y = EAB_2L, colour = "black"), linetype = "dashed", size = 1) +
  geom_line(aes(y = EAB_2U, colour = "black"), linetype = "dashed", size = 1) +
  # geom_line(aes(y = EAB_4U, colour = "black"), linetype = "dashed", size = 1) +
  geom_line(aes(y = EABpr100_ili_totalcovid_hospital, colour = "chocolate4"), linetype = "solid", size = 1) +
  geom_line(aes(y = EABpr100_ili_total, colour = "blue"), linetype = "solid", size = 1) +
  geom_line(aes(y = EAB, colour = "grey0"), linetype = "solid", size = 1.2) +
  facet_wrap(vars(group), scales = "free_y", ncol = 1) +
  scale_x_continuous(name = "ISOWeek",
                     labels = AttData[seq(min(AttData$wk), max(AttData$wk), by = 4),]$ISOweek,
                     breaks = seq(min(AttData$wk), max(AttData$wk), by = 4)) +
  scale_y_continuous(name = "Number of deaths") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
  scale_color_identity(name = "",
                       breaks = c("gray50", "grey0", 'black', 'blue', 'chocolate4'),
                       labels = c("Observed", "Adjusted baseline", "+/- 2 z-scores", "Adjusted baseline + Influenza", "Adjusted baseline + Influenza + COVID-19"),
                       guide = "legend")
print(graph_zoom)

plot_grid(graph, graph_zoom, align = 'h', labels = c('2014-W27 to 2020-W20', '2019-W40 to 2020-W20'), label_size = 10, ncol = 2, rel_widths = c(2, 1))






# Figure 3 ----------------------------------------------------------------

graph_base <- ggplot(AttData[('2019-W40' <= ISOweek) & (ISOweek <= '2020-W20') & (group == 'Total'), ], aes(x = wk)) +
  geom_line(aes(y = deaths, colour = "gray50"), linetype = "solid", size = 1.2) +
  geom_line(aes(y = EB_2L, colour = "black"), linetype = "dashed", size = 1) +
  geom_line(aes(y = EB_2U, colour = "black"), linetype = "dashed", size = 1) +
  geom_line(aes(y = EBpr100_ili_totalcovid_hospitalET, colour = "green3"), linetype = "solid", size = 1) +
  geom_line(aes(y = EBpr100_ili_totalcovid_hospital, colour = "chocolate4"), linetype = "solid", size = 1) +
  geom_line(aes(y = EBpr100_ili_total, colour = "blue"), linetype = "solid", size = 1) +
  geom_line(aes(y = EB, colour = "grey0"), linetype = "solid", size = 1.2) +
  ggtitle("Base model") +
  scale_x_continuous(name = "ISOWeek",
                     labels = AttData[seq(min(AttData$wk), max(AttData$wk), by = 4),]$ISOweek,
                     breaks = seq(min(AttData$wk), max(AttData$wk), by = 4)) +
  scale_y_continuous(name = "Number of deaths") +
  theme(plot.title = element_text(face = 'bold', hjust = 0.5, size = 20), legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
  scale_color_identity(name = "",
                       breaks = c("gray50", "grey0", 'black', 'blue', 'chocolate4', 'green3'),
                       labels = c("Observed", "Baseline", "+/- 2 z-scores", "Baseline + Influenza", "Baseline + Influenza + COVID-19",
                                  "Baseline + Influenza + COVID-19 + Excess temperatures"),
                       guide = "legend")
# print(graph_base)

graph_adj <- ggplot(AttData[('2019-W40' <= ISOweek) & (ISOweek <= '2020-W20') & (group == 'Total'), ], aes(x = wk)) +
  geom_line(aes(y = deaths, colour = "gray50"), linetype = "solid", size = 1.2) +
  geom_line(aes(y = EAB_2L, colour = "black"), linetype = "dashed", size = 1) +
  geom_line(aes(y = EAB_2U, colour = "black"), linetype = "dashed", size = 1) +
  # geom_line(aes(y = EAB_4U, colour = "black"), linetype = "dashed", size = 1) +
  geom_line(aes(y = EABpr100_ili_totalcovid_hospital, colour = "chocolate4"), linetype = "solid", size = 1) +
  geom_line(aes(y = EABpr100_ili_total, colour = "blue"), linetype = "solid", size = 1) +
  geom_line(aes(y = EAB, colour = "grey0"), linetype = "solid", size = 1.2) +
  ggtitle("Adjusted baseline model") +
  scale_x_continuous(name = "ISOWeek",
                     labels = AttData[seq(min(AttData$wk), max(AttData$wk), by = 4),]$ISOweek,
                     breaks = seq(min(AttData$wk), max(AttData$wk), by = 4)) +
  scale_y_continuous(name = "Number of deaths") +
  theme(plot.title = element_text(face = 'bold', hjust = 0.5, size = 20), legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
  scale_color_identity(name = "",
                       breaks = c("gray50", "grey0", 'black', 'blue', 'chocolate4'),
                       labels = c("Observed", "Adjusted baseline", "+/- 2 z-scores", "Adjusted baseline + Influenza", "Adjusted baseline + Influenza + COVID-19"),
                       guide = "legend")
# print(graph_adj)

plot_grid(graph_base, graph_adj, align = 'h', ncol = 1, rel_widths = c(1, 1))








# ET graph ----------------------------------------------------------------

ETData <- data.table(read.table(file = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Norway/AttMOMO_2020-W22/data/ET_data.txt",
                                sep = ";", header = TRUE, as.is = TRUE))

ETData[, `:=`(tempCold = ifelse(temp < ptmin, temp, NA),
              tempWarm = ifelse(temp > ptmax, temp, NA),
              wk = as.numeric(factor(ISOweek)))]

graph <- ggplot(ETData, aes(x = wk)) +
  geom_ribbon(aes(ymin = ptmin, ymax = ptmax), fill = "gray85") +
  geom_point(aes(y = temp, colour = "black"), size = 1.5) +
  geom_point(aes(y = tempCold, colour = "blue"), size = 2) +
  geom_point(aes(y = tempWarm, colour = "red"), size = 2) +
  scale_x_continuous(name = "ISOWeek",
                     labels = ETData[seq(min(ETData$wk), max(ETData$wk), by = 4),]$ISOweek,
                     breaks = seq(min(ETData$wk), max(ETData$wk), by = 4)) +
  scale_y_continuous(name = "Degrees celcius") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
  scale_color_identity(name = "",
                       breaks = c('black', 'blue', 'red'),
                       labels = c("Normal week", "Cold week", "Warm week"),
                       guide = "legend")
print(graph)

# pr100_ili_total ------------------------------------------------------------------

pr100_ili_totalData <- data.table(read.table(file = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Norway/AttMOMO_2020-W22/data/pr100_ili_total_data.txt",
                                sep = ";", header = TRUE, as.is = TRUE))

pr100_ili_totalData[, `:=`(pr100_ili_total = 10 * pr100_ili_total, wk = as.numeric(factor(ISOweek)))]

graph <- ggplot(pr100_ili_totalData[('2019-W40' <= ISOweek) & (ISOweek <= '2020-W20') & (group == 'Total'), ], aes(x = wk)) +
  geom_line(aes(y = pr100_ili_total), colour = "black", linetype = "solid", size = 1.2) +
  scale_x_continuous(name = "ISOWeek",
                     labels = pr100_ili_totalData[seq(min(pr100_ili_totalData$wk), max(pr100_ili_totalData$wk), by = 4),]$ISOweek,
                     breaks = seq(min(pr100_ili_totalData$wk), max(pr100_ili_totalData$wk), by = 4)) +
  scale_y_continuous(name = "pr100_ili_total") +
  # facet_wrap(vars(group), scales = "free_y", ncol = 1) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, size = 7))

print(graph)

# covid_hospital ------------------------------------------------------------------

covid_hospitalData <- data.table(read.table(file = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Norway/AttMOMO_2020-W22/data/covid_hospital_data.txt",
                                   sep = ";", header = TRUE, as.is = TRUE))

covid_hospitalData[, wk := as.numeric(factor(ISOweek))]

graph <- ggplot(covid_hospitalData[('2019-W40' <= ISOweek) & (ISOweek <= '2020-W20') & (group == 'Total'), ], aes(x = wk)) +
  geom_line(aes(y = covid_hospital), colour = "black", linetype = "solid", size = 1.2) +
  scale_x_continuous(name = "ISOWeek",
                     labels = covid_hospitalData[seq(min(covid_hospitalData$wk), max(covid_hospitalData$wk), by = 4),]$ISOweek,
                     breaks = seq(min(covid_hospitalData$wk), max(covid_hospitalData$wk), by = 4)) +
  scale_y_continuous(name = "covid_hospital") +
  # facet_wrap(vars(group), scales = "free_y", ncol = 1) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, size = 7))

print(graph)

# Indicators --------------------------------------------------------------

indicators <- merge(pr100_ili_totalData <- data.table(read.table(file = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Norway/AttMOMO_2020-W22/data/pr100_ili_total_data.txt",
                                                        sep = ";", header = TRUE, as.is = TRUE)),
                    covid_hospitalData <- data.table(read.table(file = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Norway/AttMOMO_2020-W22/data/covid_hospital_data.txt",
                                                       sep = ";", header = TRUE, as.is = TRUE)),
                    by = c('group', 'ISOweek'))
indicators[, `:=`(pr100_ili_total = 100000 * pr100_ili_total, wk = as.numeric(factor(ISOweek)))]

graph <- ggplot(indicators[('2014-W27' <= ISOweek) & (ISOweek <= '2020-W20') & (group == 'Total'),], aes(x = wk)) +
  geom_line(aes(y = covid_hospital, colour = "chocolate4"), linetype = "solid", size = 1.2) +
  geom_line(aes(y = pr100_ili_total, colour = "blue"), linetype = "solid", size = 1.2) +
  scale_x_continuous(name = "ISOWeek",
                     labels = indicators[seq(min(indicators$wk), max(indicators$wk), by = 8),]$ISOweek,
                     breaks = seq(min(indicators$wk), max(indicators$wk), by = 8)) +
  scale_y_continuous(name = "Indicator value") +
  # facet_wrap(vars(group), scales = "free_y", ncol = 1) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
  scale_color_identity(name = "",
                       breaks = c('blue', 'chocolate4'),
                       labels = c("10 * pr100_ili_total", "covid_hospital"),
                       guide = "legend")
print(graph)

# Zoom on ISOweek >= 2019-W40
graph_zoom <- ggplot(indicators[('2019-W40' <= ISOweek) & (ISOweek <= '2020-W20') & (group == 'Total'),], aes(x = wk)) +
  geom_line(aes(y = covid_hospital, colour = "chocolate4"), linetype = "solid", size = 1.2) +
  geom_line(aes(y = pr100_ili_total, colour = "blue"), linetype = "solid", size = 1.2) +
  scale_x_continuous(name = "ISOWeek",
                     labels = indicators[seq(min(indicators$wk), max(indicators$wk), by = 4),]$ISOweek,
                     breaks = seq(min(indicators$wk), max(indicators$wk), by = 4)) +
  scale_y_continuous(name = "") +
  # facet_wrap(vars(group), scales = "free_y", ncol = 1) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
  scale_color_identity(name = "",
                       breaks = c('blue', 'chocolate4'),
                       labels = c("10 * pr100_ili_total", "covid_hospital"),
                       guide = "legend")
print(graph_zoom)

plot_grid(graph, graph_zoom, align = 'h', labels = c('2014-W27 to 2020-W20', '2019-W40 to 2020-W20'), label_size = 20, ncol = 2, rel_widths = c(2, 1))
