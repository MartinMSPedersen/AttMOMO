library(data.table)
library(ggplot2)
library(cowplot)

# Model -------------------------------------------------------------------

AttData <- data.table(read.table(file = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark/AttMOMO_2020-W22/output/AttData_GSIPLS_GSCLS.txt",
                                 sep = ";", header = TRUE, as.is = TRUE))

AttData[, `:=`(EB_2L = EB - 2*sqrt(VEB),
               EB_2U = EB + 2*sqrt(VEB),
               EB_4U = EB + 4*sqrt(VEB),
               EBGSIPLS = EB + EGSIPLS,
               EBGSIPLSGSCLS = EB + EGSIPLS + EGSCLS,
               EBGSIPLSGSCLSET = EB + EGSIPLS + EGSCLS + EET,
               EAB_2L = EAB - 2*sqrt(VEAB),
               EAB_2U = EAB + 2*sqrt(VEAB),
               EAB_4U = EAB + 4*sqrt(VEAB),
               EABGSIPLS = EAB + EAGSIPLS,
               EABGSIPLSGSCLS = EAB + EAGSIPLS + EAGSCLS,
               wk = as.numeric(factor(ISOweek)))]
# Exact 95% (qchisq(0.025, 2*x)/2, qchisq(0.975, 2*(x+1))/2)
# http://ms.mcmaster.ca/peter/s743/poissonalpha.html

# Crude graphs ------------------------------------------------------------

graph <- ggplot(AttData[group != 'TotalPooled',], aes(x = wk)) +
  geom_line(aes(y = deaths, colour = "gray50"), linetype = "solid", size = 1.2) +
  geom_line(aes(y = EB_2L, colour = "black"), linetype = "dashed", size = 1) +
  geom_line(aes(y = EB_2U, colour = "black"), linetype = "dashed", size = 1) +
  # geom_line(aes(y = EB_4U, colour = "black"), linetype = "dashed", size = 1) +
  geom_line(aes(y = EBGSIPLSGSCLSET, colour = "green3"), linetype = "solid", size = 1) +
  geom_line(aes(y = EBGSIPLSGSCLS, colour = "chocolate4"), linetype = "solid", size = 1) +
  geom_line(aes(y = EBGSIPLS, colour = "blue"), linetype = "solid", size = 1) +
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
  geom_line(aes(y = EBGSIPLSGSCLSET, colour = "green3"), linetype = "solid", size = 1) +
  geom_line(aes(y = EBGSIPLSGSCLS, colour = "chocolate4"), linetype = "solid", size = 1) +
  geom_line(aes(y = EBGSIPLS, colour = "blue"), linetype = "solid", size = 1) +
  geom_line(aes(y = EB, colour = "grey0"), linetype = "solid", size = 1.2) +
  geom_line(aes(y = EAB, colour = "red"), linetype = "solid", size = 1.2) +
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
  geom_line(aes(y = EABGSIPLSGSCLS, colour = "chocolate4"), linetype = "solid", size = 1) +
  geom_line(aes(y = EABGSIPLS, colour = "blue"), linetype = "solid", size = 1) +
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
  geom_line(aes(y = EABGSIPLSGSCLS, colour = "chocolate4"), linetype = "solid", size = 1) +
  geom_line(aes(y = EABGSIPLS, colour = "blue"), linetype = "solid", size = 1) +
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
  geom_line(aes(y = EBGSIPLSGSCLSET, colour = "green3"), linetype = "solid", size = 1) +
  geom_line(aes(y = EBGSIPLSGSCLS, colour = "chocolate4"), linetype = "solid", size = 1) +
  geom_line(aes(y = EBGSIPLS, colour = "blue"), linetype = "solid", size = 1) +
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
  geom_line(aes(y = EABGSIPLSGSCLS, colour = "chocolate4"), linetype = "solid", size = 1) +
  geom_line(aes(y = EABGSIPLS, colour = "blue"), linetype = "solid", size = 1) +
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

ETData <- data.table(read.table(file = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark/AttMOMO_2020-W20/data/ET_data.txt",
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

# GSIPLS ------------------------------------------------------------------

GSIPLSData <- data.table(read.table(file = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark/AttMOMO_2020-W20/data/GSIPLS_data.txt",
                                sep = ";", header = TRUE, as.is = TRUE))

GSIPLSData[, `:=`(GSIPLS = 10 * GSIPLS, wk = as.numeric(factor(ISOweek)))]

graph <- ggplot(GSIPLSData[('2019-W40' <= ISOweek) & (ISOweek <= '2020-W20') & (group == 'Total'), ], aes(x = wk)) +
  geom_line(aes(y = GSIPLS), colour = "black", linetype = "solid", size = 1.2) +
  scale_x_continuous(name = "ISOWeek",
                     labels = GSIPLSData[seq(min(GSIPLSData$wk), max(GSIPLSData$wk), by = 4),]$ISOweek,
                     breaks = seq(min(GSIPLSData$wk), max(GSIPLSData$wk), by = 4)) +
  scale_y_continuous(name = "GSIPLS") +
  # facet_wrap(vars(group), scales = "free_y", ncol = 1) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, size = 7))

print(graph)

# GSCLS ------------------------------------------------------------------

GSCLSData <- data.table(read.table(file = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark/AttMOMO_2020-W19/data/GSCLS_data.txt",
                                   sep = ";", header = TRUE, as.is = TRUE))

GSCLSData[, wk := as.numeric(factor(ISOweek))]

graph <- ggplot(GSCLSData[('2019-W40' <= ISOweek) & (ISOweek <= '2020-W20') & (group == 'Total'), ], aes(x = wk)) +
  geom_line(aes(y = GSCLS), colour = "black", linetype = "solid", size = 1.2) +
  scale_x_continuous(name = "ISOWeek",
                     labels = GSCLSData[seq(min(GSCLSData$wk), max(GSCLSData$wk), by = 4),]$ISOweek,
                     breaks = seq(min(GSCLSData$wk), max(GSCLSData$wk), by = 4)) +
  scale_y_continuous(name = "GSCLS") +
  # facet_wrap(vars(group), scales = "free_y", ncol = 1) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, size = 7))

print(graph)

# Indicators --------------------------------------------------------------

indicators <- merge(GSIPLSData <- data.table(read.table(file = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark/AttMOMO_2020-W22/data/GSIPLS_data.txt",
                                                        sep = ";", header = TRUE, as.is = TRUE)),
                    GSCLSData <- data.table(read.table(file = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark/AttMOMO_2020-W22/data/GSCLS_data.txt",
                                                       sep = ";", header = TRUE, as.is = TRUE)),
                    by = c('group', 'ISOweek'))
indicators[, `:=`(GSIPLS = 10 * GSIPLS, wk = as.numeric(factor(ISOweek)))]

graph <- ggplot(indicators[('2014-W27' <= ISOweek) & (ISOweek <= '2020-W20') & (group == 'Total'),], aes(x = wk)) +
  geom_line(aes(y = GSCLS, colour = "chocolate4"), linetype = "solid", size = 1.2) +
  geom_line(aes(y = GSIPLS, colour = "blue"), linetype = "solid", size = 1.2) +
  scale_x_continuous(name = "ISOWeek",
                     labels = indicators[seq(min(indicators$wk), max(indicators$wk), by = 8),]$ISOweek,
                     breaks = seq(min(indicators$wk), max(indicators$wk), by = 8)) +
  scale_y_continuous(name = "Indicator value") +
  # facet_wrap(vars(group), scales = "free_y", ncol = 1) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
  scale_color_identity(name = "",
                       breaks = c('blue', 'chocolate4'),
                       labels = c("10 * GSIPLS", "GSCLS"),
                       guide = "legend")
print(graph)

# Zoom on ISOweek >= 2019-W40
graph_zoom <- ggplot(indicators[('2019-W40' <= ISOweek) & (ISOweek <= '2020-W20') & (group == 'Total'),], aes(x = wk)) +
  geom_line(aes(y = GSCLS, colour = "chocolate4"), linetype = "solid", size = 1.2) +
  geom_line(aes(y = GSIPLS, colour = "blue"), linetype = "solid", size = 1.2) +
  scale_x_continuous(name = "ISOWeek",
                     labels = indicators[seq(min(indicators$wk), max(indicators$wk), by = 4),]$ISOweek,
                     breaks = seq(min(indicators$wk), max(indicators$wk), by = 4)) +
  scale_y_continuous(name = "") +
  # facet_wrap(vars(group), scales = "free_y", ncol = 1) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
  scale_color_identity(name = "",
                       breaks = c('blue', 'chocolate4'),
                       labels = c("10 * GSIPLS", "GSCLS"),
                       guide = "legend")
print(graph_zoom)

plot_grid(graph, graph_zoom, align = 'h', labels = c('2014-W27 to 2020-W20', '2019-W40 to 2020-W20'), label_size = 20, ncol = 2, rel_widths = c(2, 1))



# Adjusted baseline illustration ------------------------------------------

AttData <- data.table(read.table(file = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark/AttMOMO_2020-W22/output/AttData_GSIPLS_GSCLS.txt",
                                 sep = ";", header = TRUE, as.is = TRUE))

AttData[, `:=`(EBET = EB + EET,
               wk = as.numeric(factor(ISOweek)))]

graph_zoom <- ggplot(AttData[('2019-W40' <= ISOweek) & (ISOweek <= '2020-W20') & (group == 'Total'), ], aes(x = wk)) +
  geom_line(aes(y = deaths, colour = "gray70"), linetype = "solid", size = 1.2) +
  geom_line(aes(y = EB, colour = "black"), linetype = "solid", size = 1) +
  geom_line(aes(y = EBET, colour = "green3"), linetype = "solid", size = 1) +
  geom_line(aes(y = EAB, colour = "red"), linetype = "solid", size = 1.2) +
  scale_x_continuous(name = "ISOWeek",
                     labels = AttData[seq(min(AttData$wk), max(AttData$wk), by = 4),]$ISOweek,
                     breaks = seq(min(AttData$wk), max(AttData$wk), by = 4)) +
  scale_y_continuous(name = "Number of deaths") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
  scale_color_identity(name = "",
                       breaks = c("gray70", 'black', 'green3', 'red'),
                       labels = c("Observed", "Baseline", "Adjusted to excess temperatures", "Adjusted to excess temperatures and benign effects"),
                       guide = "legend")
print(graph_zoom)
