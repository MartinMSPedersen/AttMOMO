library(data.table)
library(ggplot2)

StartWeek <- '2020-W09'
EndWeek <- '2020-W20'

AttData <- data.table(read.table(file = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark/AttMOMO_2020-W22/output/AttData_GSIPLS_GSCLS.txt",
                                 sep = ";", header = TRUE, as.is = TRUE))[(StartWeek <= ISOweek) & (ISOweek <= EndWeek) & (group %in% c('Total', 'TotalPooled')),]


# round(c(sum(AttData$EGSIPLS), sum(AttData$EGSIPLS) - 1.96* sqrt(sum(AttData$VEGSIPLS)), sum(AttData$EGSIPLS) + 1.96* sqrt(sum(AttData$VEGSIPLS))))

GSCLS_week <- data.table(ISOweek = c(paste0('2020-W', sprintf("%02d", 9:20)), 'Total'))
for (g in c('Total', 'TotalPooled')) {
  # g <- 'Total'
  expr <- parse(text = paste0(".(B", g," = round(sum(EGSCLS), 2),
                               B", g,".low = round(qchisq(0.025, df = 2*sum(EGSCLS))/2, 2),
                               B", g,".high = round(qchisq(0.975, df = 2*(sum(EGSCLS)+1))/2, 2))"))
  GSCLS_week <- merge(GSCLS_week,
                      rbind(AttData[group == g, eval(expr), keyby = ISOweek], c(ISOweek = 'Total', AttData[group == g, eval(expr),])), by = 'ISOweek', all.x = TRUE)
}

AGSCLS_week <- data.table(ISOweek = c(paste0('2020-W', sprintf("%02d", 9:20)), 'Total'))
for (g in c('Total', 'TotalPooled')) {
  # g <- 'Total'
  expr <- parse(text = paste0(".(AB", g," = round(sum(EAGSCLS), 2),
                               AB", g,".low = round(
                               sum(EAGSCLS) + max(1, sum(VEAGSCLS)/sum(EAGSCLS))*qchisq(0.95, 1)/2 -
                               sqrt(max(1, sum(VEAGSCLS)/sum(EAGSCLS))*qchisq(0.95, 1))*
                               sqrt(sum(EAGSCLS) + max(1, sum(VEAGSCLS)/sum(EAGSCLS))*qchisq(0.95, 1)/4), 2),
                               AB", g,".high = round(
                               sum(EAGSCLS) + max(1, sum(VEAGSCLS)/sum(EAGSCLS))*qchisq(0.95, 1)/2 +
                               sqrt(max(1, sum(VEAGSCLS)/sum(EAGSCLS))*qchisq(0.95, 1))*
                               sqrt(sum(EAGSCLS) + max(1, sum(VEAGSCLS)/sum(EAGSCLS))*qchisq(0.95, 1)/4), 2))"))
  AGSCLS_week <- merge(AGSCLS_week,
                       rbind(AttData[group == g, eval(expr), keyby = ISOweek], c(ISOweek = 'Total', AttData[group == g, eval(expr),])), by = 'ISOweek', all.x = TRUE)
}

AGSCLS_week <- data.table(ISOweek = c(paste0('2020-W', sprintf("%02d", 9:20)), 'Total'))
for (g in c('Total', 'TotalPooled')) {
  # g <- 'Total'
  expr <- parse(text = paste0(".(AB", g," = round(sum(EAGSCLS), 2),
                               AB", g,".low = round(
                               sum(EAGSCLS) + qchisq(0.95, 1)/2 -
                               sqrt(qchisq(0.95, 1))*
                               sqrt(sum(EAGSCLS) + qchisq(0.95, 1)/4), 2),
                               AB", g,".high = round(
                               sum(EAGSCLS) + qchisq(0.95, 1)/2 +
                               sqrt(qchisq(0.95, 1))*
                               sqrt(sum(EAGSCLS) + qchisq(0.95, 1)/4), 2))"))
  AGSCLS_week <- merge(AGSCLS_week,
                       rbind(AttData[group == g, eval(expr), keyby = ISOweek], c(ISOweek = 'Total', AttData[group == g, eval(expr),])), by = 'ISOweek', all.x = TRUE)
}



GSCLS_week <- data.table(ISOweek = c(paste0('2020-W', sprintf("%02d", 9:20)), 'Total'))
for (g in c('Total', 'TotalPooled')) {
  # g <- 'Total'
  expr <- parse(text = paste0(".(B", g," = round(sum(EGSCLS), 2),
                               B", g,".low = round(sum(EGSCLS) - qnorm(0.975)*sqrt(sum(VEGSCLS)), 2),
                               B", g,".high = round(sum(EGSCLS) + qnorm(0.975)*sqrt(sum(VEGSCLS)), 2))"))
  GSCLS_week <- merge(GSCLS_week,
                      rbind(AttData[group == g, eval(expr), keyby = ISOweek], c(ISOweek = 'Total', AttData[group == g, eval(expr),])), by = 'ISOweek', all.x = TRUE)
}

AGSCLS_week <- data.table(ISOweek = c(paste0('2020-W', sprintf("%02d", 9:20)), 'Total'))
for (g in c('Total', 'TotalPooled')) {
  # g <- 'Total'
  expr <- parse(text = paste0(".(AB", g," = round(sum(EAGSCLS), 2),
                               AB", g,".low = round(sum(EAGSCLS) - qnorm(0.975)*sqrt(sum(VEAGSCLS)), 2),
                               AB", g,".high = round(sum(EAGSCLS) + qnorm(0.975)*sqrt(sum(VEAGSCLS)), 2))"))
  AGSCLS_week <- merge(AGSCLS_week,
                      rbind(AttData[group == g, eval(expr), keyby = ISOweek], c(ISOweek = 'Total', AttData[group == g, eval(expr),])), by = 'ISOweek', all.x = TRUE)
}

Table2 <- merge(GSCLS_week, AGSCLS_week, by = 'ISOweek')

source("inst/scr/Casefatality.R", encoding = "UTF-8")
Table2 <- merge(Table2, GetCOVID19CF(StartWeek, EndWeek), by = 'ISOweek')

Table2[, wk := as.numeric(factor(ISOweek))]
graph <- ggplot(Table2[ISOweek != 'Total', ], aes(x = wk)) +
  geom_line(aes(y = ABTotalPooled, colour = "gray10"), linetype = "solid", size = 1) +
  geom_line(aes(y = ABTotalPooled.low, colour = "black"), linetype = "dashed", size = 1) +
  geom_line(aes(y = ABTotalPooled.high, colour = "black"), linetype = "dashed", size = 1) +
  geom_line(aes(y = cf, colour = "blue"), linetype = "solid", size = 1) +
  geom_hline(yintercept = 0, colour = 'black') +
  # ggtitle("Deaths attributable to COVID-19") +
  scale_x_continuous(name = "ISOWeek",
                     labels = Table2$ISOweek,
                     breaks = seq(min(Table2$wk), max(Table2$wk), by = 1)) +
  scale_y_continuous(name = "Number of deaths") +
  theme(plot.title = element_text(face = 'bold', hjust = 0.5, size = 20), legend.position = "bottom", legend.text = element_text(size = 20),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
  scale_color_identity(name = "",
                       breaks = c('gray10', 'black', 'blue'),
                       labels = c("Pooled over age groups (Adjusted baseline model)", "95% CI", "CaseFatality"),
                       guide = "legend")
print(graph)
