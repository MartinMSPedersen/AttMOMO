





# Baseline, EIA and EET estimation variances - not on log scale
AttData$VB <- with(AttData, (exp(VlogB)-1)*exp(2*log(EB) + VlogB))
AttData$VIA <- with(AttData, (exp(VlogIA)-1)*exp(2*log(EIA) + VlogIA))
AttData$VET1 <- with(AttData, (exp(VlogET)-1)*exp(2*log(pET) + VlogET))

# Effects of IA and ET
AttData$EdIA <- with(AttData, EIA - EB)
AttData[is.na(AttData$EdIA),"EdIA"] <- 0
AttData$EdET <- with(AttData, EET - EB)
AttData[is.na(AttData$EdET),"EdET"] <- 0
# Excess relative to baseline
AttData$excess <- with(AttData, deaths - EB)
# Unexplained excess
AttData$uexcess <- with(AttData, deaths - (EB + EdIA + EdET))
# Exclude negative IA effects
if (IArest) AttData$uexcess <- with(AttData, deaths - (EB + pmax(0,EdIA) + EdET))

# Baseline 2/3 residual confidence intervals
AttData$RVB <- with(AttData, ((2/3)*(EB^(2/3-1))^2)*Vdeaths + ((2/3)*(EB^(2/3-1))^2)*VB)
AttData[with(AttData, is.na(RVB) | is.infinite(RVB)), "RVB"] <- 0
AttData$EB_95L <- with(AttData, pmax(0,sign((sign(EB)*abs(EB)^(2/3))-1.96*sqrt(RVB))*abs((sign(EB)*abs(EB)^(2/3))-1.96*sqrt(RVB))^(3/2)))
AttData$EB_95U <- with(AttData, sign((sign(EB)*abs(EB)^(2/3))+1.96*sqrt(RVB))*abs((sign(EB)*abs(EB)^(2/3))+1.96*sqrt(RVB))^(3/2))

# EdIA 2/3 residual confidence intervals
AttData$RVdIA <- with(AttData, ((2/3)*(EB^(2/3-1))^2)*VB + ((2/3)*(EIA^(2/3-1))^2)*VIA)
AttData[with(AttData, is.na(RVdIA) | is.infinite(RVdIA)), "RVdIA"] <- 0
AttData$EdIA_95L <- with(AttData, sign((sign(EdIA)*abs(EdIA)^(2/3))-1.96*sqrt(RVdIA))*abs((sign(EdIA)*abs(EdIA)^(2/3))-1.96*sqrt(RVdIA))^(3/2))
AttData$EdIA_95U <- with(AttData, pmax(EdIA_95L,sign((sign(EdIA)*abs(EdIA)^(2/3))+1.96*sqrt(RVdIA))*abs((sign(EdIA)*abs(EdIA)^(2/3))+1.96*sqrt(RVdIA))^(3/2)))

# EdET 2/3 residual confidence intervals
AttData$RVdET <- with(AttData, ((2/3)*(EB^(2/3-1))^2)*VB + ((2/3)*(EET^(2/3-1))^2)*VET)
AttData[with(AttData, is.na(RVdET) | is.infinite(RVdET)), "RVdET"] <- 0
AttData$EdET_95L <- with(AttData, sign((sign(EdET)*abs(EdET)^(2/3))-1.96*sqrt(RVdET))*abs((sign(EdET)*abs(EdET)^(2/3))-1.96*sqrt(RVdET))^(3/2))
AttData$EdET_95U <- with(AttData, pmax(EdET_95L,sign((sign(EdET)*abs(EdET)^(2/3))+1.96*sqrt(RVdET))*abs((sign(EdET)*abs(EdET)^(2/3))+1.96*sqrt(RVdET))^(3/2)))

AttData$summer <- with(AttData, ifelse(week>=21 & week<=39, year, NA))
AttData$winter <- with(AttData, ifelse(week<=20 | week>=40, year - (week<=20), NA))

# Order
AttData <- AttData[with(AttData, order(agegrp, year, week)),]

cummulate <- function(x, pos=FALSE) {
  # x = variable, EdIA
  y <- x[,1]
  if (pos) y <- x[,1] * (x[,2] > 0)
  return(cumsum(y))
}
cCI <- function(x) {
  # x = cEB, cVB, cE*, cV*
  colnames(x) <- c("cEB", "cVB", "cE", "cV")
  x$cEd <- x$cE - x$cEB
  x$cRVd <- ((2/3)*(x$cEB^(2/3-1))^2)*x$cVB + ((2/3)*(x$cE^(2/3-1))^2)*x$cV
  x[with(x, is.na(cRVd) | is.infinite(cRVd)), "cRVd"] <- 0
  x$cCI_95L <- with(x, sign((sign(cEd)*abs(cEd)^(2/3))-1.96*sqrt(cRVd))*abs((sign(cEd)*abs(cEd)^(2/3))-1.96*sqrt(cRVd))^(3/2))
  x$cCI_95U <- with(x, sign((sign(cEd)*abs(cEd)^(2/3))+1.96*sqrt(cRVd))*abs((sign(cEd)*abs(cEd)^(2/3))+1.96*sqrt(cRVd))^(3/2))
  return(x[, c("cEd", "cCI_95L", "cCI_95U")])
}

# EdIA and EdET residual variances
for (s in c("summer", "winter", "year")) {
  nn <- !is.na(AttData[,s])  # logical vector indicating season
  for (a in 0:4) {
    ag <- (group == g)
    for (v in c("excess","uexcess")) {
      AttData[nn & ag ,paste0("c", v, "_", s)] <- NA
      for (sy in sort(unique(AttData[,s]))) {
        AttData[(AttData[,s]==sy) & nn & ag, paste0("c", v, "_", s)] <-
          cummulate(AttData[(AttData[,s]==sy) & nn & ag, c(v, "EdIA")], FALSE)
      }
    }
    for (v in c("B","IA")) {
      AttData[nn & ag ,paste0("cE", v, "_", s)] <- NA
      AttData[nn & ag ,paste0("cV", v, "_", s)] <- NA
      for (sy in sort(unique(AttData[,s]))) {
        AttData[(AttData[,s]==sy) & nn & ag, paste0("cE", v, "_", s)] <-
          cummulate(AttData[(AttData[,s]==sy) & nn & ag, c(paste0("E",v),"EdIA")], IArest)
        AttData[(AttData[,s]==sy) & nn & ag, paste0("cV", v, "_", s)] <-
          cummulate(AttData[(AttData[,s]==sy) & nn & ag, c(paste0("V",v),"EdIA")],  IArest)
      }
    }
    AttData[nn & ag, paste0("cEd", v, "_", s)] <- NA
    AttData[nn & ag, paste0("cEd", v, "_", s, "_95L")] <- NA
    AttData[nn & ag, paste0("cEd", v, "_", s, "_95U")] <- NA
    AttData[nn & ag, c(paste0("cEd", v, "_", s), paste0("cEd", v, "_", s, "_95L"), paste0("cEd", v,"_", s, "_95U"))] <-
      cCI(AttData[nn & ag, c(paste0("cEB_", s), paste0("cVB_", s), paste0("cE",v,"_", s), paste0("cV",v,"_",s))])
    for (v in c("B","ET")) {
      AttData[nn & ag ,paste0("cE", v, "_", s)] <- NA
      AttData[nn & ag ,paste0("cV", v, "_", s)] <- NA
      for (sy in sort(unique(AttData[,s]))) {
        AttData[(AttData[,s]==sy) & nn & ag, paste0("cE", v, "_", s)] <-
          cummulate(AttData[(AttData[,s]==sy) & nn & ag, c(paste0("E",v),"EdIA")], FALSE)
        AttData[(AttData[,s]==sy) & nn & ag, paste0("cV", v, "_", s)] <-
          cummulate(AttData[(AttData[,s]==sy) & nn & ag, c(paste0("V",v),"EdIA")],  FALSE)
      }
    }
    AttData[nn & ag ,paste0("cEd", v, "_", s)] <- NA
    AttData[nn & ag ,paste0("cEd", v, "_", s, "_95L")] <- NA
    AttData[nn & ag ,paste0("cEd", v, "_", s, "_95U")] <- NA
    AttData[nn & ag ,c(paste0("cEd", v, "_", s), paste0("cEd", v, "_", s, "_95L"), paste0("cEd", v, "_", s, "_95U"))] <-
      cCI(AttData[nn & ag , c(paste0("cEB_", s), paste0("cVB_", s), paste0("cE",v,"_", s), paste0("cV",v,"_",s))])
  }
}

AttData$country <- country
AttData$IArestricted <- as.integer(IArest)
AttData <- AttData[, c("country", "IArestricted", "agegrp", "year", "week", "deaths", "Vdeaths", "N", "IA", "ET",
                       "EB", "EB_95L", "EB_95U", "VB",
                       "EIA", "VIA", "EET", "VET",
                       "EdIA", "EdIA_95L", "EdIA_95U",
                       "EdET", "EdET_95L", "EdET_95U",
                       "cexcess_year", "cuexcess_year",
                       "cEdIA_year", "cEdIA_year_95L", "cEdIA_year_95U",
                       "cEdET_year", "cEdET_year_95L", "cEdET_year_95U",
                       "summer", "cexcess_summer", "cuexcess_summer",
                       "cEdIA_summer", "cEdIA_summer_95L", "cEdIA_summer_95U",
                       "cEdET_summer", "cEdET_summer_95L", "cEdET_summer_95U",
                       "winter", "cexcess_winter", "cuexcess_winter",
                       "cEdIA_winter", "cEdIA_winter_95L", "cEdIA_winter_95U",
                       "cEdET_winter", "cEdET_winter_95L", "cEdET_winter_95U")]

if (IArest) {
  write.table(AttData, file=paste0(outdir,"/",country,"_output_v4_IArestricted.txt"),
              row.names = FALSE, quote = FALSE, sep =";", dec=".", na="")
} else {
  write.table(AttData, file=paste0(outdir,"/",country,"_output_v4.txt"),
              row.names = FALSE, quote = FALSE, sep =";", dec=".", na="")
}

rm(a, s, sy, v, nn, ag, cummulate, cCI, AttData)
### END: Post estimation ###
