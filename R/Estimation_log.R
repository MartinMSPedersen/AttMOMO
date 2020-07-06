#' Read input data, create output directories and return estimates.
#'
#' @param country Country name.
#' @param wdir Working directory.
#' @param StartWeek ISOweek format
#' @param EndWeek ISOweek format
#' @param groups list if group names
#' @param indicators list if indicator variables.
#' One file for each must be available: IndicatorName_data.txt
#' @param restrict Restrict to only positive effects (TRUE/FALSE - default TRUE).
#' One file for each must be available: IndicatorName.txt
#' @param population Is a  ;-separated population,txt file avaiable (TRUE/FALSE - default FALSE).
#' If, estimation will be witk population as exposure.
#' @param IArest Restrict effect of IA to positive (TRUE/FALSE - default TRUE)
#' @param lags weeks of laged effect (default = 2, max = 9)
#' @param ptrend significance of trend to be included (default = 0.05)
#' @param p26 significance of halfyear-sine be included (default = 0.05)
#' @param p52 significance of year-sine be included (default = 0.10)
#' @param Rdata Return data (TRUE/FALSE - default FALSE).
#' @import data.table
#' @return data and write a ;-separated file AttData_indicators.txt
#' @export
estimation <- function(country, wdir, StartWeek, EndWeek, groups, indicators, restrict = TRUE, population = FALSE,
                       IArest = TRUE, lags = 2, ptrend = 0.05, p26 = 0.05, p52 = 0.10, Rdata = FALSE) {
  library(data.table)

  # country <- "Denmark"
  # wdir <- "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark"
  # StartWeek <- '2015-W27'
  # EndWeek <- '2020-W16'
  # groups <- c('00to04', '05to14', '15to64', '65P', '65to74', '75to84', '85P', 'Total')
  # indicators <- c('GSInfl', 'GSCOVID19')
  # restrict <- TRUE
  # population <- TRUE
  # lags <- 2
  # ptrend <- 0.05
  # p26 <- 0.05
  # p52 <- 0.10
  # Rdata <- TRUE


  # Directory setup ---------------------------------------------------------
  ### Create general output dir
  if (!dir.exists(paste0(wdir, "/AttMOMO_", EndWeek))) { dir.create(paste0(wdir, "/AttMOMO_", EndWeek)) }
  ### Copy data directory - directory where input data are stored
  indir <- paste0(wdir, "/AttMOMO_", EndWeek, "/data")
  if (!dir.exists(indir)) dir.create(indir)
  file.copy(from = list.files(paste0(wdir,"/data"), all.files = TRUE, full.names = TRUE, no.. = TRUE),
            to = indir, overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)
  #file.remove(list.files(paste0(wdir,"/data"), all.files = TRUE, full.names = TRUE, no.. = TRUE))

  ### Create output directory - directory where output are created
  outdir <- paste0(wdir,"/AttMOMO_", EndWeek, "/output")
  if (!dir.exists(outdir)) dir.create(outdir)

  # Read and merge data -----------------------------------------------------
  # death data
  AttData <- try(setDT(read.table(paste0(indir, "/Death_data.txt"), sep=";", dec=".", header = TRUE, as.is = TRUE)[,c("group", "ISOweek", "deaths")]))
  if (inherits(AttData, "try-error")) {
    stop(paste0("In ", indir, "/deaths.txt"))
  }
  for (g in groups) {
    if (!(g %in% unique(AttData$group))) {
      stop(paste("group", g, "not in deaths.txt"))
    }
    if ((min(AttData[g == group,]$ISOweek) > StartWeek) | (max(AttData[g == group,]$ISOweek) < EndWeek)) {
      stop(paste("deaths.txt", g, "do not cover", StartWeek, "to", EndWeek))
    }
  }
  AttData <- AttData[(StartWeek <= ISOweek) & (ISOweek <= EndWeek) & (group %in% groups),]
  AttData <- AttData[order(group, ISOweek),]

  # Extreme temperature data
  ET <- try(setDT(read.table(paste0(indir,"/ET_data.txt"), header = TRUE, sep = ";", dec = ".", as.is =  TRUE))[, c("ISOweek", "ET")])
  if (inherits(ET,"try-error")) {
    stop(paste0("Error: ",indir,"/ET_data.txt"))
  }
  if ((min(ET$ISOweek) > StartWeek) | (max(ET$ISOweek) < EndWeek)) {
    stop(paste("ET_data.txt do not cover", StartWeek, "to", EndWeek))
  }
  AttData <- merge(AttData, ET[order(ISOweek), ], by = "ISOweek")
  rm(ET)

  # Indicator data
  for (i in indicators) {
    X <- try(setDT(read.table(paste0(indir,"/", i, "_data.txt"), header = TRUE, sep = ";", dec = ".", as.is =  TRUE)[, c("group", "ISOweek", i)]))
    if (inherits(X, "try-error")) {
      stop(paste0("Error: ",indir,"/",i, "_data.txt"))
    }
    for (g in groups) {
      if (!(g %in% unique(X$group))) {
        stop(paste0("group ", g, " not in ", i, "_data.txt"))
      }
      if ((min(X[g == group,]$ISOweek) > StartWeek) | (max(X[g == group,]$ISOweek) < EndWeek)) {
        stop(paste0(i, "_data.txt ", g, " do not cover ", StartWeek, " to ", EndWeek))
      }
    }
    AttData <- merge(AttData, X[order(group, ISOweek),], by = c("group", "ISOweek"))
    rm(X)
  }

  # Population data ---------------------------------------------------------
  if (population) {
    pop_data <- try(read.table(paste0(indir,"/Population_data.txt"), sep=";", dec=".", header=TRUE)[,c("group", "ISOweek", "N")])
    if (inherits(pop_data,"try-error")) {
      stop(paste0("Error: ",indir,"/population.txt"))
    }
    AttData <- merge(AttData, pop_data, by = c("group", "ISOweek"))
  } else {
    AttData$N <- 1
  }

  # Prepare data ------------------------------------------------------------
  AttData[, `:=`(wk = as.numeric(as.factor(ISOweek)),
                 season = as.numeric(substr(ISOweek,1,4)) - (as.numeric(substr(ISOweek,7,8)) < 27),
                 summer = as.numeric((21 <= as.numeric(substr(ISOweek,7,8))) & (as.numeric(substr(ISOweek,7,8)) <= 39)),
                 winter = as.numeric((20 >= as.numeric(substr(ISOweek,7,8))) | (as.numeric(substr(ISOweek,7,8)) >= 40)))]
  AttData[, `:=`(sin52 = sin((2*pi/(365.25/7)) * wk),
                 cos52 = cos((2*pi/(365.25/7)) * wk),
                 sin26 = sin((4*pi/(365.25/7)) * wk),
                 cos26 = cos((4*pi/(365.25/7)) * wk),
                 cold_summer = -((ET < 0) * ET) * summer,
                 warm_summer = ((ET > 0) * ET) * summer,
                 cold_winter = -((ET < 0) * ET) * winter,
                 warm_winter = ((ET > 0) * ET) * winter)]
  AttData[, `:=`(summer = NULL, winter = NULL)]
  # lags
  for (i in c(indicators, c('cold_summer', 'warm_summer', 'cold_winter', 'warm_winter'))) {
    for (l in 0:lags) {
      expr <- parse(text = paste0(i, "_d", l, ":= shift(", i, ", ", l, ", type = 'lag')"))
      AttData[, eval(expr), by = group]
    }
  }
  AttData[is.na(AttData)] <- 0

  # Estimation data ---------------------------------------------------------
  AttData.B <- copy(AttData)
  for (i in c(indicators, c('cold_summer', 'warm_summer', 'cold_winter', 'warm_winter'))) {
    for (l in 0:lags) {
      expr <- parse(text = paste0(i, "_d", l, ":= 0"))
      AttData.B[, eval(expr)]
    }
  }
  AttData.ET <- copy(AttData)
  for (i in indicators) {
    for (l in 0:lags) {
      expr <- parse(text = paste0(i, "_d", l, ":= 0"))
      AttData.ET[, eval(expr)]
    }
  }
  for (i in indicators) {
    X <- copy(AttData)
    for (ir in c(indicators[indicators != i], 'cold_summer', 'warm_summer', 'cold_winter', 'warm_winter')) {
      for (l in 0:lags) {
        expr <- parse(text = paste0(ir, "_d", l, ":= 0"))
        X[, eval(expr)]
      }
    }
    assign(paste0("AttData.", i), X)
  }
  rm(X)

  # Estimation --------------------------------------------------------------
  # Parameters
  parm <- paste(c(grep("(winter|summer)(_d[0-9])", names(AttData), value=TRUE),
                  paste0("(", paste(grep("winter|summer", grep("_d[0-9]", names(AttData), value=TRUE), invert = TRUE, value = TRUE), collapse = " + "), "):factor(season)")),
                collapse = " + ")

  for (g in groups) {
    print(paste("### Group", g, "###"))

    f <- paste(c("deaths ~ wk", "sin52 + cos52", "sin26 + cos26", parm), collapse = " + ")
    m <- try(glm(f, quasipoisson, offset = log(N), data = AttData[group == g,]))

    if ((!inherits(m, "try-error")) & (median(AttData[group == g,]$deaths) > 0)) {
      if (m$converged) {
        fa <- paste(c("deaths ~ wk", parm), collapse = " + ")
        ma <- glm(fa, quasipoisson, offset = log(N), data = AttData[group == g,])
        if (anova(m, ma, dispersion = max(1, sum(residuals(m, type = "deviance")^2)/df.residual(m)), test="LRT")$`Pr(>Chi)`[2] > max(p52, p26)) {
          m <- ma
        } else {
          fa <- paste(c("deaths ~ wk", "sin52 + cos52", parm), collapse = " + ")
          ma <- glm(fa, quasipoisson, offset = log(N), data = AttData[group == g,])
          if (anova(m, ma, dispersion = max(1, sum(residuals(m, type = "deviance")^2)/df.residual(m)), test="LRT")$`Pr(>Chi)`[2]>p26) {
            m <- ma
          } else {
            fa <- paste(c("deaths ~ wk", parm), collapse = " + ")
            ma <- glm(fa, quasipoisson, offset = log(N), data = AttData[group == g,])
            if (anova(m, ma, dispersion = max(1, sum(residuals(m, type = "deviance")^2)/df.residual(m)), test="LRT")$`Pr(>Chi)`[2]>p52) {
              m <- ma
            }
          }
        }
      } else {
        f <- paste(c("deaths ~ wk", parm), collapse = " + ")
        m <- try(glm(f, quasipoisson, offset = log(N), data = AttData[group == g,]))
        if ((inherits(m, "try-error")) | (!m$converged)) {
          warning(paste("Group", g, "did not converge. A Simple model with only trend used i.e. no effect of indicators"))
          f <- paste(c("deaths ~ wk"))
          m <- glm(f, quasipoisson, offset = log(N), data = AttData[group == g,])
        }
      }
    } else {
      if (median(AttData[group == g,]$deaths) == 0) {msg <- "Zero inflated." } else {msg <- NULL}
      if (inherits(m, "try-error")) warning(paste("Group", g, "Could not fit model.", msg, "Simple model with only trend used i.e. no effect of indicators"))
      f <- paste(c("deaths ~ wk"))
      m <- glm(f, quasipoisson, offset = log(N), data = AttData[group == g,])
    }
    print(summary(m, dispersion = max(1, sum(residuals(m, type = "deviance")^2)/df.residual(m))))

    # Full model
    AttData[group == g, `:=`(plogFM = predict.glm(m, newdata = AttData[group == g,], se.fit=TRUE)$fit,
                             VlogFM = predict.glm(m, newdata = AttData[group == g,], dispersion = max(1, sum(residuals(m)^2)/df.residual(m)), se.fit=TRUE)$se.fit^2)]
    # Baseline
    AttData[group == g, `:=`(plogB = predict.glm(m, newdata = AttData.B[group == g,], se.fit=TRUE)$fit,
                             VlogB = predict.glm(m, newdata = AttData.B[group == g,], dispersion = max(1, sum(residuals(m)^2)/df.residual(m)), se.fit=TRUE)$se.fit^2)]
    AttData[group == g, `:=`(EB = exp(plogB), VEB = (max(1, sum(residuals(m)^2)/df.residual(m)))*exp(plogB) + exp(2*plogB)*VlogB)]
    AttData[group == g, `:=`(VlogEB = VEB/(EB^2))]
    # ET
    AttData[group == g, `:=`(plogET = predict.glm(m, newdata = AttData.ET[group == g,], se.fit=TRUE)$fit,
                             VlogET = predict.glm(m, newdata = AttData.ET[group == g,], dispersion = max(1, sum(residuals(m)^2)/df.residual(m)), se.fit=TRUE)$se.fit^2)]
    # Indicators
    for (i in indicators) {
      expr <- parse(text = paste0("`:=`(plog", i, " = predict.glm(m, newdata = AttData.", i, "[group == g,], se.fit=TRUE)$fit,
      Vlog", i, " = predict.glm(m, newdata = AttData.", i, "[group == g,], dispersion = max(1, sum(residuals(m)^2)/df.residual(m)), se.fit=TRUE)$se.fit^2)"))
      AttData[group == g, eval(expr)]
    }
  }

  # Clean up
  AttData[, `:=`(wk = NULL, sin52 = NULL, cos52 = NULL, sin26 = NULL, cos26 = NULL, season = NULL,
                 cold_summer= NULL, warm_summer = NULL, cold_winter = NULL, warm_winter = NULL)]
  for (i in c(indicators, c('cold_summer', 'warm_summer', 'cold_winter', 'warm_winter'))) {
    for (l in 0:lags) {
      expr <- parse(text = paste0(i, "_d", l, ":= NULL"))
      AttData[, eval(expr)]
    }
  }
  rm(f, fa, expr, parm, g, i, l, m, ma, AttData.B, AttData.ET)

  # Post estimation ---------------------------------------------------------
  AttData[, `:=`(EET = (exp(plogET - plogB) - 1)*EB, VlogEET = (VlogET + VlogB + VlogEB))]
  for (i in indicators) {
    if (restrict) {
      expr <- parse(text = paste0("`:=`(E", i, " = pmax(0, (exp(plog", i, " - plogB) - 1)*EB), VlogE", i, " = (Vlog", i, " + VlogB + VlogEB))"))
    } else {
      expr <- parse(text = paste0("`:=`(E", i, " = (exp(plog", i, " - plogB) - 1)*EB, VlogE", i, " = (Vlog", i, " + VlogB + VlogEB))"))
    }
    AttData[, eval(expr)]
  }
  AttData <- cbind(country, AttData)
  AttData <- AttData[order(country, group, ISOweek)]

  write.table(AttData, file = paste0(outdir, "/AttData_", paste(indicators, collapse = '_'), ".txt"), sep = ";", row.names = FALSE, col.names = TRUE)

  if (Rdata) return(AttData)
}
