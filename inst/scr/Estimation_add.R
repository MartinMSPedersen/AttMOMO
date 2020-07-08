#' Read input data, create output directories and return estimates.
#'
#' @param country Country name.
#' @param wdir Working directory.
#' @param StartWeek ISOweek format
#' @param EndWeek ISOweek format
#' @param groups list of group names
#' @param pooled list of group names to be pooled (default = NULL)
#' Must be part of groups.
#' @param indicators list if indicator variables.
#' One file for each must be available: IndicatorName_data.txt
#' @param population Is a  ;-separated population,txt file avaiable (TRUE/FALSE - default FALSE).
#' If, estimation will be with population as exposure.
#' @param lags weeks of laged effect (default = 2, max = 9)
#' @param ptrend significance of trend to be included (default = 0.05)
#' @param p26 significance of halfyear-sine be included (default = 0.05)
#' @param p52 significance of year-sine be included (default = 0.10)
#' @param Rdata Return data (TRUE/FALSE - default FALSE).
#' @import data.table
#' @import glm2
#' @import rsq
#' @return data and write a ;-separated file AttData_indicators.txt
#' @export
estimation <- function(country, wdir, StartWeek, EndWeek, groups, pooled = NULL, indicators, population = FALSE,
                       lags = 2, ptrend = 0.05, p26 = 0.05, p52 = 0.10, Rdata = FALSE) {
  library(data.table)
  library(glm2)

  # country <- "Denmark"
  # wdir <- "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark"
  # StartWeek <- '2015-W27'
  # EndWeek <- '2020-W22'
  # groups = c('00to14', '15to44', '45to64', '65to74', '75to84', '85P', 'Total')
  # pooled <- c('00to14', '15to44', '45to64', '65to74', '75to84', '85P')
  # indicators <- c('GSIPLS', 'GSCLS')
  # population <- TRUE
  # lags <- 3
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
    stop(paste0("In ", indir, "/deaths_data.txt"))
  }
  for (g in groups) {
    if (!(g %in% unique(AttData$group))) {
      stop(paste("group", g, "not in deaths_data.txt"))
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
    rm(pop_data)
  } else {
    AttData$N <- 1
  }

  # Prepare data ------------------------------------------------------------
  AttData[, `:=`(season = as.numeric(substr(ISOweek,1,4)) - (as.numeric(substr(ISOweek,7,8)) < 27),
                 summer = as.numeric((21 <= as.numeric(substr(ISOweek,7,8))) & (as.numeric(substr(ISOweek,7,8)) <= 39)),
                 winter = as.numeric((20 >= as.numeric(substr(ISOweek,7,8))) | (as.numeric(substr(ISOweek,7,8)) >= 40)))]
  # Warm/cold summer/winter
  AttData[, `:=`(cold_summer = -((ET < 0) * ET) * summer,
                 warm_summer = ((ET > 0) * ET) * summer,
                 cold_winter = -((ET < 0) * ET) * winter,
                 warm_winter = ((ET > 0) * ET) * winter)]
  AttData[, `:=`(summer = NULL, winter = NULL)]
  # lags
  for (i in c(indicators, c('cold_summer', 'warm_summer', 'cold_winter', 'warm_winter'))) {
    for (l in 0:lags) {
      expr <- parse(text = paste0(i, "_d", l, " := shift(", i, ", ", l, ", type = 'lag')"))
      AttData[, eval(expr), by = group]
    }
  }
  # indicator by season
  for (i in indicators) {
    for (l in 0:lags) {
      for (s in unique(AttData$season)) {
        expr <- parse(text = paste0(i, "_d", l, "_", s, " := ifelse(", s, " == season, ", i, "_d", l, ", 0)"))
        AttData[, eval(expr), by = group]
      }
      expr <- parse(text = paste0(i, "_d", l, " := NULL"))
      AttData[, eval(expr)]
    }
  }
  AttData[, `:=`(cold_summer= NULL, warm_summer = NULL, cold_winter = NULL, warm_winter = NULL)]
  AttData[is.na(AttData)] <- 0
  # baseline
  AttData[, wk := as.numeric(as.factor(ISOweek))]
  AttData[, `:=`(const = 1,
                 sin52 = sin((2*pi/(365.25/7)) * wk),
                 cos52 = cos((2*pi/(365.25/7)) * wk),
                 sin26 = sin((4*pi/(365.25/7)) * wk),
                 cos26 = cos((4*pi/(365.25/7)) * wk))]

  # Prediction data ---------------------------------------------------------
  # baseline
  AttData.B <- copy(AttData)
  for (l in 0:lags) {
    for (i in c('cold_summer', 'warm_summer', 'cold_winter', 'warm_winter')) {
      expr <- parse(text = paste0(i, "_d", l, ":= 0"))
      AttData.B[, eval(expr)]
    }
    for (i in indicators) {
      for (s in unique(AttData$season)) {
        expr <- parse(text = paste0(i, "_d", l, "_", s, " := 0"))
        AttData.B[, eval(expr)]
      }
    }
  }
  # ET
  AttData.ET <- copy(AttData)
  AttData.ET[, `:=`(const = 0, wk = 0, sin52 = 0, cos52 = 0, sin26 = 0, cos26 = 0)]
  for (i in indicators) {
    for (l in 0:lags) {
      for (s in unique(AttData$season)) {
        expr <- parse(text = paste0(i, "_d", l, "_", s, " := 0"))
        AttData.ET[, eval(expr)]
      }
    }
  }
  # indicators
  for (i in indicators) {
    X <- copy(AttData)
    X[, `:=`(const = 0, wk = 0, sin52 = 0, cos52 = 0, sin26 = 0, cos26 = 0)]
    for (l in 0:lags) {
      for (ir in c('cold_summer', 'warm_summer', 'cold_winter', 'warm_winter')) {
        expr <- parse(text = paste0(ir, "_d", l, ":= 0"))
        X[, eval(expr)]
      }
      for (ir in indicators[indicators != i]) {
        for (s in unique(AttData$season)) {
          expr <- parse(text = paste0(ir, "_d", l, "_", s, " := 0"))
          X[, eval(expr)]
        }
      }
    }
    assign(paste0("AttData.", i), X)
  }
  rm(X)

  # Estimation --------------------------------------------------------------
  # V <- vcov(m) # covariance matrix
  # non-baseline parameters
  parm <- paste(grep("_d[0-9]", names(AttData), value=TRUE), collapse = " + ")

  for (g in groups) {
    print(paste("### Group", g, "###"))

    f <- paste(c("deaths ~ -1 + const + wk", "sin52 + cos52", "sin26 + cos26", parm), collapse = " + ")
    m <- try(glm(f, quasipoisson(identity), data = AttData[group == g,]), silent = TRUE)
    if ((!inherits(m, "try-error")) & (median(AttData[group == g,]$deaths) > 0)) {
      if (m$converged) {
        fa <- paste(c("deaths ~ -1 + const + wk", parm), collapse = " + ")
        ma <- glm(fa, quasipoisson(identity), data = AttData[group == g,])
        if (anova(m, ma, dispersion = max(1, sum(residuals(m, type = "deviance")^2)/df.residual(m)), test="LRT")$`Pr(>Chi)`[2] > max(p52, p26)) {
          m <- ma
        } else {
          fa <- paste(c("deaths ~ -1 + const + wk", "sin52 + cos52", parm), collapse = " + ")
          ma <- glm(fa, quasipoisson(identity), data = AttData[group == g,])
          if (anova(m, ma, dispersion = max(1, sum(residuals(m, type = "deviance")^2)/df.residual(m)), test="LRT")$`Pr(>Chi)`[2] > p26) {
            m <- ma
          } else {
            fa <- paste(c("deaths ~ -1 + const + wk", parm), collapse = " + ")
            ma <- glm(fa, quasipoisson(identity), data = AttData[group == g,])
            if (anova(m, ma, dispersion = max(1, sum(residuals(m, type = "deviance")^2)/df.residual(m)), test="LRT")$`Pr(>Chi)`[2] > p52) {
              m <- ma
            }
          }
        }
      } else {
        f <- paste(c("deaths ~ -1 + const + wk", parm), collapse = " + ")
        m <- try(glm(f, quasipoisson(identity), data = AttData[group == g,]), silent = TRUE)
        if ((inherits(m, "try-error")) | (!m$converged)) {
          print(paste("The model did not converge. A Simple model with only trend used i.e. no effect of indicators"))
          f <- paste(c("deaths ~ -1 + const + wk"))
          m <- glm(f, quasipoisson(identity), data = AttData[group == g,])
        }
      }
    } else {
      if (median(AttData[group == g,]$deaths) == 0) {msg <- "Zero inflated." } else {msg <- NULL}
      if (inherits(m, "try-error")) print(paste("Could not fit model.", msg, "Simple model with only trend used i.e. no effect of indicators"))
      f <- paste(c("deaths ~ -1 + const + wk"))
      m <- glm(f, quasipoisson(identity), data = AttData[group == g,])
    }

    # Remove NA colinearity
    f <- paste("deaths ~ -1 +", paste(names(m$coefficients[!is.na(m$coefficients)]), collapse = ' + '))

    # Get R2 - don't work in function
    # X <- data.frame(copy(AttData[group == g,]))
    # m <- glm(f, quasipoisson(identity), data = X)
    # R2 <- rsq::rsq(m, adj = FALSE, type = 'v')
    # Adj.R2 <- rsq::rsq(m, adj = TRUE, type = 'v')

    m <- glm(f, quasipoisson(identity), data = AttData[group == g,])

    print(summary(m, dispersion = max(1, sum(residuals(m, type = "deviance")^2)/df.residual(m))))
    # print(paste('R2 =', R2, ' Adjusted R2 =', Adj.R2))
    # rm(X, R2, Adj.R2)

    # Predictions -------------------------------------------------------------

    # Prediction baseline
    AttData[group == g, `:=`(EB = predict.glm(m, newdata = AttData.B[group == g,], se.fit=TRUE)$fit)]
    AttData[group == g, `:=`(VEB = (max(1, sum(residuals(m)^2)/df.residual(m)))*EB +
                               predict.glm(m, newdata = AttData.B[group == g,], dispersion = max(1, sum(residuals(m)^2)/df.residual(m)), se.fit=TRUE)$se.fit^2)]
    #  Prediction ET
    AttData[group == g, `:=`(EET = predict.glm(m, newdata = AttData.ET[group == g,], se.fit=TRUE)$fit,
                             VEET = predict.glm(m, newdata = AttData.ET[group == g,], dispersion = max(1, sum(residuals(m)^2)/df.residual(m)), se.fit=TRUE)$se.fit^2)]

    # Predictions indicators
    for (i in indicators) {
      expr <- parse(text = paste0("`:=`(E", i, " = predict.glm(m, newdata = AttData.", i, "[group == g,], se.fit=TRUE)$fit,
      VE", i, " = predict.glm(m, newdata = AttData.", i, "[group == g,], dispersion = max(1, sum(residuals(m)^2)/df.residual(m)), se.fit=TRUE)$se.fit^2)"))
      AttData[group == g, eval(expr)]
    }

    # Adjusted baseline
    X <- copy(AttData)
    for (i in indicators) {
      for (l in 0:lags) {
        for (s in unique(AttData$season)) {
          expr <- parse(text = paste0(i, "_d", l, "_", s, " := ifelse(E", i," >= 0, 0,", i, "_d", l, "_", s,")"))
          X[, eval(expr)]
        }
      }
    }
    AttData[group == g, `:=`(EAB = predict.glm(m, newdata = X[group == g,], se.fit=TRUE)$fit)]
    AttData[group == g, `:=`(VEAB = (max(1, sum(residuals(m)^2)/df.residual(m)))*EAB +
                               predict.glm(m, newdata = X[group == g,], dispersion = max(1, sum(residuals(m)^2)/df.residual(m)), se.fit=TRUE)$se.fit^2)]

    # Adjusted predictions indicators
    for (i in indicators) {
      X <- copy(AttData)
      X[, `:=`(const = 0, wk = 0, sin52 = 0, cos52 = 0, sin26 = 0, cos26 = 0)]
      for (l in 0:lags) {
        for (ir in c('cold_summer', 'warm_summer', 'cold_winter', 'warm_winter')) {
          expr <- parse(text = paste0(ir, "_d", l, ":= 0"))
          X[, eval(expr)]
        }
        for (l in 0:lags) {
          for (s in unique(AttData$season)) {
            for (ir in indicators[indicators != i]) {
              expr <- parse(text = paste0(ir, "_d", l, "_", s, " := 0"))
              X[, eval(expr)]
            }
            expr <- parse(text = paste0(i, "_d", l, "_", s, " := ifelse(E", i," < 0, 0,", i, "_d", l, "_", s,")"))
            X[, eval(expr)]
          }
        }
      }
      expr <- parse(text = paste0("`:=`(EA", i, " = predict.glm(m, newdata = X[group == g,], se.fit=TRUE)$fit,
                                  VEA", i, " = predict.glm(m, newdata = X[group == g,], dispersion = max(1, sum(residuals(m)^2)/df.residual(m)), se.fit=TRUE)$se.fit^2)"))
      AttData[group == g, eval(expr)]
    }
    rm(X)

  }

  # Clean up
  AttData[, `:=`(const = NULL, wk = NULL, sin52 = NULL, cos52 = NULL, sin26 = NULL, cos26 = NULL)]
  for (l in 0:lags) {
    for (i in c('cold_summer', 'warm_summer', 'cold_winter', 'warm_winter')) {
      expr <- parse(text = paste0(i, "_d", l, " := NULL"))
      AttData[, eval(expr)]
    }
    for (i in indicators) {
      for (s in unique(AttData$season)) {
        expr <- parse(text = paste0(i, "_d", l, "_", s, " := NULL"))
        AttData[, eval(expr)]
      }
    }
  }
  AttData[, season := NULL]
  rm(f, fa, expr, parm, g, i, l, m, ma, AttData.B, AttData.ET)

  # Pooled total ------------------------------------------------------------
  if (!is.null(pooled)) {
    pooledData <- AttData[group %in% pooled,
                          .(group = 'TotalPooled',
                            deaths = sum(deaths, na.rm = TRUE),
                            ET = sum(ET*N/sum(N), na.rm = TRUE),
                            N = sum(N, na.rm = TRUE),
                            EB = sum(EB, na.rm = TRUE),
                            VEB = sum(VEB, na.rm = TRUE),
                            EET = sum(EET, na.rm = TRUE),
                            VEET = sum(VEET, na.rm = TRUE),
                            EAB = sum(EAB, na.rm = TRUE),
                            VEAB = sum(VEAB, na.rm = TRUE)
                          ), keyby = ISOweek]

    for (i in indicators) {
      expr <- parse(text = paste0(".(", i, " = sum(", i, "*N/sum(N), na.rm = TRUE),
                                  E", i, " = sum(E", i, ", na.rm = TRUE),
                                  VE", i, " = sum(VE", i, ", na.rm = TRUE),
                                  EA", i, " = sum(EA", i, ", na.rm = TRUE),
                                  VEA", i, " = sum(VEA", i, ", na.rm = TRUE))"))
      pooledData <- merge(pooledData, AttData[group %in% pooled, eval(expr), keyby = ISOweek], by = "ISOweek", all.x = TRUE)
    }
    AttData <- rbind(AttData, pooledData)
  }

  AttData <- cbind(country, AttData)
  AttData <- AttData[order(country, group, ISOweek)]

  write.table(AttData, file = paste0(outdir, "/AttData_", paste(indicators, collapse = '_'), ".txt"), sep = ";", row.names = FALSE, col.names = TRUE)

  if (Rdata) return(AttData)
}

# # Residual autocorellation and heteroskedasticity
# g <- 'Total'
# parm <- paste(grep("_d[0-9]", names(AttData), value=TRUE), collapse = " + ")
# f <- paste(c("deaths ~ 1 + wk", "sin52 + cos52", "sin26 + cos26", parm), collapse = " + ")
# m<- glm(f, quasipoisson(identity), data = AttData[group == g,])
# summary(m)
# res <- data.table(res = m$residuals)
# res$n <- 1:nrow(res)
# X <- acf(res$res)
# mod2 = lm(res ~ n, data = res)
# summary(mod2)
# plot(res$res ~ res$n)
