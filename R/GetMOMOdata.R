#' Convert an A-MOMO complete file to an AttMOMO input file
#'
#' @param indir Full path to the directory where A-MOMO complete file is placed.
#' @param filename Name of A-MOMO complete file incl. extension (.txt)
#' @param outdir Directory where the AttMOMO input file should be placed (default indir)
#' @param outfile Should output be written an AttMOMO deaths input ;-separated file, death_data.txt, in outdir (default TRUE)
#' @import data.table
#' @return AttMOMO input file: group, ISOweek, deaths
#' @export
GetMOMOdata <- function(indir, filename, outdir = NULL, outfile = TRUE) {
  group <- YoDi <- WoDi <- nbc <- . <- nb <- NULL
  # library(data.table)

  # indir <- 'H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/DK-MOMO/MOMOv4-3-Denmark-2020-22/EUROMOMO-COMPLETE-Denmark-2020-22'
  # filename <- 'EUROMOMOv4-3-COMPLETE-Denmark-2020-22.txt'
  # outdir = "H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/AttMOMO/Denmark/data"
  # outfile = TRUE

  if (is.null(outdir)) { outdir <- indir}
  deaths <- try(setDT(read.table(paste0(indir,"/", filename), sep = ",", dec = ".", header = TRUE)[ ,c("group", "YoDi", "WoDi", "nb", "nbc")]))
  if (inherits(deaths, "try-error")) {
    stop(paste0("Error: ", indir, "/", filename))
  }
  deaths <- deaths[!is.na(group) & !is.na(YoDi) & !is.na(WoDi) & !is.na(nbc), .(deaths = max(nb, nbc)),
                   keyby = .(group, ISOweek = paste0(YoDi, "-W", formatC(WoDi, width=2, flag="0")))]
  deaths <- deaths[order(group, ISOweek),]

  if (file) {
    write.table(deaths, file = paste0(outdir, "/death_data.txt"), sep = ";", col.names = TRUE, row.names = FALSE)
  } else {
    return(deaths)
  }
}


# indir = paste0("H:/SFSD/INFEPI/Projekter/AKTIVE/MOMO/DK-MOMO/MOMOv4-3-Denmark-", gsub('W', '', EndWeek),"/EUROMOMO-COMPLETE-Denmark-", gsub('W', '', EndWeek))
# filename = paste0("EUROMOMOv4-3-COMPLETE-Denmark-", gsub('W', '', EndWeek), ".txt")

# anova,
# df.residuals,
# glm,
# median,
# predict,
# predict.glm,
# quasipoisson,
# residuals,
# read.table,
# write.table
