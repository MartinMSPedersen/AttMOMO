#' Get date of birth (DoB) from Danish person number (cprnr)
#'
#' @param cprnr.
#' @return as.Date(Dob)
#' @export
GetDoB <- function(cprnr) {
  m <- as.numeric(substr(cprnr, 3, 4))
  d <- as.numeric(substr(cprnr, 1, 2))
  y <- (1900 + as.numeric(substr(cprnr, 5, 6))
        + 100 * as.numeric(grepl("[4-9]", substr(cprnr, 7, 7)) & (as.numeric(substr(cprnr, 5, 6)) <= 36))
        - 100 * as.numeric(grepl("[5-8]", substr(cprnr, 7, 7)) & (as.numeric(substr(cprnr, 5, 6)) >= 58)))
  DoB <- as.Date(ISOdate(y, m , d))
  return(DoB)
}
