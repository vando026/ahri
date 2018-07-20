#' @title getDeaths
#' 
#' @description Gets mortality data from Demography dataset.
#' 
#' @param dat dataset from \code{\link{splitData}}. 
#' 
#' @return data.frame
#'
#' @import dplyr
#'
#' @export
#' 

getDeaths <- function(inFile=NULL, dat) {
  if (!is.null(inFile)) {
  dat <- read_tsv(inFile, 
    col_types=cols_only(
      BSIntID="i",
      IIntID="i",
      # ObservationStart="T",
      # ObservationEnd="T",
      Sex="i",
      ExpYear="i",
      # ExpDays="i",
      # Area="i",
      Died="i"))
  }
  Total <- tapply(dat$IIntID, dat$ExpYear, 
    function(x) length(unique(x)))
  Died <- tapply(dat$Died, dat$ExpYear, sum)
  dat <- cbind(Died, Total)
  dat
}
