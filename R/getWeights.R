#' @title getAgeWeights
#' 
#' @description  Get age weights from all residents in DSA.
#' 
#' @param Args requires Args, see \code{\link{setArgs}}.
#' 
#' @return 
#'
#' @export 

getAgeWeights <- function(Args) {
  dat <- getDemResident(Args)
  tapply(dat$Age, dat$Year, mean)
}
