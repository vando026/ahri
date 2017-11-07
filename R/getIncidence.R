#' @title getIncidence
#' 
#' @description Calculates the crude and adjusted incidence.
#' 
#' @param Args takes list from \code{\link{setArgs}}.
#'
#' @return data.frame
#'
#' @import dplyr

getIncidence <- function(Args) {

  hiv   <- getHIV(Args)
  rtdat <- getRTData(hiv)
  sdat  <- imputeRandomPoint(rtdat)
  wdat <- getWeightsKZN(Args)

  dat <- lapply(seq(Args$nSimulations),
    function(i) doIncData(rtdat, sdat, Args, i))

  Year <- lapply(dat, 
    function(x) calcInc(x, wdat, calcBy="Year"))

  Age <- lapply(dat, 
    function(x) calcInc(x, wdat, calcBy="AgeCat"))
  
  if (Args$nSimulations==1) 
    return(list(Year, Age))

  est_year <- getRate(Year)
  est_age <- getRate(Age)
  lapply(c(est_year, est_age), sumEst)
}

getRate <- function(dat) {
  nm  =c("crude.rate", "adj.rate")
  out <- lapply(nm, function(x) {
    out <- lapply(dat, `[`, x)
    do.call('cbind', out)})
  names(out) <- nm
  out
}

sumEst <- function(x) {
    rate <- apply(x, 1, mean)
    lower <- apply(x, 1, quantile, 0.025)
    upper <- apply(x, 1, quantile, 0.975)
    cbind(rate, lower, upper)
}
