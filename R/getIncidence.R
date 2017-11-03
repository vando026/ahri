#' @title getIncidence
#' 
#' @description Calculates the crude and adjusted incidence.
#' 
#' @param Args takes list from \code{\link{setArgs()}}.
#'
#' @return data.frame
#'
#' @import dplyr

getIncidence <- function(Args) {

  hiv   <- getHIV(Args)
  rtdat <- getRTData(hiv)
  sdat  <- imputeRandomPoint(rtdat)
  wdat <- aggregate(as.formula(
    paste('IIntID ~ ', Args$LHS)),
    data=hiv, length)

  dat <- lapply(seq(Args$nSimulations),
    function(i) doIncData(rtdat, sdat, wdat, Args, i))

  Year <- lapply(dat, 
    function(x) calcIncidence(x, calcBy="Year"))

  Age <- lapply(dat, 
    function(x) calcIncidence(x, calcBy="AgeCat"))
  
  if (Args$nSimulations==1) 
    return(list(Year, Age))

  est_year <- getInc(Year)
  est_age <- getInc(Age)
  lapply(c(est_year, est_age), sumEst)
}

getInc <- function(dat) {
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
