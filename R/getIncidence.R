#' @title aggregateInc
#' 
#' @description Gets the aggregated sero_event counts and total person-years.
#' 
#' @param dat dataset from \code{\link{censorData}}. 
#' 
#' @return data.frame
#'
#' @examples
#' edat <- censorData(rtdat, Args)
#' adat <- getAgeData(edat, Args)
#' inc <- aggregateInc(adat)

aggregateInc <- function(dat) {
  aggregate(cbind(sero_event, pyears=Time/365.25) ~ 
    Year+AgeCat+Female, data=dat, FUN=sum)
}

#' @title doIncData
#' 
#' @description Function used to prepare the data for \code{\link{getIncidence}}.
#' 
#' @param rtdat dataset from \code{\link{getRTData}}. 
#' 
#' @param Args takes list from \code{\link{setArgs}}.
#'
#' @return data.frame

doIncData <- function(rtdat, Args) {
  dat <- Args$imputeMethod(rtdat)
  edat <- censorData(dat, Args) 
  adat <- getAgeData(edat, Args)
  idat <- aggregateInc(adat)
  idat
}

#' @title calcInc
#' 
#' @description Calculates the crude and adjusted incidence.
#' 
#' @param dat Dataset from \code{\link{aggregateInc}}.
#' 
#' @param wdat Dataset of weights from \code\link{getWeightsKZN}}.
#' 
#' @param calcBy Results by Year, Age, or Sex.
#'
#' @return data.frame
#'
#' @importFrom epitools ageadjust.direct
#'
#' @examples
#' edat <- censorData(rtdat,Args)
#' adat <- getAgeData(edat, Args)
#' inc <- aggregateInc(adat)
#' wdat <- getWeightsKZN() 
#' calcInc(inc, wdat)

calcInc <- function(dat, wdat, calcBy="Year") { 
  dat <- merge(dat, wdat, by="AgeCat")
  dat$AgeCat <- factor(dat$AgeCat)
  dat <- split(dat, dat[calcBy])
  dat <- sapply(dat, function(x) ageadjust.direct(
    x["sero_event"],x["pyears"],stdpop=x["Total"]))
  dat <- data.frame(t(dat)*100)
  dat
}

# used in getIncidence
getRate <- function(dat) {
  nm  =c("crude.rate", "adj.rate")
  out <- lapply(nm, function(x) {
    out <- lapply(dat, `[`, x)
    do.call('cbind', out)})
  names(out) <- nm
  out
}

# used in getIncidence
sumEst <- function(x) {
    rate <- apply(x, 1, mean)
    lower <- apply(x, 1, quantile, 0.025)
    upper <- apply(x, 1, quantile, 0.975)
    cbind(rate, lower, upper)
}

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
  wdat <- getWeightsKZN(Args)

  set.seed(Args$Seed)
  dat <- lapply(seq(Args$nSimulations),
    function(i) doIncData(rtdat, Args))

  Year <- lapply(dat, 
    function(x) calcInc(x, wdat, calcBy="Year"))

  Age <- lapply(dat, 
    function(x) calcInc(x, wdat, calcBy="AgeCat"))
  
  if (Args$nSimulations==1) 
    return(list(Year=Year, Age=Age))

  est_year <- getRate(Year)
  est_age <- getRate(Age)
  lapply(c(est_year, est_age), sumEst)
}

