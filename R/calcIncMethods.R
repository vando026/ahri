#' @title aggregateInc
#' 
#' @description Gets the aggregated sero_event counts and total person-years.
#' 
#' @param dat dataset from \code{\link{doIncData}}. 
#' 
#' @return data.frame
#'
#' @examples
#' edat <- censorData(rtdat, sdat, Args)
#' adat <- getAgeData(edat, Args)
#' inc <- aggregateInc(adat)

aggregateInc <- function(dat) {
  aggregate(cbind(sero_event, pyears=Time/365.25) ~ 
    Year+AgeCat+Female, data=dat, FUN=sum)
}

#' @title calcInc
#' 
#' @description Calculates the crude and adjusted incidence.
#' 
#' @param dat Dataset with weights in IIntID column.
#' 
#' @param calcBy Results by Year, Age, or Sex.
#'
#' @return data.frame
#'
#' @importFrom epitools ageadjust.direct
#'
#' @examples
#' edat <- censorData(rtdat, sdat, Args)
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

#' @title doIncData
#' 
#' @description pulls in all the data to calculate the incidence rate for
#' \code{nSimulations} in \code{\link{setArgs}}.
#' 
#' @param rtdat dataset from \code{\link{getRTData}}. 
#' 
#' @param sdat dataset from \code{\link{imputeRandomPoint}}.
#'
#' @param wdat dataset of weights from \code{\link{getWeights}}.
#'
#' @param Args takes list from \code{\link{setArgs}}.
#'
#' @param i the ith column to use as serodates, taken from
#' \code{\link{imputeRandomPoint}}.
#'
#' @return data.frame

doIncData <- function(rtdat, sdat, Args, i) {
    si <- paste0("s", i)
    sdates <- sdat[, c("IIntID", si)]
    names(sdates) <- c("IIntID", "sero_date")
    edat <- censorData(rtdat, sdates, Args) 
    adat <- getAgeData(edat, Args)
    idat <- aggregateInc(adat)
    idat
}

