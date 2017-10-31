#' @title getIncData
#' 
#' @description Gets the aggregated sero_event counts and total person-years.
#' 
#' @param edat dataset from \code{\link{censorData()}}. 
#' 
#' @param LHS Takes lefthand side of formula for aggregation.
#'
#' @return data.frame
#'
#' @examples 
#' hiv <- getIncData(edat)

getIncData <- function(edat, LHS="Year") {
  inc <- aggregate(as.formula(paste(
    "cbind(sero_event, pyears=Time/365.25) ~ ",
    LHS)), data=edat, FUN=sum)
  inc
}

#' @title calcIncidence
#' 
#' @description Calculates the crude and adjusted incidence.
#' 
#' @param dat Dataset with weights in IIntID column.
#' 
#' @param calcBy Results by Year, Age, or Sex.
#'
#' @return data.frame
#'
#' @examples 
#' byVars = c("Year", "Female", "AgeCat")
#' LHS1 = paste(byVars, collapse='+')
#' idat <- getIncData(adat, LHS=LHS1)
#' wdat <- aggregate(as.formula(paste('IIntID ~ ', LHS1)),
#' data=hiv, length)
#' wdat <- merge(idat, wdat, by=byVars)
#' calcIncidence(wdat, calcBy="Year")

calcIncidence <- function(dat, calcBy="Year") { 
  dat <- split(dat, dat[calcBy])
  dat <- sapply(dat, function(x) ageadjust.direct(
    x[,"sero_event"],x[,"pyears"],stdpop=x[, "IIntID"]))
  dat <- data.frame(t(dat)*100)
  dat
}


#' @title doIncData
#' 
#' @description Function to calculate the incidence rate for \code{nSimulations}.
#' 
#' @param rtdat dataset from \code{\link{getRTData()}}. 
#' 
#' @param sdat dataset from \code{\link{imputeRandomPoint()}}.
#'
#' @param args takes list from \code{\link{setargs()}}
#'
#' @param i the ith column to use as serodates, taken from
#' \code{\link{imputeRandomPoint()}}.
#'
#' @return data.frame
#'
#' @import dplyr
#'

doIncData <- function(rtdat, sdat, wdat, Args, i) {
    si <- paste0("s", i)
    sdates <- sdat[, c("IIntID", si)]
    names(sdates) <- c("IIntID", "sero_date")
    edat <- censorData(rtdat, sdates, Args) 
    adat <- getAgeData(edat, Args)
    idat <- getIncData(adat, LHS=Args$LHS)
    dat <- merge(idat, wdat, by=Args$byVars)
    dat
}



