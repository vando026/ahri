#' @title getAggregate
#' 
#' @description Gets aggregated numbers by category.
#' 
#' @param  Args.
#' @param  Formula. 
#' 
#' @return list
#'
#' @export 
#' @examples
#' Args <- setArgs()
#' hiv <- getHIV(Args)
#' hiv <- setData(hiv)
#' getAggregate(hiv, "IIntID ~ Year + HIVResult")
getAggregate <- function(dat, 
  Formula = "IIntID ~ Year") {
  F1 <- as.formula(Formula)
  dat <- aggregate(as.formula(Formula),
    data=dat, FUN=length)
  dat
}

#' @title calcTrend
#' 
#' @description  Multipurpose function to calc trends over time.
#' 
#' @param dat dataset for computing trends. 
#'
#' @param wdat from \code{\link{getPopAgg}}.
#'
#' @param Formula string argument for aggregate function; terms and operators must be
#' separated by white space.
#'
#' @param mergeVars variables with which to merge \code{dat} and \code{wdat} datasets,
#' typically \code{AgeCat} variable. 
#'
#' @param calcBy variable(s) to calc the trend by.
#'
#' @param binom use binomial exact formula to calculate CIs: TRUE/FALSE.
#'
#' @param fmt convert to percentage and round: TRUE/FALSE.
#'
#' @return data.frame
#'
#' @export

calcTrend <- function(
  dat, wdat=NULL,
  Formula="HIVResult ~ Year + AgeCat",
  calcBy="Year", mergeVars=c("AgeCat"),
  binom=FALSE, fmt=TRUE, ...) {
  Input <- strsplit(Formula,' ')[[1]][1]
  dat <- do.call('data.frame', 
    aggregate(as.formula(Formula), data=dat,
    FUN=function(x) c(Count=sum(x), Total=length(x))))
  if (!is.null(wdat)) 
    dat <- merge(dat, wdat, by=mergeVars) 
  adat <- split(dat, dat[calcBy])
  Count <- paste0(Input, '.Count')
  Total <- paste0(Input, '.Total')
  stpopVar <- ifelse(!is.null(wdat), "IIntID", Total)
  if (binom==FALSE) {
    edat <- lapply(adat, function(x) 
      epitools::ageadjust.direct(x[Count], x[Total], 
      stdpop=x[stpopVar]))
  } else {
    edat <- lapply(adat, function(x) 
      epitools::binom.exact(x[, Count], x[, Total]))
  }
  edat <- do.call("rbind", edat)
  if (fmt==TRUE) edat <- round(edat*100, 2)
  N <- tapply(dat[, Total], dat[, calcBy], sum)
  data.frame(cbind(N=N, edat))
}


