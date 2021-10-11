#' @title getAggregate
#' 
#' @description Gets aggregated numbers by category.
#' 
#' @param Args requires Args, see \code{\link{setArgs}}.
#' @param  Formula. 
#' 
#' @return list
#'
#' @export 
#' @keywords internal
#' @examples
#' Args <- setArgs()
#' hiv <- getHIV()
#' hiv <- setData(hiv, Args)
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
#' @param wdat from \code{\link{getAggregate}}.
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
#' @keywords internal

calcTrend <- function(
  dat, wdat=NULL,
  Formula="HIVResult ~ Year + AgeCat",
  calcBy="Year", mergeVars=c("AgeCat"),
  binom=FALSE, fmt=TRUE, 
  aggName="", aggType=as.integer, ...) {
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
  out <- data.frame(cbind(N=N, edat))
  aggName <- ifelse(aggName=="", calcBy, aggName)
  out[[aggName]] <- aggType(rownames(out))
  out
}

#' @title calcTrendYear
#' 
#' @description  Simpler fuction to calculate trends by Year.
#' 
#' @param RHS The right hand side variable, must be string name.
#' @param dat A dataset. 
#' 
#' @return data.frame
#'
#' @keywords internal
#' @export 
calcTrendYear <- function(RHS="", dat) {
  dat <- group_by(dat, Year) %>%
    summarize(Count = sum(get(RHS)), N = n())
  edat <- epitools::binom.exact(dat$Count, dat$N)
  edat <- select(edat,
    crude.rate = proportion, lci=lower, uci=upper )
  edat <- round(edat*100, 2)
  cbind(Year=dat$Year, N=dat$N, edat)
}

