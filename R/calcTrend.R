#' @title calcTrend
#' 
#' @description  Multipurpose function to calc trends  over time.
#' 
#' @param dat dataset for computing trends. 
#'
#' @param wdat weights.
#'
#' @param Formula string argument for aggregate function; terms and operators must be
#' separated by white space
#'
#' @param stpopVar name of standard population column in \code{wdat}
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
#' @import dplyr
#' 
#' @importFrom epitools binom.exact ageadjust.direct
#' 
#' @importFrom stringr word
#' 
#' @export
#' 
#' @examples
#' inFiles <- getFiles()
#' Args <- setArgs(inFiles, 
#'   AgeCat=c(15, 25, 55), 
#'   Sex="Fem")
#' hiv=getHIV(Args)
#' 
#' # Calculate HIV prevalence using KZN 2015 weights
#' calcTrend(hiv, wdat, Formula="HIVResult ~ Year + AgeCat", mergeVar="AgeCat", calcBy="Year")

calcTrend <- function(
  dat, wdat=NULL, 
  Formula="HIVResult ~ Year + AgeCat",
  calcBy="Year", mergeVars=c("AgeCat"),
  binom=FALSE, fmt=TRUE, ...) {

  Input <- word(Formula, 1)
  adat <- do.call('data.frame', 
    aggregate(as.formula(Formula), data=dat,
    FUN=function(x) c(Count=sum(x), Total=length(x))))
  if (!is.null(wdat)) 
    adat <- merge(adat, wdat, by=mergeVars) 
  adat <- split(adat, adat[calcBy])
  Count <- paste0(Input, '.Count')
  Total <- paste0(Input, '.Total')
  stpopVar <- ifelse(!is.null(wdat), "Total", Total)
  if (binom==FALSE) {
    adat <- lapply(adat, function(x) 
      ageadjust.direct(x[Count], x[Total], 
      stdpop=x[stpopVar]))
  } else {
    adat <- lapply(adat, function(x) 
      binom.exact(x[, Count], x[, Total]))
    # adat <- adat[, c("proportion", "lower", "upper")]
  }
  adat <- do.call("rbind", adat)
  if (fmt==TRUE) 
    adat <- round(adat*100, 2)
  data.frame(adat)
}








