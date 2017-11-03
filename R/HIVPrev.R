#' @title HIVPrev
#' 
#' @description  Calculate HIV prevalence for AHRI data.
#' 
#' @param Args arguments from \code{\link{setArgs}} to pass to \code{getHIV} function
#'
#' @param wdat dataset of standard population weights
#'
#' @param Formula string argument for aggregate function; terms and operators must be
#' separated by white space
#'
#' @param stpopVar name of standard population column in \code{wdat}
#'
#' @param mergeVars variables with which to merge \code{dat} and \code{wdat} datasets
#'
#' @param calcBy variable(s) to calc the trend by
#'
#' @param binom use binomial exact formula to calculate CIs: TRUE/FALSE
#'
#' @param fmt convert to percentage and round: TRUE/FALSE
#'
#' @return data.frame
#'
#' @import dplyr
#' 
#' @importFrom epitools binom.exact
#' 
#' @export

HIVPrev <- function(
  Args, dat, wdat=NULL, 
  Formula="HIVResult ~ Year + Female + AgeCat",
  stpopVar="IIntID", calcBy="Year",
  mergeVars=c("Female", "AgeCat"),
  binom=FALSE, fmt=TRUE) {
  #
  hiv=getHIV(Args)
  calcTrend(hiv, wdat=wdat, Formula=Formula,
    stpopVar=stpopVar, calcBy=calcBy, 
    mergeVars=mergeVars, binom=binom,
    fmt=fmt)
}

