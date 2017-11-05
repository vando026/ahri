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
#'
#' @examples
#' Args <- setArgs(inFiles, 
#'   Age=list(Mal=c(15, 54), Fem=c(15, 49)),
#'   AgeCat=c(15, 25, 55), 
#'   Sex="Fem", Years=c(2005:2016))
#' 
#' # Calculate HIV prevalence using Census 2011 weights
#' wdat <- read.csv("C:/Users/avandormael/Documents/AC_Data/Derived/Other/Census2011AgeSex.csv", comment="#")
#' wdat <- mutate(wdat, Index = ifelse(row_number()<=2, 0, 1)) %>%
#'   group_by(Index) %>% summarize(Ratio=sum(SexRatio))
#' wdat <- data.frame(cbind(wdat,  AgeCat=c("[15,25)", "[25,55)")))
#' HIVPrev(Args, wdat, stpopVar="Ratio", Formula="HIVResult ~ Year + AgeCat", mergeVar="AgeCat", calcBy="Year")

HIVPrev <- function(
  Args, wdat=NULL, 
  Formula="HIVResult ~ Year + AgeCat",
  calcBy="Year", mergeVars=c("AgeCat"),
  binom=FALSE, fmt=TRUE, ...) {
   
  hiv=getHIV(Args)
  calcTrend(hiv, wdat=wdat, Formula=Formula, 
    calcBy=calcBy, mergeVars=mergeVars, binom=binom, fmt=fmt)
}

