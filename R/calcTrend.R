#' @title calcTrend
#' 
#' @description  Multipurpose function to calc trends  over time.
#' 
#' @param dat dataset from \code{\link{getRTData()}} 
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
#' @importFrom epitools binom.exact ageadjust.direct
#' 
#' @export
#' 
#' @examples
#' inFiles <- getFiles()
#' Args <- setArgs(inFiles, 
#'   Age=list(Mal=c(15, 54), Fem=c(15, 49)),
#'   AgeCat=c(15, 25, 55), 
#'   Sex="Fem", Years=c(2005:2016))
#' hiv=getHIV(Args)
#' 
#' # Calculate HIV prevalence using Census 2011 weights
#' wdat <- read.csv("C:/Users/avandormael/Documents/AC_Data/Derived/Other/Census2011AgeSex.csv", comment="#")
#' wdat <- mutate(wdat, Index = ifelse(row_number()<=2, 0, 1)) %>%
#'   group_by(Index) %>% summarize(Ratio=sum(SexRatio))
#' wdat <- data.frame(cbind(wdat,  AgeCat=c("[15,25)", "[25,55)")))
#' calcTrend(hiv, wdat, stpopVar="Ratio", Formula="HIVResult ~ Year + AgeCat", mergeVar="AgeCat", calcBy="Year")

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


#' @title getWeights
#' 
#' @description get the KZN population weights from StatSA 2015: 
#' https://www.statssa.gov.za/publications/P0302/P03022015.pdf (Table 15)				
#' 
#' @param Args requires Args, see \code{\link{setArgs}}

getWeights <- function(Args) {
  wdat <- read.csv(Args$inFiles$kznwght, header=TRUE, comment="#")
  wdat$AgeCat <- cut(wdat$Index, breaks=Args$AgeCat, 
    labels=NULL, right=FALSE)
  mw <- wdat[wdat$Index >= Args$Age$Mal[1] & wdat$Index <= Args$Age$Mal[2],
    c("AgeCat", "Mal") ]
  fw <- wdat[wdat$Index >= Args$Age$Fem[1] & wdat$Index <= Args$Age$Fem[2],
    c("AgeCat", "Fem")]
  aw <- wdat[, c("AgeCat", "All")] 
  ldat <- list(Fem=fw, Mal=mw, All=aw)
  ldat <- lapply(ldat, function(x)
    aggregate(x[, 2], by=list(x[, 1]), FUN=sum))
  dat <- data.frame(ldat[Args$Sex])
  colnames(dat) <- c("AgeCat", "Total")
  dat
}






