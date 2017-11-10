#' @title getWeights
#' 
#' @description Get weights for age and sex adjustement. 
#' 
#' @param Args requires Args, see \code{\link{setArgs}}.
#' 
#' @param KZN get the KZN population weights from StatSA 2015: 
#' https://www.statssa.gov.za/publications/P0302/P03022015.pdf (Table 15), or get weights
#' from the HIV surveillance. 

getWeights <- function(Args, KZN=FALSE) {
  if (KZN==TRUE) {
    wdat <- read.csv(Args$inFiles$kznwght, header=TRUE, comment="#")
  } else {
    wdat <- read.csv(Args$inFiles$ahriwgt, header=TRUE)
  }
  doWt <- function(sex="All", dat=wdat) {
    ages=unlist(Args$Age[sex])
    wt <- dat[(dat$Index >= ages[1]) & (dat$Index <= ages[2]),]
    wt$AgeCat <- cut(wt$Index, breaks=Args$AgeCat,
      labels=NULL, right=FALSE)
    aggregate(list(Total=wt[, sex]), 
      by=list(AgeCat=wt[, "AgeCat"]), FUN=sum)
  }
  doWt(Args$Sex, dat=wdat)
}


