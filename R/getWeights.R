#' @title getWeightsKZN
#' 
#' @description get the KZN population weights from StatSA 2015: 
#' https://www.statssa.gov.za/publications/P0302/P03022015.pdf (Table 15)				
#' 
#' @param Args requires Args, see \code{\link{setArgs}}

getWeightsKZN <- function(Args) {
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
  ldat <- data.frame(ldat[Args$Sex])
  colnames(ldat) <- c("AgeCat", "Total")
  ldat
}
