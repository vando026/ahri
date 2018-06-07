#' @title getIndDat
#' 
#' @description  Pull in Individuals dataset
#' 
#' @param Args requires Args, see \code{\link{setArgs}}
#'
#' @return data.frame
#'
#' @import readr 
#'
#' @export 

getIndDat <- function(Args) {
  idat <- read_csv(Args$inFiles$indfile, 
    col_types=cols_only(
      IIntID="i", DateOfBirth="D"))
  idat <- subset(idat, !duplicated(IIntID))
  idat
}
