#' @title getIndDat
#' 
#' @description  Pull in Individuals dataset
#' 
#' @param infile requires Args, see \code{\link{setArgs}}
#'
#' @return data.frame
#'
#' @import readr 
#'
#' @export 

getIndDat <- function(inFile=Args$inFiles$indfile) {
  idat <- read_csv(inFile,
    col_types=cols_only(
      IIntID="i", DateOfBirth="D"))
  idat <- subset(idat, !duplicated(IIntID))
  idat
}
