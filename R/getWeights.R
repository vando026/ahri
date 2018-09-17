#' @title getWeights
#' 
#' @description Get weights for age and sex adjustement. 
#' 
#' @param Args requires Args, see \code{\link{setArgs}}.
#' 
#' @export

getWeights <- function(Args) {
  dat <- getHIV(Args)
  dat <- filter(dat, HIVResult==0)
  dat <- rename(dat, Total=IIntID)
  aggregate(Total ~ Year + AgeCat, 
    data=dat, FUN=length)
} 
