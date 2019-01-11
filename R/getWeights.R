#' @title getWeights
#' 
#' @description Get weights for age and sex adjustement. 
#' 
#' @export

getWeights <- function() {
  Args <- setArgs()
  dat <- getHIV(Args)
  dat <- setData(dat)
  dat <- filter(dat, HIVResult==0)
  dat <- rename(dat, Total=IIntID)
  aggregate(Total ~ Year + AgeCat, 
    data=dat, FUN=length)
} 
