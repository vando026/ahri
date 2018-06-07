#' @title imputeMidPoint
#' 
#' @description  Impute a mid-point seroconversion date within the censored interval.
#' 
#' @param dat dataset from \code{\link{getRTData()}}. 
#'
#' @return data.frame
#'
#' @export
#' 
#' @import dplyr

imputeMidPoint <- function(dat) {
  dat$sero_date <-  
    (as.numeric(dat$late_neg) + as.numeric(dat$early_pos))/2
  dat$sero_date <- as.Date(dat$sero_date, origin='1970-01-01') 
  dat
}

#' @title imputeRandomPoint
#' 
#' @description  Impute random seroconversion date(s) within the censored interval.
#' 
#' @param dat dataset from \code{\link{getRTData()}}. 
#'
#' @return data.frame
#'
#' @export
#' 
#' @import dplyr

imputeRandomPoint <- function(dat) {
  pdat <- dat[is.finite(dat$early_pos), ]
  sdat <- mapply(runif, 1, pdat$late_neg + 1, pdat$early_pos)
  sdat <- cbind(IIntID=pdat["IIntID"], sero_date=sdat)
  dat <- merge(dat, sdat, by="IIntID", all.x=TRUE)
  dat$sero_date <- as.Date(dat$sero_date, origin = "1970-01-01")
  dat
}

#' @title imputeEndPoint
#' 
#' @description  Impute a end-point seroconversion date within the censored interval.
#' 
#' @param dat dataset from \code{\link{getRTData()}}. 
#'
#' @return data.frame
#'
#' @export
#' 
#' @import dplyr

imputeEndPoint <- function(dat) {
  dat$sero_date <- dat$early_pos
  dat
}

