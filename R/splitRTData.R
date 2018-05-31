#' @title splitRTData
#' 
#' @description  Splits repeat-tester data into annual episodes. Data is right censored at
#' the latest HIV-negative test date or imputed seroconversion date. 
#' 
#' @param dat dataset from \code{\link{getRTData()}}. 
#'
#' @param splitYears  vector of values to split by
#'
#' @param Origin a date value (eg "2014-01-01") which is used to create a variable Time
#' from origin 
#'
#' @param Scale specify a time scale
#'
#' @return data.frame
#'
#' @import dplyr
#' 
#' @importFrom survival survSplit Surv
#'
#' @examples 
#' hiv <- getHIV(Args)
#' rtdat <- getRTData(hiv)
#' sdat <- splitRTData(rtdat)

splitRTData <- function(
  dat, splitYears=c(2004:2015),
  Origin="", Scale=1) {

  dat <- mutate(dat, obs_end = 
    ifelse(sero_event==1, early_pos, late_neg))

  # Make numeric for survplit
  dat$obs_start0 <- dat$obs_start
  date_origin <- as.numeric(Origin)
  vars <- c("obs_start",  "obs_end", "obs_start0")
  dat[vars] <- lapply(dat[vars], as.numeric)

  # Now split episodes
  yr_cut <- ndate(splitYears)
  dat <- survSplit(
    Surv(time=obs_start, time2=obs_end, event=sero_event) ~ .,
    data=dat, 
    start="obs_start",
    cut=yr_cut)

  # Calculate time since early.neg exposure
  dat <- mutate(dat,
    TimeExp = (obs_start - obs_start0)/Scale,
    TimeOrigin = (obs_end - date_origin)/Scale)

  # Reformat back to time var for difftime
  dat[vars[1:2]] <- lapply(dat[vars[1:2]], as.Date, origin="1970-01-01")
  dat <- mutate(dat, Year=as.integer(format(obs_start, "%Y")))
  dat <- dat[, !(names(dat) %in% c("obs_start0", "date_origin"))]
  tbl_df(dat)
}

#' @title ndate
#' 
#' @description  Splits time interval into years. 
#' 
#' @param Years  Creates episode at YEAR-01-01.
#'
#' @return numeric vector
#'
#' @examples 
#' ndate(Args$Years) 

ndate <- function(Years) {
  sapply(Years, function(x)  
    as.numeric(as.Date(paste0(x, "-01-01"), 
    origin="1970-01-01")))
}
