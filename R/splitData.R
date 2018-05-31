#' @title splitData
#' 
#' @description Split the data into year episodes at svar
#' 
#' @param dat dataset from imputation method, eg \code{\link{imputeRandomPoint()}}. 
#'
#' @param svar varname to create obs_end variable
#'
#' @param years to split
#'
#' @return data.frame
#'
#' @import dplyr
#' 
#' @importFrom survival survSplit Surv
#'
#' @examples
#' rtdat <- getRTData(hiv)
#' sdat <- imputeMidPoint(rtdat)
#' sdat <- rename(sdat, sero_date=s1)
#' splitData(sdat, splitYears=Args$Years)

splitData <- function(
  dat=NULL, svar="sero_date", splitYears=NULL) {

  ndate <- function(Years) {
    sapply(Years, function(x)
      as.numeric(as.Date(paste0(x, "-01-01"))))
  }

  if (svar=="sero_date") {
    dat$obs_start0 <- dat$obs_start
    dat <- mutate(dat, obs_end=ifelse(sero_event==1, sero_date, late_neg))
  } else {
    dat <- mutate(dat, obs_end=ifelse(sero_event==1, early_pos, late_neg))
  }

  # Split into episodes
  edat <- survSplit(Surv(
    time=as.integer(obs_start), 
    time2=as.integer(obs_end), 
    event=sero_event) ~ . , 
    data=dat,
    start="obs_start",
    end="obs_end",
    cut=ndate(splitYears))

  vars <- c("obs_start", "obs_end")
  edat[vars] <- lapply(edat[vars], as.Date)

  if (svar=="sero_date") {
    edat <- mutate(edat, 
      Time = as.numeric(difftime(obs_end, obs_start0, units='days')))
  } else {
    edat <- mutate(edat, 
      Time = as.numeric(difftime(obs_end, obs_start, units='days')))
  }
  edat <- mutate(edat, Year=as.integer(format(obs_start, "%Y")))
  tbl_df(edat)
}
