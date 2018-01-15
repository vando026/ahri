#' @title imputeMidPoint
#' 
#' @description  Impute a mid-point seroconversion date within the censored interval.
#' 
#' @param dat dataset from \code{\link{getRTData()}}. 
#'
#' @return data.frame
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
#' @import dplyr

imputeRandomPoint <- function(dat) {
  pdat <- dat[is.finite(dat$early_pos), ]
  sdat <- mapply(runif, 1, pdat$late_neg + 1, pdat$early_pos)
  sdat <- cbind(IIntID=pdat["IIntID"], sero_date=sdat)
  dat <- merge(dat, sdat, by="IIntID", all.x=TRUE)
  dat$sero_date <- as.Date(dat$sero_date, origin = "1970-01-01")
  dat
}

#' @title censorData
#' 
#' @description Censor the data into year episodes at the imputed date or latest
#' HIV-negative date. 
#' 
#' @param dat dataset from imputation method, eg \code{\link{imputeRandomPoint()}}. 
#'
#' @param Args takes list from \code{\link{setArgs()}}.
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
#' censorData(rtdat, sdat, Args)

censorData <- function(
  dat=NULL, 
  Args=eval.parent(quote(Args))) {

  dat <- mutate(dat, 
    obs_end=ifelse(sero_event==1, sero_date, late_neg))

  # testInterval(dat)
  # Split into episodes
  edat <- survSplit(Surv(
    time=as.integer(obs_start), 
    time2=as.integer(obs_end), 
    event=sero_event) ~ . , 
    data=dat,
    start="obs_start",
    end="obs_end",
    cut=ndate(Args$Years))

  vars <- c("obs_start", "obs_end")
  edat[vars] <- lapply(edat[vars], as.Date, origin="1970-01-01")
  edat <- mutate(edat, 
    Time = difftime(obs_end, obs_start, units='days'),
    Year=as.integer(format(obs_start, "%Y")))
  tbl_df(edat)
}
