#' @title splitData
#' 
#' @description Split the data into year episodes at svar.
#' 
#' @param dat dataset from imputation method, eg \code{\link{imputeRandomPoint()}}. 
#'
#' @param svar varname to create obs_end variable. Can either split at early_pos or at
#' sero_date.
#'
#' @param splitYears years to split.
#'
#' @return data.frame
#'
#' @import dplyr
#' 
#' @importFrom survival survSplit Surv
#'
#' @export
#'
#' @examples
#' rtdat <- getRTData(hiv)
#' sdat <- imputeMidPoint(rtdat)
#' sdat <- rename(sdat, sero_date=s1)
#' splitData(sdat, splitYears=Args$Years, svar=sero_date)

splitData <- function(
  dat=NULL,  splitYears=NULL,
  svar="sero_date") {

  ndate <- function(Years) {
    sapply(Years, function(x)
      as.numeric(as.Date(paste0(x, "-01-01"))))
  }

  dat$obs_start0 <- dat$obs_start
  if (svar=="sero_date") {
    dat <- mutate(dat, obs_end=ifelse(sero_event==1, sero_date, late_neg))
  } else if (svar=="early_pos") {
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

  if (svar=="sero_date") { # this is needed for IntCens var
    edat <- mutate(edat, 
      Time = as.numeric(difftime(obs_end, obs_start, units='days')))
  } else if (svar=="early_pos") { # this is needed for IncCalc
   edat <- mutate(edat, Time = as.numeric(obs_end - obs_start0))
  }
  edat <- mutate(edat, Year=as.integer(format(obs_start, "%Y")))
  tbl_df(edat)
}

#' @title splitData2
#' 
#' @description  version 2 of \code{\link{splitData}}.
#' 
#' @param  dat Dataset must have variables called obs_start, obs_end and event.
#' 
#' @return data.frame
#'
#' @importFrom survival survSplit Surv
#'
#' @export 

splitData2 <- function(
  dat, years=Args$Years) {
  edat <- survSplit(Surv(
    time=as.integer(obs_start), 
    time2=as.integer(obs_end), 
    event=event) ~ . , 
    data=dat,
    start="obs_start",
    end="obs_end",
    cut=getYearDates(years))
  vars <- c("obs_start", "obs_end")
  edat[vars] <- lapply(edat[vars], as.Date)
  edat <- mutate(edat, Year=as.integer(format(obs_start, "%Y")))
  tbl_df(edat)
}


#' @title getYearDates
#' 
#' @description  gets numeric dates for 31DecYYYY.
#' 
#' @param Years 
#' 
#' @return vector
#'
#' @export 

getYearDates <- function(Years) {
  sapply(Years, function(x)
    as.numeric(as.Date(paste0(x, "-01-01"))))
}

#' @title splitAtSeroDate
#' 
#' @description Split data at the imputed seroconversion date.
#' 
#' @param dat a dataset
#' @param splitYears Vector from \code{\link{getYearDates}}.
#' 
#' @return data.frame 
#'
#' @export 
#'
#' @examples
#' hiv <- getHIV(Args)
#' rtdat <- getRTData(hiv)
#' sdat <- imputeMidPoint(rtdat)
#' splitAtSeroDate(sdat, splitYears=Args$Years)

splitAtSeroDate <- function(
  dat=NULL,  splitYears=Args$Years) {
  dat <- rename(dat, event = sero_event)
  dat <- mutate(dat, obs_end=ifelse(event==1, sero_date, late_neg))
  edat <- splitData2(dat, splitYears)
  edat <- mutate(edat, Time = as.numeric(difftime(obs_end, obs_start, units='days')))
  edat <- rename(edat, sero_event = event)
  tbl_df(edat)
}

#' @title splitAtEarlyPos
#' 
#' @description Split data at the earliest HIV-positive date. 
#' 
#' @param dat a dataset
#' @param splitYears Vector from \code{\link{getYearDates}}.
#' 
#' @return data.frame
#'
#' @export 
#'
#' @examples
#' hiv <- getHIV(Args)
#' rtdat <- getRTData(hiv)
#' sdat <- imputeMidPoint(rtdat)
#' splitAtEarlyPos(sdat, splitYears=Args$Years)

splitAtEarlyPos <- function(
  dat=NULL,  splitYears=NULL) {
  dat$obs_start0 <- dat$obs_start
  dat <- mutate(dat, obs_end=ifelse(sero_event==1, early_pos, late_neg))
  dat <- rename(dat, event = sero_event)
  edat <- splitData2(dat, splitYears)
  edat <- mutate(edat, Time = as.numeric(obs_end - obs_start0))
  edat <- rename(edat, sero_event = event)
  tbl_df(edat)
}

