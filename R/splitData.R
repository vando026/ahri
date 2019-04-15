#' @title splitData2
#' 
#' @description  version 2 of \code{\link{splitData}}.
#' 
#' @param  dat Dataset must have variables called obs_start, obs_end and event.
#' 
#' @return data.frame
#D 
#' @importFrom survival survSplit Surv
#'
#' @export 

splitData2 <- function(
  dat, years=c(2003:2018)) {
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
  dat=NULL,  splitYears=c(2003:2018)) {
  dat <- rename(dat, event = sero_event)
  dat <- mutate(dat, obs_end=ifelse(event==1, sero_date, late_neg))
  edat <- splitData2(dat, years=splitYears)
  edat <- mutate(edat, Time = as.numeric(obs_end - obs_start))
  if(any(edat$Time>366)) stop("Days > 366")
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
  dat=NULL,  splitYears=c(2003:2018)) {
  dat <- mutate(dat, obs_end=ifelse(sero_event==1, early_pos, late_neg))
  dat <- rename(dat, event = sero_event)
  edat <- splitData2(dat, years=splitYears)
  edat <- mutate(edat, Time = as.numeric(obs_end - obs_start))
  if(any(edat$Time > 366)) stop("Days > 366")
  edat <- rename(edat, sero_event = event)
  tbl_df(edat)
}

