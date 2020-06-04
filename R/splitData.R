#' @title splitData2
#' 
#' @description  Function for splitting data into episodes. 
#' 
#' @param  dat Dataset must have variables called obs_start, obs_end and event.
#' @param  years The years by which to split the data.
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
  edat[vars] <- lapply(edat[vars], 
    function(x) as.Date(x, origin="1970-01-01"))
  edat <- mutate(edat, 
    Year=as.integer(format(obs_start, "%Y")))
  tibble::as_tibble(edat)
}


#' @title getYearDates
#' 
#' @description  gets numeric dates for 31DecYYYY.
#' 
#' @param  Years The years by which to split the data.
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
#' Args <- setArgs(Years=c(2008:2018))
#' hiv <- setHIV(Args)
#' rtdat <- getRTData(hiv)
#' sdat <- imputeMidPoint(rtdat)
#' splitAtSeroDate(sdat, splitYears=Args$Years)

splitAtSeroDate <- function(
  dat=NULL,  splitYears=c(2003:2019)) {
  dat <- rename(dat, event = .data$sero_event)
  dat <- mutate(dat, obs_end=ifelse(.data$event==1, .data$sero_date, .data$late_neg))
  edat <- splitData2(dat, years=splitYears)
  edat <- mutate(edat, Time = as.numeric(.data$obs_end - .data$obs_start))
  if(any(edat$Time>366)) stop("Days > 366")
  edat <- rename(edat, sero_event = .data$event)
  tibble::as_tibble(edat)
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
#' Args <- setArgs(Years=c(2008:2018))
#' hiv <- setHIV(Args)
#' rtdat <- getRTData(hiv)
#' splitAtEarlyPos(rtdat, splitYears=Args$Years)

splitAtEarlyPos <- function(
  dat=NULL,  splitYears=c(2003:2018)) {
  dat <- mutate(dat, obs_end=ifelse(.data$sero_event==1, .data$early_pos, .data$late_neg))
  dat <- rename(dat, event = .data$sero_event)
  edat <- splitData2(dat, years=splitYears)
  edat <- mutate(edat, Time = as.numeric(.data$obs_end - .data$obs_start))
  if(any(edat$Time > 366)) stop("Days > 366")
  edat <- rename(edat, sero_event = .data$event)
  tibble::as_tibble(edat)
}

