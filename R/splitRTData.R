#' @title splitRTData
#' 
#' @description  Splits repeat-tester data into annual episodes.
#' 
#' @param dat dataset from \code{\link{getRTData()}}. 
#'
#' @param Args takes list from \code{\link{setArgs()}}. 
#'
#' @return data.frame
#'
#' @import dplyr, survival
#'
#' @examples 
#' hiv <- getHIV(Args)
#' rtdat <- getRTData(hiv)
#' sdat <- splitRTdat(rtdat)

splitRTData <- function(dat, 
  Args=eval.parent(quote(Args))) {
  # Make obs_end
  dat <- mutate(dat, obs_end = 
    ifelse(sero_event==1, early_pos, late_neg))
  dat$obs_start0 <- dat$obs_start

  # Make numeric for sursplit
  vars <- c("obs_start", "obs_end")
  dat[vars] <- lapply(dat[vars], as.numeric)

  # Now split episodes
  ndate <- function(Args) {
    ndate <- c()
    for (yr in c(Args$Years)) {
      ydate <- as.Date(paste0(yr, "-01-01"), origin="1970-01-01")
      ndate <- c(ndate, as.numeric(ydate))
    }
  ndate
}
  yr_cut <- ndate(Args)
  dat <- survival::survSplit(
    Surv(time=obs_start, time2=obs_end, event=sero_event) ~ .,
    data=dat, 
    start="obs_start",
    cut=yr_cut)

  # Reformat back to time var for difftime
  dat[vars] <- lapply(dat[vars], as.Date, origin="1970-01-01")

  # Calculate time since exposure, early.neg
  dat <- mutate(dat, Time =  round(as.numeric(difftime(
    obs_end, obs_start0, units="days")+1)*0.00273790700698851, 2))

  dat <- mutate(dat, Year=as.integer(format(obs_start, "%Y")))
  dat <- select(dat, IIntID, Female, Time, sero_event, Year, obs_start, obs_end)

  tbl_df(dat)
}
# tt <- splitRTdat(rtdat, Args)


