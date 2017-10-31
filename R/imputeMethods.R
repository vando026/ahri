#' @title imputeMidPoint
#' 
#' @description  Impute a mid-point seroconversion date within the censored interval.
#' 
#' @param dat dataset from \code{\link{getRTData()}}. 
#'
#' @param Args takes list from \code{\link{setArgs()}}, need seed and nSimulation
#' arguments specifically.
#'
#' @return data.frame
#'
#' @import dplyr

imputeMidPoint <- function(dat) {
  dat <- filter(dat, is.finite(early_pos))
  dat <-  mutate(dat, s1 = 
    (as.numeric(late_neg) + as.numeric(early_pos))/2,
    s1 = as.Date(s1, origin='1970-01-01')) %>% 
    select(IIntID, s1)
  dat
}

#' @title imputeRandomPoint
#' 
#' @description  Impute random seroconversion date(s) within the censored interval. Needs
#' \code{Args$seed} and \code{Args$nSimulations}.
#' 
#' @param dat dataset from \code{\link{getRTData()}}. 
#'
#' @param Args takes list from \code{\link{setArgs()}}.
#'
#' @return data.frame
#'
#' @import dplyr

imputeRandomPoint <- function(dat, Args) {
  dat <- filter(dat, is.finite(early_pos))
  set.seed(Args$seed)
  mat <- with(dat, mapply(runif, Args$nSimulations,
    late_neg+1, early_pos, SIMPLIFY=FALSE))
  mat <- data.frame(do.call("rbind", mat))
  mat[] <- lapply(mat, as.Date, origin='1970-01-01')
  colnames(mat) <- paste0("s", seq(ncol(mat)))
  tbl_df(cbind(dat[, "IIntID"], mat))
}

#' @title censorData
#' 
#' @description Censor the data into year episodes at the imputed date or latest
#' HIV-negative date. 
#' 
#' @param dat dataset from \code{\link{getRTData()}}. 
#'
#' @param sdat dataset from \code{\link{imputeMethod()}}. 
#'
#' @param Args takes list from \code{\link{setArgs()}}.
#'
#' @return data.frame
#'
#' @import dplyr, survival
#'
#' @examples
#' rtdat <- getRTData(hiv)
#' sdat <- imputeMidPoint(rtdat)
#' sdat <- rename(sdat, sero_date=s1)
#' censorData(rtdat, sdat, Args)

censorData <- function(
  dat=NULL, sdat=NULL, Args) {
  # Bring in imputed days
  dat <- left_join(dat, sdat, by="IIntID")

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
  edat <- mutate(edat, Time = difftime(obs_end, obs_start, units='days'),
    Year=as.integer(format(obs_start, "%Y")))
  tbl_df(edat)
}
