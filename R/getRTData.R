#' @title getRTData
#' 
#' @description  Get all repeat testers from HIV surveillance.
#' 
#' @param dat dataset from \code{\link{getHIV}}. 
#'
#' @param onlyRT Drops IDs who are not repeat-testers.
#'
#' @return data.frame
#'
#' @import dplyr
#'
#' @export
#'
#' @examples 
#' hiv <- getHIV(Args)
#' rtdat <- getRTData(hiv)

getRTData <- function(dat, onlyRT=TRUE) {

  # function  get dates 
  early_neg <- getDatesMin(dat, "HIVNegative", "early_neg")
  early_pos <- getDatesMin(dat, "HIVPositive", "early_pos")
  late_neg <- getDatesMax(dat, "HIVNegative", "late_neg")
  late_pos <- getDatesMax(dat, "HIVPositive", "late_pos")
  dat <- distinct(dat, IIntID, Female)
  dat <- suppressMessages(Reduce(left_join, 
    list(dat, early_neg, late_neg, early_pos, late_pos)))

  # We have LatestNegativeDate after EarliestHIVPositive. 02May2016:  101 individuals
  rtdat <- mutate(dat, late_neg_after = ifelse(
    (late_neg > early_pos) & is.finite(early_pos) & is.finite(late_neg), 1, 0)) 
  # ** I just drop these individuals, irreconcilable
  rtdat <- filter(rtdat, late_neg_after==0) %>% 
    select(-c(late_neg_after, late_pos))
  if (onlyRT) {
    # Drop any indiv that dont have a first neg date.
    rtdat <- filter(rtdat, !(is.na(early_neg) & is.na(late_neg)))
    # Must have two tests, if early neg date is equal to late neg date and missing pos date then drop
    rtdat <- filter(rtdat, !(early_neg==late_neg & is.na(early_pos)))
  }
  rtdat <- mutate(rtdat, sero_event = ifelse(is.finite(early_pos), 1, 0))
  # Make for split episodes later rather than in loop to save time
  rtdat <- rename(rtdat, obs_start = early_neg)
  vars <- c("obs_start", "late_neg", "early_pos")
  rtdat[vars] <- lapply(rtdat[vars], as.Date, origin="1970-01-01")
  rtdat
}

#' @title getDates
#' 
#' @description Function to get earliest/latest test dates
#' 
#' @param  f a function.
#' 
#' @return data.frame
#' 
#' @export
getDates <- function(f) {
  function(dat, Var, Name) {
    dat <- data.frame(dat[!is.na(dat[, Var]), c("IIntID", Var)])
    dates <- tapply(dat[, Var], dat[, "IIntID"], f)
    out <- data.frame(as.integer(names(dates)), dates)
    colnames(out) <- c("IIntID", Name)
    out
  }
}

#' @title getDatesMin
#' 
#' @description Function to get earliest test dates
#' 
#' @param  dat a dataset.
#' @param  Var a variable name.
#' @param  Name new variable name.
#' 
#' @return data.frame
#' 
#' @export
#'
#' @examples
#' hdat <- getHIV()
#' getDatesMin(dat, "HIVNegative", "early_neg")

getDatesMin <- getDates(min)

#' @title getDatesMax
#' 
#' @description Function to get latest test dates
#' 
#' @param  dat a dataset.
#' @param  Var a variable name.
#' @param  Name new variable name.
#' 
#' @return data.frame
#' 
#' @export
#'
#' @examples
#' hdat <- getHIV(Args)
#' getDatesMax(dat, "HIVNegative", "late_neg")

getDatesMax <- getDates(max)



#' @title getHIVDatesLong
#' 
#' @description Gets HIV dates and saves in long format. Used mainly for IntCens.  
#' 
#' @param dat Dateset from \code{\link{getHIV}}.
#' 
#' @return 
#'
#' @export 

getHIVDatesLong <- function(dat) {
  dat <- getHIV(Args)
  early_neg <- getDatesMin(dat, "HIVNegative", "early_neg")
  early_pos <- getDatesMin(dat, "HIVPositive", "early_pos")
  late_neg <- getDatesMax(dat, "HIVNegative", "late_neg")
  late_pos <- getDatesMax(dat, "HIVPositive", "late_pos")
  dat <- distinct(dat, IIntID, Female)
  dat <- Reduce(left_join, 
    list(early_neg, late_neg, early_pos, late_pos))
}



