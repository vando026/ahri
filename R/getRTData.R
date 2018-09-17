#' @title getRTData
#' 
#' @description  Get all repeat testers from HIV surveillance.
#' 
#' @param dat dataset from \code{\link{getHIV()}}. 
#'
#' @param Args takes an Args list from \code{\link{setArgs()}}. 
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

getRTData <- function(dat, 
  Args=eval.parent(quote(Args))) {

  # Now make the dates
  dat <- group_by(dat, IIntID) %>% mutate(
    early_neg = min(HIVNegative, na.rm=TRUE),
    late_neg = max(HIVNegative, na.rm=TRUE),
    early_pos = min(HIVPositive, na.rm=TRUE),
    late_pos = max(HIVPositive, na.rm=TRUE)
  )

  dat <- group_by(dat, IIntID) %>% slice(1) %>% ungroup(dat)

  # We have LatestNegativeDate after EarliestHIVPositive. 02May2016:  101 individuals
  rtdat <- mutate(dat, late_neg_after = ifelse(
    (late_neg > early_pos) & is.finite(early_pos) & is.finite(late_neg), 1, 0)) 

  # ** I just drop these individuals, irreconcilable
  rtdat <- filter(rtdat, late_neg_after==0) 

  # Drop any indiv that dont have a first neg date.
  rtdat <- filter(rtdat, !(is.infinite(early_neg) & is.infinite(late_neg)))

  # Must have two tests, if early neg date is equal to late neg date and missing pos date then drop
  rtdat <- filter(rtdat, !(early_neg==late_neg & is.infinite(early_pos)))
  rtdat <- mutate(rtdat, sero_event = ifelse(is.finite(early_pos), 1, 0)) %>%
    select(IIntID, Female, early_neg, late_neg, early_pos, sero_event)

  ### Sanity Checks
  testDates <- function(dat=NULL) {
    testNeg <- filter(dat, is.finite(early_neg) & is.finite(late_neg))
    if(any(with(testNeg, early_neg > late_neg))) 
        stop("Some early_neg > late_neg") 
    testPos <- filter(dat, is.finite(early_pos) & is.finite(late_neg))
    if(any(with(testPos, late_neg >= early_pos))) 
        stop("Some late_neg >= early_pos") 
  }
  testDates(rtdat)

  # Make for split episodes later rather than in loop to save time
  rtdat <- rename(rtdat, obs_start = early_neg)
  vars <- c("obs_start", "late_neg", "early_pos")
  rtdat[vars] <- lapply(rtdat[vars], as.Date, origin="1970-01-01")
  rtdat
}

