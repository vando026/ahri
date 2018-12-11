#' @title getRTData
#' 
#' @description  Get all repeat testers from HIV surveillance.
#' 
#' @param dat dataset from \code{\link{getHIV}}. 
#'
#' @param Args takes an Args list from \code{\link{setArgs}}. 
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
  # function
  getDates <- function(dat, f) {
    function(Var, Name) {
      dat <- data.frame(dat[!is.na(dat[, Var]), c("IIntID", Var)])
      dates <- tapply(dat[, Var], dat[, "IIntID"], f)
      # names(out)
      out <- data.frame(as.integer(names(dates)), dates)
      colnames(out) <- c("IIntID", Name)
      out
    }
  }
  getDatesMin <- getDates(hiv, min)
  getDatesMax <- getDates(hiv, max)

  # get dates 
  early_neg <- getDatesMin("HIVNegative", "early_neg")
  early_pos <- getDatesMin("HIVPositive", "early_pos")
  late_neg <- getDatesMax("HIVNegative", "late_neg")
  late_pos <- getDatesMax("HIVPositive", "late_pos")

  # merge
  dat <- select(dat, IIntID, Female) %>% 
    group_by(IIntID) %>% slice(1) %>% ungroup(dat)
  dat <- left_join(dat, early_neg, by="IIntID")
  dat <- left_join(dat, late_neg, by="IIntID")
  dat <- left_join(dat, early_pos, by="IIntID")
  dat <- left_join(dat, late_pos, by="IIntID")

  # We have LatestNegativeDate after EarliestHIVPositive. 02May2016:  101 individuals
  rtdat <- mutate(dat, late_neg_after = ifelse(
    (late_neg > early_pos) & is.finite(early_pos) & is.finite(late_neg), 1, 0)) 
  # ** I just drop these individuals, irreconcilable
  rtdat <- filter(rtdat, late_neg_after==0) %>% 
    select(-c(late_neg_after, late_pos))
  # Drop any indiv that dont have a first neg date.
  rtdat <- filter(rtdat, !(is.na(early_neg) & is.na(late_neg)))
  # Must have two tests, if early neg date is equal to late neg date and missing pos date then drop
  rtdat <- filter(rtdat, !(early_neg==late_neg & is.na(early_pos)))
  rtdat <- mutate(rtdat, sero_event = ifelse(is.finite(early_pos), 1, 0))
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

