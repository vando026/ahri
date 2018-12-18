#' @title getARTDates
#' 
#' @description  get ART initiation dates
#' 
#' @param inFile path to data, typically \code{Args$inFile}. 
#'
#' @return data.frame
#'
#' @import dplyr
#'
#' @export

getARTDates <- function(inFile=Args$inFiles$epifile) {
  dat <- getEpisodes(inFile)
  dat <- select(dat, IIntID, DateOfInitiation=EarliestARTInitDate)
  dat <- filter(dat, !is.na(DateOfInitiation))
  dat <- distinct(dat, IIntID, .keep_all=TRUE)
  dat <- mutate(dat,
    YearOfInitiation = as.numeric(format(DateOfInitiation, "%Y")),
    MonthART = as.numeric(format(DateOfInitiation, "%m")))
  dat
}

#' @title ARTCov
#' 
#' @description  Calculate ART coverage for AHRI data.
#' 
#' @param Args  arguments from \code{\link{setArgs}}.
#'
#' @param cutoff value from 1 and 12, if ART initiation is after this value then no ART
#' usage for that entire year. Use cutoff=12 to ignore this argument.
#' 
#' @param calcBy string variable to calc the estimates by
#' 
#' @param fmt format to percentage and round to two decimal places
#'
#' @return data.frame
#'
#' @import dplyr
#' 
#' @import epitools 
#' 
#' @export


ARTCov <- function(
  Args, wdat=NULL, 
  Formula="OnART ~ Year + Female + AgeCat",
  stpopVar="Total", calcBy=c("Year", "Female"),
  mergeVars="AgeCat",
  binom=FALSE, cutoff=9, fmt=TRUE) {

  # Get HIV data 
  hdat <- getHIV(Args)
  getDatesMax <- getDates(hdat, max)
  earlyPos <- getDatesMax("HIVPositive", "early_pos")

  hpos <- filter(hdat, HIVResult==1) %>% 
    select(IIntID, Year, Female, VisitDate, HIVResult) 
  
  art <- getARTDates(Args$inFiles$epifile)

  # Merge with ART data
  adat <- left_join(hpos, art, by="IIntID")
  adat <- arrange(adat, IIntID, Year) 
  adat <- group_by(adat, IIntID) %>% 
    mutate(OnARTYear = ifelse(Year >= YearART, 1, 0))
  adat <- mutate(adat, OnARTYear = ifelse(is.na(OnARTYear), 0, OnARTYear))
  # Ok if month of Init is after cutoff, dont assign OnART to that year
  adat <- mutate(adat, OnART =
    ifelse((YearART==Year) & MonthART > cutoff & !is.na(MonthART), 0, OnARTYear))

  sdat <- calcTrend(adat, wdat=wdat, Formula=Formula,
    mergeVars=mergeVars, calcBy=calcBy, binom=binom, fmt=fmt)
  sdat 
}

