#' @title getARTDates
#' 
#' @description  get ART initiation dates
#' 
#' @param inFile path to data which is typically set using \code{\link{inFiles}}
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
    YearOfInitiation = as.numeric(format(DateOfInitiation, "%Y")))
  dat
}

#' @title ARTCov
#' 
#' @description  Calculate ART coverage for AHRI data. ART coverage can only be calculated
#' up to 2012, so new arguments need to be set.
#' 
#' @param Args  arguments from \code{\link{setArgs}}.
#'
#' @param wdat weights, most likely from \code{\link{getWeightsKZN}}.
#'
#' @param cutoff value from 1 and 12, if ART initiation is after this value then no ART
#' usage for that entire year. Use cutoff=12 to ignore this argument.
#' 
#' @param stpopVar name of var from \code{wdat} with weights. 
#' 
#' @param calcBy string variable to calc the estimates by
#' 
#' @param fmt format to percentage and round to two decimal places
#'
#' @return data.frame
#'
#' @import dplyr
#' 
#' @importFrom epitools binom.exact
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
  hpos <- filter(hdat, HIVResult==1) %>% 
    select(IIntID, Year, Female, AgeCat, HIVResult) 

  art <- getARTDates(Args$inFiles$epifile)
  art <- select(art, IIntID, DateOfInitiation)
  art <- filter(art, !is.na(DateOfInitiation)) 
  art <- mutate(art, 
    YearART = as.numeric(format(DateOfInitiation, "%Y")),
    MonthART = as.numeric(format(DateOfInitiation, "%m")))
  art <- filter(art, YearART %in% Args$Years)

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

