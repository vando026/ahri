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

getARTDates <- function(inFile=getFiles()$epifile) {
  dat <- getEpisodes(inFile)
  dat <- select(dat, IIntID, DateOfInitiation=EarliestARTInitDate)
  dat <- filter(dat, !is.na(DateOfInitiation))
  dat <- distinct(dat, IIntID, .keep_all=TRUE)
  dat <- mutate(dat,
    YearOfInitiation = as.numeric(format(DateOfInitiation, "%Y")),
    MonthART = as.numeric(format(DateOfInitiation, "%m")))
  dat
}

#' @title calcARTCov
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

calcARTCov <- function(Args, 
  Formula="OnART ~ Year",
  cutoff=9) {
  # Get HIV+ data only
  hdat <- getHIV(Args)
  hpos <- filter(hdat, HIVResult==1) %>% 
    select(IIntID, Year, Female, AgeAtVisit, VisitDate, HIVResult) 
  # Merge with ART data
  art <- getARTDates(Args$inFiles$epifile)
  adat <- left_join(hpos, art, by="IIntID")
  adat <- arrange(adat, IIntID, Year) 
  adat <- group_by(adat, IIntID) %>% 
    mutate(OnART = as.numeric(!(Year < YearOfInitiation | is.na(YearOfInitiation))))
  # Ok if month of Init is after cutoff, dont assign OnART to that year
  adat <- mutate(adat, OnART =
    ifelse((YearOfInitiation==Year) & (MonthART >= cutoff) & !is.na(MonthART), 0, OnART))
  adat <- setData(adat, Args)
  out <- calcTrend(adat, Formula=Formula)
  out
}

