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
  dat <- filter(dat, !is.na(EarliestARTInitDate))
  dat <- group_by(dat, IIntID) %>% 
    summarize(DateOfInitiation=min(EarliestARTInitDate))
  dat <- mutate(dat,
    YearOfInitiation = as.numeric(format(DateOfInitiation, "%Y")),
    MonthART = as.numeric(format(DateOfInitiation, "%m")))
  dat
}


#' @title getHIV_ART
#' 
#' @description  Get HIV and ART data.
#' 
#' @param Args  arguments from \code{\link{setArgs}}.
#' 
#' @return data.frame
#'
#' @export 

getHIV_ART <- function(Args, cutoff=9) {
  # Get HIV+ data only
  hdat <- getHIV()
  hpos <- filter(hdat, HIVResult==1) %>% 
    select(IIntID, Year, Female, AgeAtVisit, VisitDate, HIVResult) 
  # Merge with ART data
  art <- getARTDates()
  adat <- left_join(hpos, art, by="IIntID")
  adat <- arrange(adat, IIntID, Year) 
  adat <- group_by(adat, IIntID) %>% 
    mutate(OnART = as.numeric(!(Year < YearOfInitiation | is.na(YearOfInitiation))))
  # Ok if month of Init is after cutoff, dont assign OnART to that year
  adat <- mutate(adat, OnART =
    ifelse((YearOfInitiation==Year) & (MonthART >= cutoff) & !is.na(MonthART), 0, OnART))
  setData(adat, Args)
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
#' @return data.frame
#'
#' @export

calcARTCov <- function(Args) {
  dat <- getHIV_ART(Args)
  calcTrendYear("OnART", dat)
}


#' @title readARTCov
#' 
#' @description  Read in community ART coverage data from Source path
#' 
#' @param Female Read female data (Female = 1) or male data (Female = 0)
#' 
#' @return 
#'
#' @export 

readARTCov <- function(Female=1) {
  sex <- ifelse(Female==1, "fem_art", "mal_art")
  art <- suppressMessages(readr::read_csv(
    unlist(getFiles()[sex]), na=c("", "-")))
  art <- tidyr::gather(art, Year, ARTCov, -BSIntID)
  art <- filter(art, !is.na(ARTCov))
  art <- suppressWarnings(mutate(art, 
    Year=as.integer(gsub("[MF]_ART_", "", Year)),
    ARTCov = as.numeric(ARTCov)*100))
  art
}


#' @title addARTCov
#' 
#' @description  Add community ART cov. 
#' 
#' @param dat Dataset to add ART vars to. 
#' 
#' @return 
#'
#' @export 

addARTCov <- function(dat, Args, oppSex=TRUE) {
  Sex <- ifelse(oppSex, as.numeric(!Args$Fem), Args$Fem)
  art <- readARTCov(Sex)
  dat <- left_join(dat, art, by=c("BSIntID", "Year"))
  dat
}




