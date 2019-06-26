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
  # replace if ARTInit & replace if ARTInit < HIVPos
  dat <- mutate(dat, EarliestHIVPos2 = 
    ifelse(is.na(dat$EarliestHIVPos) & !is.na(dat$EarliestARTInitDate),
      EarliestARTInitDate, EarliestHIVPos),
    EarliestHIVPos2 = ifelse(EarliestARTInitDate <  EarliestHIVPos2,
      EarliestARTInitDate, EarliestHIVPos2),
    EarliestHIVPos2 = as.Date(EarliestHIVPos2))
  dat <- filter(dat, !is.na(EarliestARTInitDate))
  dat <- distinct(dat, IIntID, EarliestARTInitDate, .keep_all=TRUE) %>% 
    select(IIntID, EarliestHIVPos2, DateOfInitiation=EarliestARTInitDate)
  dat <- mutate(dat,
    YearOfInitiation = as.integer(format(DateOfInitiation, "%Y")),
    MonthART = as.integer(format(DateOfInitiation, "%m")))
  dat
}


#' @title getOnART
#' 
#' @description  Get HIV and ART data.
#' 
#' @param cutoff Value from 1 and 13, if ART initiation is after this value then no ART
#' usage for that entire year. Use cutoff=13 to ignore this argument.
#' 
#' @return data.frame
#'
#' @export 

getOnART <- function(cutoff=13) {
  edat <- getEpisodes()
  art <- getARTDates()
  adat <- left_join(edat, art, by="IIntID")
  adat <- filter(adat, !is.na(EarliestHIVPos2))
  adat <- mutate(adat, YearPos = as.integer(format(EarliestHIVPos2, "%Y")))
  adat <- filter(adat, !(Year < YearPos))
  adat <- mutate(adat, OnART = as.integer(!(Year < YearOfInitiation | is.na(YearOfInitiation))))
  # If month of Init is after cutoff, dont assign OnART to that year
  # adat <- mutate(adat, OnART =
    # ifelse((YearOfInitiation==Year) & (MonthART >= cutoff) & !is.na(MonthART), 0, OnART))
  adat <- select(adat, IIntID, Year, Female, AgeAtVisit, OnART)
  distinct(adat, IIntID, Year, .keep_all=TRUE)
}

#' @title calcARTCov
#' 
#' @description  Calculate ART coverage for AHRI data.
#' 
#' @param f  Use function to perform further operation on data, typically 
#' \code{\link{setData}}. Default is \code{identity}. 
#' 
#' @return data.frame
#'
#' @export

calcARTCov <- function(f=identity, cutoff=13) {
  dat <- getOnART(cutoff=cutoff)
  dat <- f(dat)
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
    Year=as.integer(gsub("[MF]_ART_|All_ART_", "", Year)),
    ARTCov = as.numeric(ARTCov)*100))
  art
}


#' @title addARTCov
#' 
#' @description  Add community ART cov. 
#' 
#' @param dat Dataset to add ART vars to. 
#' @param Args requires Args, see \code{\link{setArgs}}
#' @param oppSex Make opposite-sex HIV prevalence. Default is FALSE.
#' 
#' @return 
#'
#' @export 

addARTCov <- function(dat, Args, oppSex=FALSE) {
  Sex <- ifelse(oppSex, as.numeric(!Args$Fem), Args$Fem)
  art <- readARTCov(Sex)
  dat <- left_join(dat, art, by=c("BSIntID", "Year"))
  dat
}




