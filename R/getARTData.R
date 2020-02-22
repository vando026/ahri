#' @title getARTDates
#' 
#' @description  Get ART initiation dates
#' 
#' @param dat A dataset from \code{\link{getEpisodes}} which has the ART variables
#'
#' @return data.frame
#'
#' @import dplyr
#'
#' @export
#' @examples
#' getARTDates()

getARTDates <- function(dat) {
  dat <- readEpisodes(Vars="ART", write_rda=FALSE)
  dat <- filter(dat, !is.na(EarliestARTInitDate))
  dat <- distinct(dat, IIntID, EarliestARTInitDate, .keep_all=TRUE) %>% 
    select(IIntID, DateOfInitiation=EarliestARTInitDate)
  dat <- mutate(dat,
    YearOfInitiation = as.integer(format(DateOfInitiation, "%Y")),
    MonthART = as.integer(format(DateOfInitiation, "%m")))
  dat
}


#' @title getOnART
#' 
#' @description  Creates an OnART dataset and OnART2 variable using a different method
#' than used for the OnART variable in the Episodes dataset.
#' 
#' @param cutoff Value from 1 and 13, if ART initiation is after this value then no ART
#' usage for that entire year. Use cutoff=13 to ignore this argument.
#' 
#' @return data.frame
#'
#' @export 

getOnART <- function(cutoff=13) {
  dat <- readEpisodes(Vars="ART|EarliestHIVPos", write_rda=FALSE)
  dat <- distinct(dat, IIntID, Year, .keep_all=TRUE)
  dat <- filter(dat, !is.na(EarliestHIVPos))
  dat <- select(dat, IIntID, Year, Age, Female,
    DateOfInitiation=EarliestARTInitDate, EarliestHIVPos, OnART)
  dat <- mutate(dat, YearPos = as.integer(format(EarliestHIVPos, "%Y")),
    YearOfInitiation = as.integer(format(DateOfInitiation, "%Y")))
  dat <- filter(dat, !(Year < YearPos))
  dat <- mutate(dat, OnART2 = as.integer(!(Year < YearOfInitiation | is.na(YearOfInitiation))))
  dat
}

#' @title calcARTCov
#' 
#' @description  Calculate ART coverage for AHRI data.
#' 
#' @param Args requires Args, see \code{\link{setArgs}}
#' @param cutoff If after month, ART for that year doesn't count, default is 13 for ART in
#' any month.
#' 
#' @return data.frame
#'
#' @export

calcARTCov <- function(Args, cutoff=13) {
  dat <- getOnART(cutoff=cutoff)
  dat <- setData(dat, Args, )
  calcTrendYear("OnART2", dat)
}


#' @title readARTCov
#' 
#' @description  Read in community ART coverage data from Source path
#' 
#' @param Female Read female data (Female = 1) or male data (Female = 0)
#' 
#' @return 

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


