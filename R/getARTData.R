#' @title getARTDates
#' 
#' @description  Get ART initiation dates
#' 
#' @param dat A dataset which has a variable called \code{EarliestARTInitDate}. If
#' NULL, the function calls \code{\link{getEpisodes}} with that variable name.
#'
#' @return data.frame
#'
#' @import dplyr
#'
#' @export
#' @examples
#' getARTDates()

getARTDates <- function(dat=NULL) {
  if (is.null(dat)) dat <- getEpisodes()
  dat <- filter(dat, !is.na(.data$EarliestARTInitDate))
  dat <- distinct(dat, .data$IIntID, .data$EarliestARTInitDate, .keep_all=TRUE) %>% 
    select(IIntID, DateOfInitiation=.data$EarliestARTInitDate)
  dat <- mutate(dat,
    YearOfInitiation = as.integer(format(.data$DateOfInitiation, "%Y")),
    MonthART = as.integer(format(.data$DateOfInitiation, "%m")))
  dat
}


#' @title getEverART
#' 
#' @description  Creates an Ever on ART variable.
#' 
#' @param dat A dataset from \code{\link{getEpisodes}}.
#' 
#' @return data.frame
#' @import dplyr
#' @export 
#' @examples
#' getEverART()

getEverART <- function(dat=getEpisodes()) {
  dat <- filter(dat, !is.na(.data$EarliestHIVPos))
  dat <- select(dat, .data$IIntID,.data$Year, .data$Age, .data$Female,
    DateOfInitiation=.data$EarliestARTInitDate, .data$EarliestHIVPos, .data$OnART)
  dat <- mutate(dat, YearPos = as.integer(format(.data$EarliestHIVPos, "%Y")),
    YearOfInitiation = as.integer(format(.data$DateOfInitiation, "%Y")))
  dat <- filter(dat, !(.data$Year < .data$YearPos))
  dat <- mutate(dat,
    EverART = as.integer(!(.data$Year < .data$YearOfInitiation | is.na(.data$YearOfInitiation))))
  dat
}

#' @title calcARTCov
#' 
#' @description  Calculate ART coverage for AHRI data. (This is a very crude measure of
#' ART coverage. More work needed on an appropriate measure.)
#' 
#' @param dat A dataset from \code{\link{getEverART}}.
#' @param Args requires Args, see \code{\link{setArgs}}
#' 
#' @return data.frame
#' @export 

calcARTCov <- function(dat=getEverART(), Args=setArgs()) {
  dat <- setData(dat, Args)
  calcTrendYear("EverART", dat)
}


#' @title readARTCov
#' 
#' @description  Read in community ART coverage data from Source path
#' 
#' @param Female Read female data (Female = 1) or male data (Female = 0)
#' 
#' @return data.frame
#' @keywords internal
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
#' @description  Add community ART coverage. 
#' 
#' @param dat Dataset to add ART vars to. 
#' @param Args requires Args, see \code{\link{setArgs}}.
#' @param oppSex Make opposite-sex HIV prevalence. Default is FALSE.
#' 
#' @return data.frame
#' @keywords internal

addARTCov <- function(dat, Args, oppSex=FALSE) {
  Sex <- ifelse(oppSex, as.numeric(!Args$Fem), Args$Fem)
  art <- readARTCov(Sex)
  dat <- left_join(dat, art, by=c("BSIntID", "Year"))
  dat
}


