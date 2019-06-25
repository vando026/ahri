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
  dat <- distinct(dat, IIntID, EarliestARTInitDate) %>% 
    rename(DateOfInitiation=EarliestARTInitDate)
  dat <- mutate(dat,
    YearOfInitiation = as.integer(format(DateOfInitiation, "%Y")),
    MonthART = as.integer(format(DateOfInitiation, "%m")))
  dat
}

# debugonce(getARTDates)
# xx=getARTDates()
# dx(xx$IIntID)

# edat = haven::read_dta(getFiles()$epi_dta)
# dat1 <- select(edat, IndividualId, StartDate, EarliestARTInitDate, EarliestHIVPos)
# dat1 <- filter(dat1, !is.na(EarliestHIVPos))
# dx(dat1$IndividualId)
# dat2 <- filter(dat1, !is.na(EarliestARTInitDate))
# dx(dat2$IndividualId)
# debugonce(getOnART)
# getOnART()


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
  # Get resident episodes
  edat <- getEpisodes()
  # Get HIV+ data only
  early_pos <- getDatesMin(getHIV(), "HIVPositive", "early_pos")
  early_pos <- mutate(early_pos, early_pos=as.Date(early_pos),
    YearPos = as.numeric(format(early_pos, "%Y")))
  hdat <- left_join(edat, early_pos, by="IIntID") %>%
    filter(!is.na(YearPos))
  hdat <- filter(hdat, !(Year < YearPos))
  # Merge with ART data
  art <- getARTDates()
  adat <- left_join(hdat, art, by="IIntID")
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
#' \code{\link{setData}}.
#' 
#' @return data.frame
#'
#' @export

calcARTCov <- function(f, cutoff=13) {
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
    Year=as.integer(gsub("[MF]_ART_", "", Year)),
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




