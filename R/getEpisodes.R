#' @title readEpisodes
#' 
#' @description  Reads in new Episodes dta dataset which replaces the Demography dataset (for
#' 2017) and converts it to a .Rda file.
#' 
#' @param inFile File path to the dataset, default is set to \code{\link{getFiles}}.
#' @param dropTasP default is to drop TasP areas.
#' 
#' @return data.frame
#'
#' @export 
#'
#' @examples
#' Args <- setArgs()
#' readEpisodes()

readEpisodes <- function(
  inFile=getFiles()$epi_dta,
  outFile=getFiles()$epi_rda, 
  dropTasP=TRUE, Vars=" ") {
  #
  dat <- haven::read_dta(inFile) 
  dat <- select(dat,
    IIntID=IndividualId, BSIntID=LocationId, 
    Female=Sex, Age,  DoB, DoD,
    Year,ExpDays=Days,
    ObservationStart=StartDate,
    ObservationEnd=EndDate,
    InMigration, OutMigration,
    Resident, matches(Vars))
  dat <- filter(dat, Female %in% c(1,2))
  dat <- mutate(dat,
    IIntID=as.integer(IIntID),
    BSIntID=as.integer(BSIntID),
    Year=as.integer(Year),
    Female=as.integer(ifelse(Female==2, 1, 0)))
  if (dropTasP==TRUE) dat <- dropTasPData(dat)
  dat <- arrange(dat, IIntID, ObservationStart)
  saveRDS(dat, outFile)
  dat
}

#' @title getEpisodes
#' 
#' @description  Loads Episodes .Rdata into memory, see \code{\link{readEpisodes}}.
#' 
#' @param inFile File path to the dataset, default is set to \code{\link{getFiles}}.
#' 
#' @return data.frame
#'
#' @export 
getEpisodes <- function(inFile=getFiles()$epi_rda) {
  readRDS(inFile)
}


#' @title setEpisodes
#' 
#' @description  set episodes data according to Args.
#' 
#' @param Args requires Args, see \code{\link{setArgs}}.
#' 
#' @return data.frame
#'
#' @export 
#'
#' @examples
#' setEpisodes(setArgs())

setEpisodes <- function(Args=setArgs()) {
  dat <- getEpisodes()
  bdat <- getBirthDate()
  dat <- setData(dat, Args, 
    time2="ObservationStart", birthdate=bdat)
  dat
}


#' @title getDemResident
#' 
#' @description Gets the total number of residents from the Episodes dataset. 
#' 
#' @param Args requires Args, see \code{\link{setArgs}}.
#' @param  prop proportion time spent in DSA to be included in analysis.  
#' 
#' @return  data.frame
#'
#' @export 

getDemResident <- function(Args=setArgs(), prop=0.5) {
  dat <- setEpisodes(Args)
  dat <- filter(dat, Resident==1)
  gdat <- group_by(dat, IIntID, Year) %>% 
    summarize(Perc = sum(ExpDays)/366)
  dat <- left_join(dat, gdat, by=c("IIntID", "Year"))
  dat <- filter(dat, Perc>=prop)
  dat
}


#' @title getCensus
#' 
#' @description Gets a dataset that is essentially a census of surveillance area. 
#' 
#' @param Args requires Args, see \code{\link{setArgs}}.
#' 
#' @return  data.frame
#'
#' @export 

getCensus <- function(Args=setArgs(), prop=0.5) {
  dat <- setEpisodes(Args)
  gdat <- group_by(dat, IIntID, Year) %>% 
    summarize(Perc = sum(ExpDays)/366)
  dat <- left_join(dat, gdat, by=c("IIntID", "Year"))
  dat <- filter(dat, Perc>=prop)
  dat
}


