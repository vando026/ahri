#' @title readEpisodes
#' 
#' @description  Reads in the Surveillance Episodes dataset 
#' 
#' @param inFile File path to the .dta dataset, default is set to \code{\link{setFiles}}.
#' @param outFile File path to the write the .Rda dataset, default is set to \code{\link{setFiles}}.
#' @param dropTasP default is to drop TasP areas.
#' @param addVars A regular expression string representing the variables to be added. 
#' @param write_rda Default is to write the .Rda file.
#' 
#' @return data.frame
#'
#' @import dplyr
#' @export 
#'
#' @examples
#' readEpisodes(addVars="CurrentlyEmployed|UnEmployment")
#' readEpisodes(dropTasP=FALSE, addVars="Employ")

readEpisodes <- function(
  inFile=NULL, outFile=NULL, 
  dropTasP=TRUE, addVars=" ",
  write_rda=TRUE) {
  #
  if (is.null(inFile)) {
    check_getFiles()
    inFile=getFiles()$epifile
  }
  if(is.null(outFile)) {
    check_getFiles()
    outFile=getFiles()$epi_rda
  }
  dat <- haven::read_dta(inFile) 
  dat <- select(dat,
    IIntID=IndividualId, BSIntID=LocationId, 
    Female=Sex, Age,  DoB, DoD,
    Year,ExpDays=Days,
    ObservationStart=StartDate,
    ObservationEnd=EndDate,
    InMigration, OutMigration,
    Resident, AssetIndex=ModerntAssetIdx,
    EarliestARTInitDate, OnART,
    matches(addVars))
  dat <- filter(dat, Female %in% c(1,2))
  dat <- mutate(dat,
    IIntID=as.integer(IIntID),
    BSIntID=as.integer(BSIntID),
    Year=as.integer(Year),
    Female=as.integer(ifelse(Female==2, 1, 0)))
  if (dropTasP==TRUE) dat <- dropTasPData(dat)
  dat <- arrange(dat, IIntID, ObservationStart)
  if (write_rda) saveRDS(dat, outFile)
  dat
}

#' @title getEpisodes
#' 
#' @description  Loads Episodes .Rda into memory, see \code{\link{readEpisodes}}.
#' 
#' @param inFile File path to the dataset, default is set to \code{getFiles()$epi_rda}.
#' 
#' @return data.frame
#'
#' @export 
getEpisodes <- function(inFile=NULL) {
  if (is.null(inFile)) {
    check_getFiles()
    inFile=getFiles()$epi_rda
  }
  readRDS(inFile)
}


#' @title setEpisodes
#' 
#' @description  Set the Episodes data according to Arguments.
#' 
#' @param Args requires Args, see \code{\link{setArgs}}.
#' @param dat A dataset generated from \code{\link{readEpisodes}}, which exists in the
#' global environment. If NULL, it reads in the corresponding .Rda file (see
#' \code{\link{setFiles}}.  
#' 
#' @return data.frame
#'
#' @export 
#'
#' @examples
#' Args <- setArgs(Years=c(2005:2010))
#' setEpisodes(Args)
#' epidat <- readEpisodes(write_rda=FALSE)
#' setEpisodes(Args, epidat)

setEpisodes <- function(Args=setArgs(), dat=NULL) {
  if (is.null(dat)) dat <- getEpisodes()
  setData(dat, Args)
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


