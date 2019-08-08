#' @title readEpisodes
#' 
#' @description  Reads in new Episodes dta dataset which replaces the Demography dataset (for
#' 2017) and converts it to a .Rdata file.
#' 
#' @param inFile File path to the dataset, default is set to \code{\link{getFiles}}.
#' 
#' @return data.frame
#'
#' @importFrom haven read_dta
#'
#' @export 
#'
#' @examples
#' Args <- setArgs()
#' readEpisodes()

readEpisodes <- function(
  inFile=getFiles()$epi_dta,
  outFile=getFiles()$epifile, Vars=" ") {
  #
  dat <- haven::read_dta(inFile) 
  dat <- select(dat,
    IIntID=IndividualId, BSIntID=LocationId, 
    Year, ExpDays=Days,
    ObservationStart=StartDate,
    ObservationEnd=EndDate,
    Female=Sex, Age, Resident,
    EarliestARTInitDate, DoB, DoD, matches(Vars))
  dat <- haven::zap_labels(dat)
  dat <- haven::zap_formats(dat)
  dat <- filter(dat, Female %in% c(1, 2))
  dat <- mutate(dat, 
    Female=as.numeric(Female==2),
    IIntID=as.integer(IIntID),
    BSIntID=as.integer(BSIntID),
    Year=as.integer(Year))
  dat <- arrange(dat, IIntID, ObservationStart)
  attributes(dat$BSIntID) <- NULL
  save(dat, file=file.path(outFile))
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
#'
#' @examples
#' Args <- setArgs()
#' getEpisodes(getFiles$epifile)

getEpisodes <- function(inFile=getFiles()$epifile) {
  load(inFile, envir=environment())
  dat
}

#' @title dropTasPData
#' 
#' @description  Function to drop individuals who tested in TasP areas.
#' 
#' @param dat A dataset.
#' 
#' @return data.frame
#'
#' @export 

dropTasPData <- function(dat, inFile=getFiles()$pipfile) {
  pipdat <- readPIPData(inFile)
  pipdat <- select(pipdat, BSIntID, PIPSA)
  dat <- left_join(dat, pipdat, by="BSIntID")
  # keep if miss BS prior to 2017
  dat <- filter(dat, PIPSA %in% c("S", NA)) 
  # drop if NA in 2017
  # dat <- filter(dat, !(is.na(PIPSA) & Year==2017)) 
  dat <- select(dat, -c(PIPSA))
  comment(dat) <- "Note: This dataset drops HIV tests from TasP (and NA) areas in 2017"
  dat
}

#' @title setEpisodes
#' 
#' @description  set episodes data according to Args and drops TasP Areas if needed.
#' 
#' @param Args requires Args, see \code{\link{setArgs}}.
#' @param dropTasP default is to drop TasP areas.
#' 
#' @return data.frame
#'
#' @export 
#'
#' @examples
#' setEpisodes(setArgs())

setEpisodes <- function(Args=setArgs(), dropTasP=TRUE) {
  dat <- getEpisodes()
  dat <- setData(dat, Args)
  if (dropTasP==TRUE) 
    dat <- dropTasPData(dat, getFiles()$pipfile)
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
  dat <- setEpisodes(Args, dropTasP=TRUE)
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
  dat <- setEpisodes(Args, dropTasP=TRUE)
  gdat <- group_by(dat, IIntID, Year) %>% 
    summarize(Perc = sum(ExpDays)/366)
  dat <- left_join(dat, gdat, by=c("IIntID", "Year"))
  dat <- filter(dat, Perc>=prop)
  dat
}


