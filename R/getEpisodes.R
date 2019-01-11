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
  outFile=getFiles()$epifile) {
  #
  dat <- read_dta(inFile) 
  dat <- select(dat,
    IIntID=IndividualId, BSIntID=LocationId, 
    ExpYear=Year, ExpDays=Days,
    ObservationStart=StartDate,
    ObservationEnd=EndDate,
    Female=Sex, Age, Resident,
    EarliestARTInitDate, DoB, DoD)
  dat <- filter(dat, Female %in% c(1, 2))
  dat <- mutate(dat, 
    Female=as.numeric(Female==2),
    IIntID=as.integer(IIntID))
  dat <- arrange(dat, IIntID, ObservationStart)
  save(dat, file=file.path(outFile))
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
#' getEpisodes(Args$inFile$epifile)

getEpisodes <- function(inFile=getFiles()$epifile) {
  load(inFile, envir=environment())
  dat
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
#' Args <- setArgs()
#' setEpisodes(Args)

setEpisodes <- function(Args) {
  dat <- getEpisodes(Args$inFiles$epifile)
  dat <- filter(dat, ExpYear %in% Args$Years)
  dat
}


