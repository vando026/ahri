#' @title readEpisodes
#' 
#' @description  Reads in new Episodes dta dataset which replaces the Demography dataset (for
#' 2017) and converts it to a .Rdata file
#' 
#' @param inFile File path to the dataset, default is set to \code{\link{getFiles}}
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
  inFile=Args$inFiles$epi_dta,
  outFile=Args$inFiles$epifile) {
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
#' @description  Loads Episodes .Rdata into memory (see \code{\link{readEpisodes}}
#' 
#' @param inFile File path to the dataset, default is set to \code{\link{getFiles}}
#' 
#' @return data.frame
#'
#' @export 
#'
#' @examples
#' Args <- setArgs()
#' getEpisodes()

getEpisodes <- function(inFile=Args$inFiles$epifile) {
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


#' @title getBirthDate
#' 
#' @description  gets birth dates from \code{\link{getEpisodes}} data
#' 
#' @param inFile File path to the dataset, default is set to \code{\link{getFiles}}
#'
#' @param addVars string for a regular expression to select additional vars
#' 
#' @return data.frame
#'
#' @import dplyr
#'
#' @export 
#'
#' @examples
#' Args <- setArgs()
#' getBirthDate(addVars="Female)

getBirthDate <- function(
  inFile=Args$inFile$epifile, 
  addVars=" ") {
  dat <- getEpisodes(inFile) 
  dat <- select(dat, IIntID, DateOfBirth=DoB, contains(addVars))
  dat <- distinct(dat, IIntID, .keep_all=TRUE)
  dat <- filter(dat, as.numeric(format(DateOfBirth, "%Y")) > 1910)
  dat
}
