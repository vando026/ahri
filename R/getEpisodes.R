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


readEpisodes <- function(
  inFile=Args$inFiles$epi_dta,
  outFile=Args$inFiles$epifile) {
  #
  dat <- read_dta(inFile) 
  dat <- filter(dat, Sex %in% c(1, 2)) %>% 
    mutate(Female=as.numeric(Sex==2))
  dat <- select(dat,
    IIntID=IndividualId, BSIntID=LocationId, 
    ExpYear=Year, ExpDays=Days,
    ObservationStart=StartDate,
    ObservationEnd=EndDate,
    Female, Age, Resident,
    EarliestARTInitDate, DoB, DoD)
  dat <- arrange(dat, IIntID, ObservationStart)
  save(dat, file=file.path(outFile))
  gc()
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

getEpisodes <- function(inFile=Args$inFiles$epifile) {
  load(inFile, envir=environment())
  dat
}

