#' @title readHIVData
#' 
#' @description  Reads in AHRI data from csv file
#' 
#' @param inFiles File path from \code{\link{getFiles()$hivfile}}.
#' @param dropTasP Drop TasP surveillance areas from the data. 
#' 
#' @return data.frame
#'
#' @import dplyr
#' @export 

readHIVData <- function(
  inFile=getFiles()$hivfile, dropTasP=TRUE) {
  hiv <- readr::read_csv(inFile,
    col_types=cols_only(
      ResidencyBSIntId="i",
      IIntId="i",
      VisitDate="D",
      HIVResult="i",
      Sex="i",
      AgeAtVisit="i"))
  hiv <- filter(hiv, Sex %in% c(1,2))
  hiv <- mutate(hiv,
    Female=as.integer(ifelse(Sex==2, 1, 0)),
    Year = format(VisitDate, "%Y"))
  hiv <- rename(hiv, IIntID=IIntId, BSIntID=ResidencyBSIntId) %>% 
   select(-Sex) %>% arrange(IIntID, VisitDate)
  if (dropTasP) hiv <- dropTasPData(hiv)
  hiv
}

#' @title getHIV
#' 
#' @description  Get all valid test results >15 yrs age from HIV surveillance.
#' 
#' @param inFiles File path from \code{\link{getFiles()$hivfile}}.
#'
#' @return data.frame
#'
#' @import dplyr 
#'
#' @export 

getHIV <- function(inFile=getFiles()$hivfile) {
  hiv <- readHIVData(inFile)
  # Only deal with valid test results
  hiv <- filter(hiv, HIVResult %in% c(0,1))
  hiv <- filter(hiv, AgeAtVisit %in% c(15:100))
  hiv <- mutate(hiv, 
    HIVNegative = ifelse(HIVResult==0, VisitDate, NA), 
    HIVPositive = ifelse(HIVResult==1, VisitDate, NA))
  hiv <- mutate(hiv, Year=as.integer(format(VisitDate, "%Y")))
  Vars <- c("HIVNegative", "HIVPositive")
  hiv[Vars] <- lapply(hiv[Vars], as.Date, origin="1970-01-01")
  hiv 
}

#' @title setHIV
#' 
#' @description set HIV data according parameters of Args. 
#' 
#' @param Args requires Args, see \code{\link{setArgs}}.
#' 
#' @return 
#'
#' @export 
setHIV <- function(Args) {
  dat <- getHIV()
  setData(dat, Args)
}

