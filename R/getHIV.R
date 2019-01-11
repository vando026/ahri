
#' @title readHIVData
#' 
#' @description  Reads in AHRI data from csv file
#' 
#' @param inFiles file paths from \code{\link{setArgs}}.
#' @param dropTasP drop TasP surveillance areas from the data. 
#' 
#' @return data.frame
#'
#' @import dplyr
#' @export 

readHIVData <- function(
  inFiles=getFiles(), dropTasP=TRUE) {
  hiv <- readr::read_csv(inFiles$hivfile,
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
  if (dropTasP==TRUE) 
    hiv <- dropTasPData(hiv, inFile=inFiles$pipfile)
  hiv
}

#' @title getHIV
#' 
#' @description  get all valid test results from HIV surveillance.
#' 
#' @param Args requires Args, see \code{\link{setArgs}}.
#'
#' @return data.frame
#'
#' @import dplyr 
#'
#' @export 

getHIV <- function(Args) {
  hiv <- readHIVData(Args$inFiles, dropTasP=TRUE)
  # Only deal with valid test results
  hiv <- filter(hiv, HIVResult %in% c(0,1))
  hiv <- mutate(hiv, 
    HIVNegative = ifelse(HIVResult==0, VisitDate, NA), 
    HIVPositive = ifelse(HIVResult==1, VisitDate, NA))
  hiv <- mutate(hiv, Year=as.integer(format(VisitDate, "%Y")))
  Vars <- c("HIVNegative", "HIVPositive")
  hiv[Vars] <- lapply(hiv[Vars], as.Date, origin="1970-01-01")
  hiv 
}

