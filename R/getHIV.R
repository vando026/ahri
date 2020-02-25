#' @title readHIVData
#' 
#' @description  Reads in AHRI data from csv file
#' 
#' @param inFiles File path from \code{\link{getFiles}}.
#' @param dropTasP Drop TasP surveillance areas from the data. 
#' 
#' @return data.frame
#'
#' @import dplyr
#' @export 

readHIVData <- function(
  inFile=getFiles()$hivfile,
  outFile=getFiles()$hiv_rda,
  dropTasP=TRUE) {
  hiv <- haven::read_dta(inFile) %>% 
    select(IIntID=IIntId, BSIntID=ResidencyBSIntId, VisitDate, 
      HIVResult, Female=Sex, Age=AgeAtVisit)
  hiv <- haven::zap_labels(hiv)
  hiv <- filter(hiv, Female %in% c(1,2))
  hiv <- mutate(hiv,
    IIntID = as.integer(IIntID),
    BSIntID = as.integer(BSIntID),
    Female=as.integer(ifelse(Female==2, 1, 0)))
  hiv <- arrange(hiv, IIntID, VisitDate)
  if (dropTasP) hiv <- dropTasPData(hiv)
  # Only deal with valid test results
  hiv <- filter(hiv, HIVResult %in% c(0,1))
  hiv <- filter(hiv, Age %in% c(15:100))
  hiv <- mutate(hiv, 
    HIVNegative = ifelse(HIVResult==0, VisitDate, NA), 
    HIVPositive = ifelse(HIVResult==1, VisitDate, NA))
  hiv <- mutate(hiv, Year=as.integer(format(VisitDate, "%Y")))
  Vars <- c("HIVNegative", "HIVPositive")
  hiv[Vars] <- lapply(hiv[Vars], as.Date, origin="1970-01-01")
  saveRDS(hiv, file=outFile)
  hiv
}

#' @title getHIV
#' 
#' @description  Get all valid test results >15 yrs age from HIV surveillance.
#' 
#' @param inFiles File path from \code{\link{getFiles}}.
#'
#' @return data.frame
#'
#' @export 

getHIV <- function(inFile=getFiles()$hiv_rda) {
  readRDS(file=inFile)
}

#' @title setHIV
#' 
#' @description set HIV data according arguments of \code{\link{setArgs}}.
#' 
#' @param Args requires Args, see \code{\link{setArgs}}.
#' 
#' @return 
#'
#' @export 
setHIV <- function(Args=setArgs()) {
  dat <- getHIV()
  setData(dat, Args)
}

