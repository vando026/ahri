#' @title readHIVData
#' 
#' @description  Reads in AHRI data from csv file
#' 
#' @param inFile File path to .dta, default is set by \code{\link{setFiles}}.
#' @param outFile File path to save .Rda, default is set by \code{\link{setFiles}}.
#' @param dropTasP Default is to drop TasP surveillance areas from the data. 
#' @param addVars A regular expression string representing the variables to be added. 
#' @param drop15Less Default is to drop all observations with Age < 15 years. Only
#' participants >=15 years eligible for HIV testing.
#' @param write_rda Default is to write the .Rda file.
#' 
#' @return data.frame
#'
#' @import dplyr
#' @export 
#' @examples
#' # Writes .Rda to file
#' \donttest{
#' readHIVData()
#' }
#' # Saves to global environment 
#' hdat <- readHIVData(write_rda=FALSE)
#' # Add variables
#' hdat <- readHIVData(addVars="HIVRefused|WhereLastTested", write_rda=FALSE)

readHIVData <- function(
  inFile=NULL, outFile=NULL,
  dropTasP=TRUE, addVars=" ", 
  drop15Less=TRUE, write_rda=TRUE) {
  #
  if (is.null(inFile)) {
    check_getFiles()
    inFile=getFiles()$hivfile
  }
  if(is.null(outFile)) {
    check_getFiles()
    outFile=getFiles()$hiv_rda
  }
  #
  hiv <- haven::read_dta(inFile) %>% 
    select(IIntID=IIntId, 
      BSIntID=ResidencyBSIntId, VisitDate, 
      HIVResult, Female=Sex, Age=AgeAtVisit,
      matches(addVars))
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
  if (drop15Less) hiv <- filter(hiv, Age %in% c(15:100))
  hiv <- mutate(hiv, 
    HIVNegative = ifelse(HIVResult==0, VisitDate, NA), 
    HIVPositive = ifelse(HIVResult==1, VisitDate, NA))
  hiv <- mutate(hiv, Year=as.integer(format(VisitDate, "%Y")))
  Vars <- c("HIVNegative", "HIVPositive")
  hiv[Vars] <- lapply(hiv[Vars], as.Date, origin="1970-01-01")
  if (write_rda) saveRDS(hiv, file=outFile)
  hiv
}

#' @title getHIV
#' 
#' @description  Load in the .rda version of the HIV surveillance dataset. 
#' 
#' @param inFile File path to .rda dataset, default is \code{getFiles()$hiv_rda}. Leave as
#' NULL if you don't know what to do or see \code{\link{setFiles}}. 
#'
#' @return data.frame
#'
#' @export 

getHIV <- function(inFile=NULL) {
  if (is.null(inFile)) {
    check_getFiles()
    inFile=getFiles()$hiv_rda
  }
  readRDS(file=inFile)
}

#' @title setHIV
#' 
#' @description set HIV data by arguments.
#' 
#' @param Args requires Args, see \code{\link{setArgs}}.
#' 
#' @param dat A dataset generated from \code{\link{readHIVData}}, which exists in the
#' global environment. If NULL, it reads in the .Rda file from
#' \code{getFiles()$hiv_rda}.  
#' 
#' @return data.frame
#'
#' @export 
#' @examples
#' Args <- setArgs() 
#' set_hiv <- setHIV(Args)
#' # Pass in existing data as an argument
#' hdat <- readHIVData(write_rda=FALSE)
#' hdat1 <- setHIV(Args, dat=hdat)
setHIV <- function(Args=setArgs(), dat=NULL) {
  if (is.null(dat)) dat <- getHIV()
  setData(dat, Args)
}

