#' @title readHealthData
#' 
#' @description  Reads the Women (WGH) or Men (MGH) General Health Dataset. 
#' 
#' @param Female Either a value of 0 to read in Men or 1 to read in Women.
#' @param write_rda Default is to write the .Rda file.
#' 
#' @return 
#'
#' @export 
#' @examples
#' readHealthData(Female=0)
readHealthData <- function(Female=1, write_rda=TRUE) {
  check_getFiles()
  inFile <- ifelse(Female==0, getFiles()$mghfile, getFiles()$wghfile)
  outFile <- ifelse(Female==0, getFiles()$mgh_rda, getFiles()$wgh_rda)
  dat <- haven::read_dta(inFile) %>%
    rename(IIntID=IIntId, BSIntID=ResidenceBSIntId, Age=AgeAtVisit)
  dat <- mutate(dat, 
    Year = as.integer(format(dat$VisitDate, "%Y")),
    IIntID = as.integer(IIntID),
    Female = Female)
  if (write_rda) saveRDS(dat, file=outFile) 
  dat
}

#' @title getMGH
#' 
#' @description  Reads in Men's general health data. 
#' 
#' @param inFile Filepath to .rda dataset, default is \code{getFiles()$mgh_rda}. Leave as
#' NULL if you don't know what to do or see \code{\link{setFiles}}. 
#' @return  data.frame
#'
#' @export 
getMGH <- function(inFile=NULL) {
  if(is.null(inFile)) {
    check_getFiles()
    inFile=getFiles()$mgd_rda
  }
  readRDS(inFile)
}


#' @title getWGH
#' 
#' @description  Reads in Women's general health data. 
#' 
#' @param inFile Filepath to .rda dataset, default is \code{getFiles()$wgh_rda}. Leave as
#' NULL if you don't know what to do or see \code{\link{setFiles}}. 
#' 
#' @return data.frame
#'
#' @export 
getWGH <- function(inFile=NULL) {
  if(is.null(inFile)) {
    check_getFiles()
    inFile=getFiles()$wgd_rda
  }
  readRDS(inFile)
}



#' @title getCircumcisionData
#' 
#' @description  Gets circumcision data from MGH dataset. 
#' 
#' @export
getCircumcisionData <- function() {
  dat <- getMGH() 
  dat <- filter(dat, IsCircumcised %in% c(1, 2))
  dat <- mutate(dat, IsCircumcised=as.numeric(IsCircumcised==1))
  dat <- filter(dat, IsCircumcised==1)
  dat <- group_by(dat, IIntID) %>% 
    summarize(YearCircum = min(Year)) %>% 
      mutate(EverCircum=1)
  dat
}

setCircumStatus <- function(Keep) {
  function(dat) {
    cdat <- getCircumcisionData()
    dat <- left_join(dat, cdat, by="IIntID")
    dat <- mutate(dat,
      IsCircum = as.numeric(Year >= YearCircum & !is.na(YearCircum)),
      EverCircum = as.numeric(EverCircum==1 & !is.na(EverCircum)))
    dat <- filter(dat, EverCircum %in% Keep)
    select(dat, -(YearCircum))
  }
}

#' @title getCircum
#' 
#' @description gets Circumcision data from MGH AHRI datasets.
#' 
#' @param dat Dataset to make the circumcision status variable.
#' 
#' @export
getCircum <- setCircumStatus(Keep = c(0, 1))


#' @title keepCircum
#' 
#' @description gets Circumcision data from MGH AHRI datasets.
#' 
#' @param dat Dataset to make the circumcision status variable.
#' 
#' @export
keepCircum <- setCircumStatus(Keep=1)

#' @title dropCircum
#' 
#' @description gets Circumcision data from MGH AHRI datasets.
#' 
#' @param dat Dataset to make the circumcision status variable.
#' 
#' @export
dropCircum <- setCircumStatus(Keep=0)


#' @title getCondomUseData
#' 
#' @description  gets Condom use data from AHRI datasets. File path must be set in
#' \code{\link{setFiles}}. 
#' 
#' @export

getCondomUseData <- function() {
  dat <- getWGH_MGH()
  dat <- select(dat, IIntID, Year, EverUsedCondom, Female)
  dat <- filter(dat, EverUsedCondom %in% c(1:3))
  dat <- distinct(dat, IIntID, Year, .keep_all=TRUE)
  dat
}

#' @title addCondomVar
#' 
#' @description  Adds condom variable.
#' 
#' @param dat Master dataset to be merged with condom use variable.
#' @param dropFemale Drop the Female variable. Default is TRUE.
#' 
#' @return
#'
#' @export 
addCondomVar <- function(dat, dropFemale=TRUE) {
  cdat <- getCondomUseData()
  if (dropFemale) cdat <- select(cdat, -Female)
  probs <- prop.table(table(cdat$EverUsedCondom))
  dat <- left_join(dat, cdat, by=c("IIntID", "Year"))
  # First carry forward
  dat <- group_by(dat, IIntID) %>% mutate( 
    EverUsedCondom=na.locf(EverUsedCondom, na.rm=FALSE),
    EverUsedCondom=na.locf(EverUsedCondom, na.rm=FALSE, fromLast=TRUE))
  idat <- data.frame(IIntID=unique(dat$IIntID[is.na(dat$EverUsedCondom)]))
  # Some people dont have any condom data, randomly sample
  idat <- mutate(idat, EverUsedCondom2 =
    sample(unique(dat$EverUsedCondom[!is.na(dat$EverUsedCondom)]),
    size=nrow(idat), replace=TRUE,
    prob=probs))
  dat <- left_join(dat, idat, by="IIntID")
  dat <- mutate(dat, EverUsedCondom =
    ifelse(is.na(EverUsedCondom), EverUsedCondom2, EverUsedCondom))
  dat <- mutate(dat, EverUsedCondom = 
    ifelse(EverUsedCondom==1, "Sometimes",
    ifelse(EverUsedCondom==2, "Sometimes", "Never")))
  dat
}


#' @title addCircumVar
#' 
#' @description  Adds circumcision data for ever Circumcised. 
#' 
#' @param dat An existing dataset.
#' 
#' @return 
#'
#' @export 
addCircumVar <- function(dat) {
  cdat <- getCircumcisionData()
  dat <- left_join(dat, cdat, by="IIntID")
  dat$EverCircum[is.na(dat$EverCircum)] <- 0
  dat
}

