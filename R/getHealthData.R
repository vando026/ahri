#' @title readHealthData
#' 
#' @description  Reads in WGH/MGH data. 
#' 
#' @param inFile Filepath to dataset, default is \code{getFiles()$mghfile}.
#' @param outFile Filepath to dataset, default is \code{getFiles()$mgh_rda}.
#' @param Fem either a 0 or 1 to represent MGH or WGH dataset
#' 
#' @return 
#'
#' @export 
#' @examples
#' readHealthData(getFiles()$mghfile, getFiles()$mgh_rda, Fem=0)
readHealthData <- function(inFile, outFile, Fem) {
    dat <- haven::read_dta(inFile) %>%
      select(IIntID=IIntId, VisitDate,
      Age=AgeAtVisit, EverUsedCondom=MRPEverUsedCondoms, 
      Marital=CurrentMaritalStatus,
      Partner12=PartnersInLastTwelveMonths,
      matches("^IsCircumcised$"))
    dat <- haven::zap_labels(dat)
    dat <- haven::zap_formats(dat)
    dat <- mutate(dat, 
      Year = as.integer(format(dat$VisitDate, "%Y")),
      IIntID = as.integer(IIntID),
      Female = Fem)
    saveRDS(dat, outFile)
    dat
}

#' @title getMGH
#' 
#' @description  Reads in men general health data. 
#' 
#' @param inFile Filepath to dataset, default is \code{getFiles()$mgh_rda}.
#' 
#' @return 
#'
#' @export 
#' @examples
#' readHealthData(getFiles()$mghfile, getFiles()$mgh_rda, Fem=0)
#' getMGH()
getMGH <- function(inFile=getFiles()$mgh_rda) {
  readRDS(inFile)
}


#' @title getWGH
#' 
#' @description  Reads in women general health data. 
#' 
#' @param inFile Filepath to dataset, default is \code{getFiles()$wgh_rda}.
#' 
#' @return 
#'
#' @export 
#' @examples
#' readHealthData(getFiles()$wghfile, getFiles()$wgh_rda, Fem=1)
#' getWGH()
getWGH <- function(inFile=getFiles()$wgh_rda) {
  readRDS(inFile)
}


#' @title getWGH_MGH
#' 
#' @description  Reads in men and women general health data. 
#' 
#' @param inFile Filepath to dataset, default is \code{getFiles()$fghfile}.
#' 
#' @return 
#'
#' @export 
#' @examples
#' readHealthWomen <- readHealthData(getFiles()$wghfile)
getWGH_MGH <- function() {
  wdat <- getWGH()
  mdat <- getMGH()
  dplyr::bind_rows(wdat, mdat)
}

#' @title getCircumcisionData
#' 
#' @description  gets Circumcision data from MGH AHRI dataset. 
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
#' \code{\link{getFiles}}. 
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

