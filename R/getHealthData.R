#' @title readHealthData
#' 
#' @description  Reads the Women (WGH) or Men (MGH) General Health Dataset. 
#' 
#' @param Female Either a value of 0 to read in Men or 1 to read in Women.
#' @param write_rda Default is to write the .Rda file.
#' 
#' @return data.frame
#'
#' @export 
#' @examples
#' \donttest{
#' readHealthData(Female=0)
#' }
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
    inFile <- getFiles()$mgh_rda
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
    inFile=getFiles()$wgh_rda
  }
  readRDS(inFile)
}


#' @title Gets circumcision data from the MGH dataset 
#' 
#' @description  Gets circumcision data from the MGH dataset with the year of
#' circumcision and an indicator of ever circumcised.
#' @param dat A dataset from \code{\link{getMGH}}.
#' @export
#' @examples
#' gdat <- getCircumcisionData()
getCircumcisionData <- function(dat=getMGH()) {
  dat <- filter(dat, IsCircumcised %in% c(1, 2))
  dat <- mutate(dat, IsCircumcised=as.numeric(.data$IsCircumcised==1))
  dat <- filter(dat, .data$IsCircumcised==1)
  dat <- group_by(dat, .data$IIntID) %>% 
    summarize(YearCircum = min(.data$Year)) %>% 
      mutate(EverCircum=1)
  dat
}


#' @title addCircumVar
#' 
#' @description  Adds the ever circumcised variable to an existing dataset. 
#' 
#' @param dat An existing dataset.
#' @param cdat Circumcision dataset from \code{\link{getCircumcisionData}}. 
#' 
#' @return 
#'
#' @export 
addCircumVar <- function(dat, cdat=NULL) {
  if (is.null(cdat)) cdat <- getCircumcisionData()
  cdat <- rename(cdat, Year = YearCircum)
  dat <- left_join(dat, cdat, by=c("IIntID", "Year"))
  dat <- group_by(dat, .data$IIntID) %>% mutate( 
    EverCircum=zoo::na.locf(.data$EverCircum, na.rm=FALSE))
  dat$EverCircum[is.na(dat$EverCircum)] <- 0
  dat
}


#' @title getCondomUseVar
#' 
#' @description  Gets the ever used condom variable from Men and Women's health datasets.
#' 
#' @export

getCondomUseVar <- function() {
  wdat <- getWGH() %>% 
    select(IIntID, Female,  Year, EverUsedCondom = MRPEverUsedCondoms )
  mdat <- getMGH() %>% 
    select(IIntID, Female, Year, EverUsedCondom = MRPEverUsedCondoms )
  dat <- rbind(mdat, wdat)
  dat <- filter(dat, EverUsedCondom %in% c(1:3))
  dat$EverUsedCondom <- as.character(haven::as_factor(dat$EverUsedCondom)) 
  dat <- distinct(dat, IIntID, Year, .keep_all=TRUE)
  dat
}


