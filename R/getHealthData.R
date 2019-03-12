readHealthData <- function(inFile, Fem) {
  function() {
    dat <- haven::read_dta(inFile) %>%
      select(IIntID=IIntId, VisitDate,
      AgeAtVisit, EverUsedCondom=MRPEverUsedCondoms, 
      Marital=CurrentMaritalStatus,
      Partner12=PartnersInLastTwelveMonths,
      matches("IsCircumcised"))
    dat <- haven::zap_labels(dat)
    dat <- haven::zap_formats(dat)
    dat <- mutate(dat, 
      Year = as.integer(format(dat$VisitDate, "%Y")),
      IIntID = as.integer(IIntID),
      Female = Fem)
    dat
  }
}

#' @title readHealthDataMen
#' 
#' @description  Reads in men general health data. 
#' 
#' @param inFile Filepath to dataset, default is \code{getFiles()$mghfile}.
#' 
#' @return 
#'
#' @export 
#' @examples
#' readHealthDataMen <- readHealthData(getFiles()$mghfile)
readHealthDataMen <- readHealthData(getFiles()$mghfile, Fem=0)


#' @title readHealthDataWomen
#' 
#' @description  Reads in women general health data. 
#' 
#' @param inFile Filepath to dataset, default is \code{getFiles()$fghfile}.
#' 
#' @return 
#'
#' @export 
#' @examples
#' readHealthWomen <- readHealthData(getFiles()$wghfile)
readHealthDataWomen <- readHealthData(inFile=getFiles()$wghfile, Fem=1)


#' @title getCircumcisionData
#' 
#' @description  gets Circumcision data from MGH AHRI dataset. 
#' 
#' @param inFile filepath to dataset, default is \code{getFiles()$mghfile}.
#' 
#' @export

getCircumcisionData <- function() {
  dat <- readHealthDataMen() 
  dat <- filter(dat, IsCircumcised %in% c(1, 2))
  dat <- mutate(dat, IsCircumcised=as.numeric(IsCircumcised==1))
  select(dat, IIntID, VisitDate, Year, AgeAtVisit, IsCircumcised)
}


getCircumStatus <- function(Keep) {
  function(dat) {
    cdat <- getCircumcisionData()
    cdat <- filter(cdat, IsCircumcised==1)
    cdat <- group_by(cdat, IIntID) %>% 
      summarize(YearCircum = min(Year)) %>% 
      mutate(EverCircum =1)
    dat <- as_tibble(left_join(dat, cdat, by="IIntID"))
    # No surv time before 2009
    dat <- filter(dat, !(Year < 2009))
    dat = mutate(dat,
      IsCircum = as.numeric(Year >= YearCircum & !is.na(YearCircum)),
      EverCircum = as.numeric(EverCircum==1 & !is.na(EverCircum)))
    dat <- select(dat, -(YearCircum))
    dat <- filter(dat, EverCircum %in% Keep)
    dat
  }
}

#' @title getCircumcision
#' 
#' @description gets Circumcision data from MGH AHRI datasets.
#' 
#' @param Keep Keeps or drops circumcised men.
#' 
#' @export
getCircum <- getCircumStatus(Keep = c(0, 1))

#' @title keepCircum
#' 
#' @description gets Circumcision data from MGH AHRI datasets.
#' 
#' @param Keep Keeps or drops circumcised men.
#' 
#' @export
keepCircum <- getCircumStatus(Keep=1)

#' @title dropCircum
#' 
#' @description gets Circumcision data from MGH AHRI datasets.
#' 
#' @param Keep Keeps or drops circumcised men.
#' 
#' @export
dropCircum <- getCircumStatus(Keep=0)


#' @title getCondomUseData
#' 
#' @description  gets Condom use data from AHRI datasets. 
#' 
#' @param inFile Filepath to dataset, default is \code{getFiles()$mghfile}.
#' 
#' @export
#' @examples
#' dat <- readHealthDataMen()
#' getCondomUseData(dat)

getCondomUseData <- function(dat) {
  dat <- select(dat, IIntID, Year, Female, EverUsedCondom)
  dat <- filter(dat, EverUsedCondom %in% c(1:3))
  dat <- distinct(dat, IIntID, Year, .keep_all=TRUE)
  dat
}

#' @title addCondomVar
#' 
#' @description  Adds condom variable.
#' 
#' @param dat Exisiting dataset.
#' 
#' @return
#'
#' @export 
addCondomVar <- function(dat, cdat) {
  cdat <- getCondomUseData(cdat) %>% 
    select(IIntID, Year, EverUsedCondom)
  dat <- left_join(dat, cdat, by=c("IIntID", "Year"))
  dat$EverUsedCondom[is.na(dat$EverUsedCondom)] <- 
    sample(sort(unique(dat$EverUsedCondom)),
    size=sum(is.na(dat$EverUsedCondom)),
    replace=TRUE,
    prob=prop.table(table(dat$EverUsedCondom)))
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
  cdat <- group_by(cdat, IIntID) %>%
    summarize(EverCircum = max(IsCircumcised))
  cdat <- left_join(dat, cdat, by="IIntID")
  cdat$EverCircum[is.na(cdat$EverCircum)] <- 0
  cdat
}
