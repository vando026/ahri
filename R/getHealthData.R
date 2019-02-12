#' @title readHealthData
#' 
#' @description  Reads in men and women general health data. 
#' 
#' @param inFile Filepath to dataset, default is \code{getFiles()$mghfile}.
#' 
#' @return 
#'
#' @examples
#' readHealthMen <- readHealthData(getFiles()$mghfile)
#' readHealthWomen <- readHealthData(getFiles()$wghfile)
readHealthData <- function(inFile, Fem) {
  function() {
    dat <- haven::read_dta(inFile) %>%
      select(IIntID=IIntId, VisitDate,
      AgeAtVisit, EverUsedCondom=MRPEverUsedCondoms, 
      matches("IsCircumcised"))
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
#' @param inFile Filepath to dataset, default is \code{getFiles()$mghfile}.
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
  select(dat, IIntID, Year, IsCircumcised)
}

#' @title getCircum
#' 
#' @description gets Circumcision data from MGH AHRI datasets.
#' 
#' @param Keep Keeps or drops circumcised men.
#' 
#' @export
#' @examples
#' keepCircum <- getCircum(Keep=1)
#' dropCircum <- getCircum(Keep=0)
#' getCircumcision <- getCircum(Keep = c(0, 1))

getCircum <- function(Keep) {
  function(dat) {
    cdat <- getCircumcisionData()
    cdat <- filter(cdat, IsCircumcised==1)
    cdat <- group_by(cdat, IIntID) %>% 
      summarize(YearCircum = min(Year))
    dat <- left_join(dat, cdat, by="IIntID")
    # No surv time before 2009
    dat <- filter(dat, !(Year < 2009))
    dat = mutate(dat,
      IsCircum = as.numeric(Year >= YearCircum & !is.na(YearCircum)))
    dat[, !(names(dat) %in% c("YearCircum"))]
    dat <- filter(dat, IsCircum %in% Keep)
    dat
  }
}
getCircumcision <- getCircum(Keep = c(0, 1))
keepCircum <- getCircum(Keep=1)
dropCircum <- getCircum(Keep=0)


#' @title getCondomUseData
#' 
#' @description  gets Condom use data from AHRI datasets. 
#' 
#' @param inFile Filepath to dataset, default is \code{getFiles()$mghfile}.
#' 
#' @export

getCondomUseData <- function(dat) {
  dat <- select(dat, IIntID, Year, Female, EverUsedCondom)
  dat <- filter(dat, EverUsedCondom %in% c(1:3))
  dat <- mutate(dat, 
    EverUsedCondom = as.numeric(EverUsedCondom %in% c(1, 2)))
  dat
}


