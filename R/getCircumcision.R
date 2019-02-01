#' @title getCircumcisionData
#' 
#' @description  gets Circumcision data from MGH AHRI datasets. 
#' 
#' @param inFile filepath to dataset, default is \code{getFiles()$mghfile}.
#' 
#' @export

getCircumcisionData <- function(
  inFile=getFiles()$mghfile) {
  cdat <- readr::read_csv(inFile, 
    col_types=readr::cols_only(
      IIntId="i",
      VisitDate="c",
      AgeAtVisit="i",
      IsCircumcised="i"))
  cdat$VisitDate <- as.Date(cdat$VisitDate, "%d/%m/%Y")
  names(cdat)[names(cdat)=="IIntId"] <- "IIntID"
  cdat <- cdat[cdat$IsCircumcised %in% c(1,2), ]
  cdat$IsCircumcised <- as.numeric(cdat$IsCircumcised==1)
  cdat$Year <- as.integer(format(cdat$VisitDate, "%Y"))
  cdat[, !(names(cdat) %in% "VisitDate")]
}

#' @title getCircum
#' 
#' @description gets Circumcision data from MGH AHRI datasets.
#' 
#' @param Keep keeps or drops circumcised men.
#' 
#' @param dat dataset with episodes.
#' 
#' @param Args arguments. 
#' 
#' @export
#' @examples
#' keepCircum <- getCircum(Keep=1)
#' dropCircum <- getCircum(Keep=0)
#' keepCircum(dat, Args)
#' getCircumcision <- getCircum(Keep = c(0, 1))

getCircum <- function(Keep) {
  function(dat, Args=eval.parent(quote(Args))) {
  if("Fem" %in% names(Args$Age)) stop("No females allowed")
  cdat <- getCircumcisionData(Args$inFiles$mghfile)
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

