#' @title getCircumcisionData
#' 
#' @description  gets Circumcision data from MGH AHRI datasets. IsCircumcised: 1=yes;
#' 2=no. WhenCircumcised: 1=as_child,2=as_adult,3=as_infant,>4 = NA.
#' 
#' @param inFile arguments from \code{\link{setArgs}} to pass to \code{getHIV} function
#' 
#' @export
#' 
#' @importFrom readr read_csv cols_only

getCircumcisionData <- function(
  inFile=Args$inFiles$mghfile) {
  cdat <- read_csv(inFile, 
    col_types=cols_only(
      IIntId="i",
      VisitDate="D",
      IsCircumcised="i"))
  names(cdat)[names(cdat)=="IIntId"] <- "IIntID"
  cdat <- cdat[cdat$IsCircumcised %in% c(1,2), ]
  cdat$IsCircumcised <- 
    ifelse(cdat$IsCircumcised==1, 1, 0)
  cdat$Year <- as.integer(format(cdat$VisitDate, "%Y"))
  cdat[, !(names(cdat) %in% "VisitDate")]
}

#' @title addCircumcisionData
#' 
#' @description gets Circumcision data from MGH AHRI datasets.
#' 
#' @param dat dataset with episodes.
#' 
#' @param cdat circumcision dataset.
#' 
#' @param mergeBy merge by IIntID or IIntID/Year.
#' 
#' @export
#' 
#' @importFrom zoo na.locf  
#' @import dplyr

addCircumcisionData <- function(dat, cdat, 
  mergeBy=c("IIntID", "Year"), Args) {

  dat <- left_join(dat, cdat, by=mergeBy)

  dat1 <- group_by(dat, IIntID) %>% mutate(
    CircumNo = ifelse(IsCircumcised==0, 0, NA),
    Circumcised = ifelse(IsCircumcised==1, 1, NA))

  dat2 <- group_by(dat1, IIntID) %>%
    arrange(IIntID, Year) %>% mutate(
      CircumNo = na.locf(CircumNo, fromLast=TRUE, na.rm=FALSE),
      Circumcised = na.locf(Circumcised, na.rm=FALSE))

  dat2$Circumcised[with(dat2, is.na(Circumcised) & CircumNo==0)] <- 0
  dat2[, !(names(dat2) %in% c("CircumNo", "IsCircumcised"))]
}


