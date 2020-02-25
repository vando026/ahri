#' @title getPartnerData
#' 
#' @description  get data on partners and marital status. 
#' 
#' @param inFile path to data which is typically set using \code{\link{getFiles}}
#'
#' @return data.frame
#'
#' @export
#' 
#' @import dplyr

getPartnerData <- function( 
  Args=eval.parent(quote(Args))) {

  getDat <- function(inFile) {
    read_csv(inFile, 
      col_types=cols_only(
        IIntId="i",
        VisitDate="D",
        AgeAtVisit="i",
        PartnersInLastTwelveMonths="i",
        CurrentMaritalStatus="i"))
  }
  wgh <- getDat(Args$inFiles$wghfile)
  mgh <- getDat(Args$inFiles$mghfile)

  ghdat0 <- bind_rows(wgh, mgh)
  ghdat0 <- filter(ghdat0, AgeAtVisit >= 15 & AgeAtVisit <= 80)
  ghdat0 <- mutate(ghdat0, Year=as.integer(format(VisitDate, "%Y"))) %>%
    rename(IIntID=IIntId, Partner12=PartnersInLastTwelveMonths)

  pdat <- select(ghdat0, IIntID, Year, Partner12) %>% 
    group_by(IIntID, Year) %>% 
    mutate(Partner1=ifelse(Partner12>90, NA, Partner12)) %>% 
    summarize(Partner1=max(Partner1, na.rm=TRUE)) %>% 
    ungroup(pdat)
  pdat <- mutate(pdat, Partner1=replace(Partner1, is.infinite(Partner1), NA))

  # MArital = Married or Polygamous
  mdat <- select(ghdat0, IIntID, Year, Married) %>%  
    mutate(Married1=mkMarriedVar(Married))

  # Now, carryforward in year only
  mdat <- group_by(mdat, IIntID, Year) %>% mutate(
    Married1=na.locf(Married1, na.rm=FALSE),
    Married1=na.locf(Married1, na.rm=FALSE, fromLast=TRUE))
  # Make married for whole year if married in episode
  mdat <- group_by(mdat, IIntID, Year) %>% 
    summarize(Married1=max(Married1))

  # Now)update partners in 12 months using marital var
  ppdat <- inner_join(mdat, pdat, by=c("IIntID", "Year"))
  ppdat <- mutate(ppdat, Partner1= ifelse(
    Married1==1 & (Partner1==0 | is.na(Partner1)),
    1, Partner1))
  save(ppdat, file=Args$inFiles$parfile)
  ppdat
}


#' @title makePartnerData
#' 
#' @description  Adds partner data from \code{\link{getPartnerData}} to existing dataset.
#' Missing values are inputed or carried forward. 
#' 
#' @param dat data.frame to which partner data is merged.
#' 
#' @param inFile path to data which is typically set using \code{\link{getFiles}}.
#'
#' @return data.frame
#'
#' @export
#' 
#' @import dplyr
#' @import zoo

makePartnerData <- function(dat,
  Args=eval.parent(quote(Args))) {

  load(file=file.path(Args$inFiles$parfile))
  out <- left_join(dat, ppdat, by=c("IIntID", "Year"))
  out <- mutate(out, Partner2=
    ifelse(Partner1 %in% c(0, 1), 0,
    ifelse(Partner1 %in% (2:100), 1, NA))) %>%
    arrange(IIntID, Year)

  out <- group_by(out, IIntID) %>% mutate(
    Married=na.locf(Married1, na.rm=FALSE),
    Married=na.locf(Married, na.rm=FALSE, fromLast=TRUE),
    Partner1=na.locf(Partner1, na.rm=FALSE),
    Partner1=na.locf(Partner1, na.rm=FALSE, fromLast=TRUE),
    Partner2=na.locf(Partner2, na.rm=FALSE),
    Partner2=na.locf(Partner2, na.rm=FALSE, fromLast=TRUE)) %>%
    ungroup(out)

  # Replace missing values
  propImpute <- function(Var) {
    Impute <- sample(Var[!is.na(Var)], 
      length(Var), replace=TRUE)
    Out <- ifelse(is.na(Var), Impute, Var)
    Out
  }
  out <- mutate(out, 
    Married=propImpute(Married),
    Partner1=propImpute(Partner1),
    Partner2=propImpute(Partner2))
  out <- select(out, -matches("Married[C1]"))
  out
}

#' @title getMaritalStatus
#' 
#' @description  Get the marital status variable.
#' 
#' @param dat WGH or MGH dataset.
#' 
#' @return variable
#'
#' @export 
getMaritalStatus <- function(var) {
  ifelse(var %in% c(1:3, 6:8, 11:16), 1, # married 
    ifelse(var %in% c(4, 5, 9, 10, 17), 0, NA)) # not married
}


#' @title addMaritalStatus
#' 
#' @description  Add marital status to exisisting dataset. 
#' 
#' @param dat Existing dataset.
#' 
#' @param mdat Dataset from \code{\link{getMGH}}.
#' 
#' @return 
#'
#' @export 
addMaritalStatus <- function(dat, mdat, fun=as.factor) {
  mdat$MaritalStatus <- getMaritalStatus(mdat$Marital)
  mdat <- select(mdat, IIntID, Year, MaritalStatus)
  dat <- left_join(dat, mdat, by=c("IIntID", "Year"))
  dat  <- mutate(dat, MaritalStatus = zoo::na.locf(MaritalStatus, na.rm=FALSE))
  dat  <- mutate(dat, MaritalStatus = zoo::na.locf(MaritalStatus, na.rm=FALSE, fromLast=TRUE))
  dat <- mutate(dat, MaritalStatus = fun(MaritalStatus))
  dat
}
