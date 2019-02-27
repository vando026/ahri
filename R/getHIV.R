#' @title readHIVData
#' 
#' @description  Reads in AHRI data from csv file
#' 
#' @param inFiles file paths from \code{\link{setArgs}}.
#' @param dropTasP drop TasP surveillance areas from the data. 
#' 
#' @return data.frame
#'
#' @import dplyr
#' @export 

readHIVData <- function(
  inFiles=getFiles(), dropTasP=TRUE) {
  hiv <- readr::read_csv(inFiles$hivfile,
    col_types=cols_only(
      ResidencyBSIntId="i",
      IIntId="i",
      VisitDate="D",
      HIVResult="i",
      Sex="i",
      AgeAtVisit="i"))
  hiv <- filter(hiv, Sex %in% c(1,2))
  hiv <- mutate(hiv,
    Female=as.integer(ifelse(Sex==2, 1, 0)),
    Year = format(VisitDate, "%Y"))
  hiv <- rename(hiv, IIntID=IIntId, BSIntID=ResidencyBSIntId) %>% 
   select(-Sex) %>% arrange(IIntID, VisitDate)
  if (dropTasP==TRUE) 
    hiv <- dropTasPData(hiv, inFile=inFiles$pipfile)
  hiv
}

#' @title getHIV
#' 
#' @description  get all valid test results from HIV surveillance.
#' 
#' @param Args requires Args, see \code{\link{setArgs}}.
#'
#' @return data.frame
#'
#' @import dplyr 
#'
#' @export 

getHIV <- function(Args) {
  hiv <- readHIVData(Args$inFiles, dropTasP=FALSE)
  # Only deal with valid test results
  hiv <- filter(hiv, HIVResult %in% c(0,1))
  hiv <- mutate(hiv, 
    HIVNegative = ifelse(HIVResult==0, VisitDate, NA), 
    HIVPositive = ifelse(HIVResult==1, VisitDate, NA))
  hiv <- mutate(hiv, Year=as.integer(format(VisitDate, "%Y")))
  Vars <- c("HIVNegative", "HIVPositive")
  hiv[Vars] <- lapply(hiv[Vars], as.Date, origin="1970-01-01")
  hiv 
}

#' @title setHIV
#' 
#' @description set HIV data according parameters of Args. 
#' 
#' @param Args requires Args, see \code{\link{setArgs}}.
#' 
#' @return 
#'
#' @export 
setHIV <- function(Args) {
  dat <- getHIV(Args)
  setData(dat)
}

#' @title getHIVDates
#' 
#' @description  Get all the test dates until the first HIV-positive date.
#' 
#' @param Args requires Args, see \code{\link{setArgs}}.
#' 
#' @return data.frame
#'
#' @export 

getHIVDates <- function(Args, surv_date="2005-01-01") {
  dat <- getHIV(Args)
  early_pos <- getDatesMin(dat, "HIVPositive", "early_pos")
  dat <- left_join(dat, early_pos, by="IIntID")
  dat <- filter(dat,
    !(VisitDate > as.Date(early_pos, "1970-01-01") & !is.na(early_pos)))
  dat <- mutate(dat, event = as.numeric(!is.na(HIVPositive) & VisitDate==HIVPositive))
  dat <- select(dat, IIntID, VisitDate, Year, Female, event)
  dat <- setData(dat, time2="VisitDate")
  dat
}

  # dat <- mutate(dat, 
    # Time = as.numeric(VisitDate - as.Date(surv_date, origin="1970-01-01")))

