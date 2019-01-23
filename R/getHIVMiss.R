#' @title getHIVMiss
#' 
#' @description  Calculates missed HIV test dates by year.
#' 
#' @param Args requires Args, see \code{\link{setArgs}}.
#' 
#' @return data.frame
#'
#' @export 

getHIVMiss <- function(Args) {
  hdat <- readr::read_csv(Args$inFiles$hivfile, 
    col_types=cols_only(
      ResidencyBSIntId="i",
      IIntId="i",
      VisitDate="D",
      HIVRefused="i",
      HIVResult="i",
      Sex="i",
      AgeAtVisit="i"))
  hdat <- dplyr::filter(hdat, Sex %in% c(1,2))
  hdat <- dplyr::mutate(hdat, Female=as.integer(ifelse(Sex==2, 1, 0)))
  hdat <- dplyr::rename(hdat, IIntID=IIntId, BSIntID=ResidencyBSIntId) %>% 
     select(-Sex) 
  # Get the bounds for obs period
  hdat <- dplyr::mutate(hdat, Year=as.integer(format(VisitDate, "%Y")))
  hdat <- dropTasPData(hdat, Args$inFiles$pipfile)
  hdat <- dplyr::arrange(hdat, IIntID, VisitDate)
  hdat <- setData(hdat, Args)
  # Keep sex
  # hdat <- dplyr::filter(hdat, Female %in% Args$FemCode)
  # Only legible for testing
  hdat <- dplyr::filter(hdat, HIVRefused %in% c(1, 2)) 

  # Refused
  out <- group_by(hdat, Year) %>% 
    summarize(N=n(),
      Refused=round((sum(as.numeric(HIVRefused==1))/n())*100, 2),
      Not_Refused=100 - Refused)

  # Get perc ever tested
  cdat <- dplyr::mutate(hdat, HIVTest = as.numeric(HIVRefused==2))
  cdat <- dplyr::group_by(cdat, IIntID) %>%
    mutate(EverTest = as.numeric(cumsum(HIVTest)>=1))
  out2 <- dplyr::group_by(cdat, Year) %>%
    dplyr::summarize(EverTest=round((sum(EverTest)/n())*100, 2))
  fem <- dplyr::group_by(hdat, Year) %>%
    dplyr::summarize(FemPerc = round((sum(Female)/n())*100, 2))
  res <- left_join(fem, out, by="Year")
  res <- left_join(res, out2, by="Year")
  res
}

#' @title readHIVSurvYear
#' 
#' @description Reads the data for each HIV surveillance dataset by year.
#' 
#' @param inFile File path to file. 
#' 
#' @return data.frame
#'
#' @export 
readHIVSurvYear <- function(inFile, addVars=" ") {
  dat <- haven::read_dta(inFile) %>% mutate(
    Comment = as.character(haven::as_factor(PrematureCompletionReason)))
  dat <- select(dat, IIntID=IIntId, VisitDate, 
    Comment, HIVRefused, contains(addVars))
  # replace missing visit dates
  yr <- unique(format(dat$VisitDate[!is.na(dat$VisitDate)], "%Y"))[1]
  dat$VisitDate[is.na(dat$VisitDate)]  <- 
    as.Date(paste0(yr, "-01-01"), origin="1970-01-01")
  filter(dat, !is.na(IIntID))
}

#' @title setHIVMiss
#' 
#' @description  Gets test dates but pulls each HIV surveillance dataset by year.
#' 
#' @param Root The root path to folder containing HIV surveillance datasets. 
#' @param Args requires Args, see \code{\link{setArgs}}.
#' 
#' @return 
#'
#' @export 

setHIVMiss <- function(Args, Root=setRoot()) {
  filep <- file.path(Root, "Source/HIVSurveillance")
  files <- list.files(filep, pattern=".dta$")
  adat <- lapply(file.path(filep, files), readHIVSurvYear)
  adat <- do.call(rbind, adat)
  adat <- mutate(adat, Year = format(VisitDate, "%Y"))
  adat <- rename(adat, obs_end=VisitDate)
  bdat <- getBirthDate(addVars="Female")
  adat <- setData(adat, bdat, Args)
  adat
}

#' @title getHIVRefused
#' 
#' @description  Get summary of participants that refused to test. 
#' 
#' @param Args requires Args, see \code{\link{setArgs}}.
#' 
#' @return data.frame
#'
#' @export 
getHIVRefused <- function(Args) {
  dat <- setHIVMiss(Args)
  # HIVRefusedYes = 1, HIVRefusedNo = 2
  dat <- filter(dat, HIVRefused %in% c(1, 2))
  dat <- mutate(dat, HIVRefused = as.numeric(HIVRefused==1))
  Refused <- group_by(dat, Year) %>% summarize(
    N=n(), Refused = round(sum(HIVRefused)/n() * 100, 2))
  Refused
}

#' @title getHIVCumTest
#' 
#' @description Get summary of number of times participants test. 
#' 
#' @param Args requires Args, see \code{\link{setArgs}}.
#' @param num_test Integer for number of times tested. Use 2 for incidence cohort.
#' 
#' @return 
#'
#' @export 
getHIVCumTest <- function(Args, num_test=1) {
  dat <- setHIVMiss(Args)
  # HIVRefusedYes = 1, HIVRefusedNo = 2
  dat <- filter(dat, HIVRefused %in% c(1, 2))
  cdat <- mutate(dat, HIVTest = as.numeric(HIVRefused==2))
  cdat <- group_by(cdat, IIntID) %>%
    mutate(EverTest = as.numeric(cumsum(HIVTest)>=num_test))
  EverTest <- group_by(cdat, Year) %>% summarize(
    N=n(), EverTest=round((sum(EverTest)/n())*100, 2))
  EverTest
}


##' @title getHIVEligible
##' 
##' @description  Calculate eligibility for HIV testing.
##' 
##' @param Root The root path to folder containing HIV surveillance datasets. 
##' @param Args requires Args, see \code{\link{setArgs}}.
##' 
##' @return 
##'
##' @export 
getHIVEligible <- function(Args) {
  dat <- setHIVMiss(Args)
  eligible <- group_by(dat, Year) %>% 
    summarize(Eligible = n())
  present <- filter(dat, !grepl(
    "Out|Temp|Migr|Broken|Non-Functional|Non-[cC]|Dead",
    Comment))
  present <- group_by(present, Year) %>% 
   summarize(Present = n()) 
  left_join(eligible, present, by="Year")
}
