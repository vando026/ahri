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
#' @param dat Dataset, otherwise made with \code{\link{setHIVMiss}}.
#' 
#' @return data.frame
#'
#' @export 
getHIVRefused <- function(Args, dat=NULL) {
  if(is.null(dat)) dat <- setHIVMiss(Args)
  # HIVRefusedYes = 1, HIVRefusedNo = 2
  dat <- filter(dat, HIVRefused %in% c(1, 2))
  dat <- mutate(dat, HIVRefused = as.numeric(HIVRefused==1))
  Refused <- group_by(dat, Year) %>% summarize(
    N=n(), RefusedN = sum(HIVRefused),
    TestedN = N - RefusedN,
    TestedPerc = round(TestedN/N *100, 2),
    RefusedPerc = round(RefusedN/N*100, 2))
  rename(Refused, ContactN = N)
}

#' @title getHIVCumTest
#' 
#' @description Get summary of number of times participants test. 
#' 
#' @param Args requires Args, see \code{\link{setArgs}}.
#' @param num_test Integer for number of times tested. Use 2 for incidence cohort.
#' @param dat Dataset, otherwise made with \code{\link{setHIVMiss}}.
#' 
#' @return 
#'
#' @export 
getHIVCumTest <- function(Args, num_test=1, dat=NULL) {
  if(is.null(dat)) dat <- setHIVMiss(Args)
  # HIVRefusedYes = 1, HIVRefusedNo = 2
  dat <- filter(dat, HIVRefused==2) %>%
    mutate(HIVTest = as.numeric(HIVRefused==2))
  cdat <- group_by(dat, IIntID) %>%
    mutate(EverTest = as.numeric(cumsum(HIVTest)>=num_test))
  EverTest <- group_by(cdat, Year) %>% summarize(
    N=n(), EverTestN=sum(EverTest),
    EverTestPerc = round(EverTestN/N*100, 2))
  EverTest
}

##' @title getHIVEligible
##' 
##' @description  Calculate eligibility for HIV testing.
##' 
##' @param Args requires Args, see \code{\link{setArgs}}.
##' @param dat Dataset, otherwise made with \code{\link{setHIVMiss}}.
##' 
##' @return 
##'
##' @export 
getHIVEligible <- function(Args, dat=NULL) {
  if(is.null(dat)) dat <- setHIVMiss(Args)
  # dat <- filter(dat, Year==2010)
  # Dead is not legible
  dat <- filter(dat, !grepl("Reported Dead", Comment))
  eligible <- group_by(dat, Year) %>% 
    summarize(EligibleN = n())
  present <- filter(dat, !grepl(
    "Out|Temp|Migr|Broken|Non-Functional", Comment))
  present <- group_by(present, Year) %>% 
   summarize(PresentN = n()) 
  out <- left_join(eligible, present, by="Year")
  # No data for 2017
  out$PresentN[out$Year==2017] = NA
   mutate(out, PresentPerc = round(PresentN/EligibleN * 100, 2))
}

#' @title getHIVIncEligible
#' 
#' @description  Get number of participants eligible for HIV incidence cohort.
#' 
#' @param Args requires Args, see \code{\link{setArgs}}.
#' @param dat Dataset, otherwise made with \code{\link{setHIVMiss}}.
#' 
#' @return 
#'
#' @export 

getHIVIncEligible <- function(Args) {
  dat <- setHIV(Args)
  dat <- group_by(dat, IIntID) %>% 
    mutate(byCount = n()) %>% arrange(IIntID, Year)
  dat <- filter(dat, byCount>=2)
  elig <- group_by(dat, Year) %>% summarize(IncEligN=n())
  # Get HIV inc cohort
  hiv <- getHIV(Args)
  hiv_test <- select(hiv, IIntID, Year) %>% mutate(Tested=1)
  rtdat <- getRTData(hiv)
  sdat <- Args$imputeMethod(rtdat)
  sdat <- splitAtSeroDate(sdat)
  sdat <- setData(sdat)
  ndat <- left_join(sdat, hiv_test, by=c("IIntID", "Year"))
  tested <- filter(ndat, Tested==1) %>% group_by(Year) %>% summarize(IncTestedN = n())
  out <- left_join(elig, tested, by="Year")
  mutate(out, IncTestPerc = round(IncTestedN/IncEligN*100, 2))
}




#' @title mkHIVTestTable
#' 
#' @description  Make a table of HIV test participation.
#' 
##' @param Args requires Args, see \code{\link{setArgs}}.
#' 
#' @return data.frame
#'
#' @export 

mkHIVTestTable <- function(Args) {
  fmt <- function(x) trimws(format(x, big.mark=","))
  rnd <- function(x) trimws(format(x, digits=2, nsmall=2))
  # Get testing date
  dat <- setHIVMiss(Args)
  elig <- getHIVEligible(Args, dat=dat)
  refuse <- getHIVRefused(Args, dat=dat)
  tab1 <- left_join(elig, refuse, by="Year")
  b1 <- select(tab1, Year, ContactN, PresentN) %>%
    mutate(ContactPerc = rnd(ContactN/PresentN*100))
  b1$Eligible = with(b1, paste0(fmt(ContactN), "/", fmt(PresentN)))
  b1$EPerc = paste0("(", b1$ContactPerc, "%)")
  b2 <- select(tab1, Year, TestedN, ContactN) %>%
    mutate(TestedPerc = rnd(TestedN/ContactN*100))
  b2$Test = with(b2, paste0(fmt(TestedN), "/", fmt(ContactN))) 
  b2$TestP = paste0("(", b2$TestedPerc, "%)")
  b3 <- getHIVIncEligible(Args)
  b3$Test = with(b3, paste0(fmt(IncTestedN), "/", fmt(IncEligN))) 
  b3$TestPerc = paste0("(", rnd(b3$IncTestPerc), "%)")
  data.frame(b1$Year, b1$Eligible, b1$EPerc, b2$Test, b2$TestP, 
    b3$Test, b3$TestPerc)
}
