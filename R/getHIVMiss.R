#' @title readHIVSurvYear
#' 
#' @description Reads the data for each HIV surveillance dataset by year.
#' 
#' @param inFile File path to file. 
#' 
#' @return data.frame
#'
#' @keywords internal
#' @export 
readHIVSurvYear <- function(inFile, addVars=" ") {
  dat <- haven::read_dta(inFile) %>% select(IIntID=IIntId, BSIntID=BSIntId, VisitDate, 
    Comment=PrematureCompletionReason, HIVResult, HIVRefused, matches(addVars))
  dat <- dplyr::mutate(dat, 
    Comment = as.character(haven::as_factor(Comment)),
    HIVResult = as.character(haven::as_factor(HIVResult)),
    HIVRefused = as.character(haven::as_factor(HIVRefused)),
    IIntID = as.integer(IIntID))
  yr <- unique(format(dat$VisitDate[!is.na(dat$VisitDate)], "%Y"))[1]
  dat$VisitDate[is.na(dat$VisitDate)]  <- 
    as.Date(paste0(yr, "-01-01"), origin="1970-01-01")
  dat$HIVRefused[is.na(dat$HIVRefused)] <- 97 
  filter(dat, !is.na(IIntID))
}

#' @title setHIVMiss
#' 
#' @description  Gets HIV test dates by merging yearly HIV surveillance datasets.
#' 
#' @param inFile The root path to folder containing HIV surveillance datasets. 
#' @param outFile Path to write new dataset.
#' @param dropTasP Drop TasP surveillance areas from the data. 
#' 
#' @return data.frame
#'
#' @keywords internal
#' @export 
setHIVMiss <- function(inFile="", outFile="", dropTasP=TRUE) {
  #
  files <- list.files(inFile, pattern=".dta$")
  # diff vars for 2005--2009
  set1 <- files[unlist(lapply(files,
    function(x) grepl("200[4-9]", x)))]
  dat1 <- lapply(file.path(inFile, set1), 
    function(x) readHIVSurvYear(x, addVars="HIVRefusedBy"))
  dat1 <- do.call(rbind, dat1)
  dat1 <- dplyr::rename(dat1, FormRefusedBy = HIVRefusedBy)
  dat1$FormRefused <- dat1$HIVRefused
  # From 2010-2017, HIVRefused changes, depends on FormRefused
  set2 <- files[unlist(lapply(files,
    function(x) grepl("201[0-9]", x)))]
  dat2 <- lapply(file.path(inFile, set2), 
    function(x) readHIVSurvYear(x, addVars="^FormRefusedBy$|^FormRefused$"))
  dat2 <- do.call(rbind, dat2)
  dat2 <- mutate(dat2, 
    FormRefused = as.character(haven::as_factor(FormRefused)))
  # Incorrectly coded from 2010, if Form Refused then HIV Refused
  dat2$HIVRefused[dat2$FormRefused=="Yes"] = "Yes"
  # dat2 <- select(dat2, -FormRefused)
  adat <- rbind(dat1, dat2)
  # Form refused by someone else
  adat$HIVRefused[adat$FormRefusedBy %in% c(2:5)] <- "Refused by other"
  adat <- mutate(adat, 
    IIntID = as.integer(IIntID),
    BSIntID = as.integer(BSIntID),
    Year = as.integer(format(VisitDate, "%Y")))
  if (dropTasP) adat <- dropTasPData(adat)
  dat <- select(adat, -c(FormRefusedBy)) %>% 
    arrange(IIntID, VisitDate)
  saveRDS(dat, outFile)
  dat
}

#' @title getHIVEligible
#' 
#' @description  Get eligibility for HIV testing.
#' 
#' @param dat Default is Null or loads dataset from \code{\link{setHIVMiss}}.
#' 
#' @return data.frame
#' @import dplyr
#' @keywords internal
#' @export 
getHIVEligible <- function(dat=NULL) {
  if (is.null(dat)) dat <- readRDS(getFiles()$eli_rda)
  dat <- mutate(dat, Drop = as.numeric(grepl(
    "OutMigrated|Outmigrat*|[Dd]ead|Broken|Non-Func*|
    Migrated unknown", Comment)))
  dat <- filter(dat, Drop != 1)
  dat <- mutate(dat, NotEligible = as.numeric(grepl(
    "Other|[Ss]ick|deaf|Mentally", Comment)))
  dat <- mutate(dat, NonContact = as.numeric(
    grepl("Non-[cC]ontact|Refused|Other|Migrated within|Temporarily|Not found", Comment)))
  dat$Contact = ""
  dat$Contact[dat$HIVRefused %in% c("Yes", "No")] <- "Contact"
  dat$Contact[dat$NonContact==1 & dat$Contact==""] <- "NonContact"
  # Everyting else not eligible
  dat$Contact[dat$Contact==""] <- "NotEligible"
  dat <- mutate(dat,
    Tested = as.numeric(HIVResult %in% c("Positive", "Negative")))
  dat
}

#' @title sumHIVMiss
#' 
#' @description  Get summary of participants that refused to test. 
#' 
#' @param dat Dataset from \code{\link{getHIVEligible}}.
#' 
#' @return data.frame
#'
#' @keywords internal
#' @export 
sumHIVMiss <- function(dat) {
  Enumerated <- group_by(dat, Year) %>% 
    summarize(EnumeratedN = n())
  Eligible <- filter(dat, Contact != "NotEligible") %>%
    group_by(Year) %>% summarize(EligibleN=n())
  Contacted <- filter(dat, Contact=="Contact") %>% 
    group_by(Year) %>% summarize(ContactN=n())
  Tested <- filter(dat, Contact=="Contact") %>% 
    group_by(Year) %>% summarize(TestedN=sum(as.numeric(HIVRefused=="No")))
  out <- Reduce(left_join, list(Enumerated, Eligible, Contacted, Tested ))
  out
}

#' @title getHIVCumTest
#' 
#' @description Get summary of number of times participants test. 
#' 
#' @param dat Dataset from  \code{\link{getHIVEligible}}.
#' @param num_test Integer for number of times tested. Use 2 for incidence cohort.
#' 
#' @return data.frame
#'
#' @keywords internal
#' @export 
getHIVCumTest <- function(dat, ntest=1) {
  dat <- filter(dat, Contact == "Contact")
  dat <- arrange(dat, IIntID, Year) %>% group_by(IIntID) %>% 
    mutate(CumTest = as.numeric(cumsum(Tested)>=ntest))
  group_by(dat, Year) %>%
    summarize(TestedN = n(), EverTest = sum(CumTest),
    TestedPerc = round(EverTest/TestedN*100, 2))
}

#' @title getHIVIncEligible
#' 
#' @description  Get number of participants eligible for HIV incidence cohort. It is
#' recommended that you use all years in the HIV surveillance system (i.e. from 2003
#' onward).
#' 
#' @param Args requires Args, see \code{\link{setArgs}}.
#' @param ids keep only IDs.
#' 
#' @return data.frame
#'
#' @export 
#' @examples
#' Args <- setArgs(Years=c(2003:2018))
#' getHIVIncEligible(Args)
getHIVIncEligible <- function(Args, ids=NULL) {
  getN <- function(dat) {
    function(i) {
      xx = filter(dat, Year <= i)
      yy <- length(unique(xx$IIntID))
      data.frame(Year=i, N=yy)
    }
  }
  hiv <- getHIV()
  ehiv <- filter(hiv, HIVResult==0)
  edat <- setData(ehiv, Args, time2="VisitDate")
  getElig <- getN(edat)
  elig <- do.call(rbind, lapply(Args$Years, getElig)) %>% rename(EligN=N)
  rtdat <- getRTData(hiv)
  sdat <- splitAtEarlyPos(rtdat)
  sdat <- setData(sdat, Args, time2="obs_end", birthdate=NULL) 
  if (!is.null(ids))
    sdat <- sdat[sdat$IIntID %in% ids, ]
  getRT <- getN(sdat)
  cohort <- do.call(rbind, lapply(c(Args$Years), getRT))
  out <- right_join(elig, cohort)
  out <- mutate(out, Perc=round((N/EligN)*100, 2))
  out
}


#' @title mkHIVTestTable
#' 
#' @description  Make a table of HIV test participation.
#' 
#' @param Args requires Args, see \code{\link{setArgs}}. In this case, Args$Year must have
#' one additional year to compute HIV cohort person time. 
#' @param edat A dataset from \code{\link{getHIVEligible}}. If NULL, it will be created for
#' you.
#' @param IDS Needed to subset dataset, default is NULL. 
#' 
#' @return data.frame
#'
#' @export 

mkHIVTestTable <- function(Args, edat=NULL, IDS=NULL) {
  fmt <- function(x) trimws(formatC(x, big.mark=",", format="d"))
  rnd <- function(x) trimws(format(x, digits=1, nsmall=1))
  # Get testing date
  if (is.null(edat)) edat <- getHIVEligible()
  sdat <- sumHIVMiss(edat)
  sdat <- filter(sdat, Year %in% Args$Years)
  Eligible = paste0(fmt(sdat$EligibleN), "/", fmt(sdat$EnumeratedN))
  EligiblePerc = with(sdat, (EligibleN/EnumeratedN)*100)
  AveEligPerc <- paste0("(", rnd(mean(EligiblePerc)), ")")
  EligiblePerc = paste0("(", rnd(EligiblePerc), ")")
  Contact = paste0(fmt(sdat$ContactN), "/", fmt(sdat$EligibleN)) 
  ContactPerc = with(sdat, (ContactN/EligibleN)*100)
  AveContactPerc <- paste0("(", rnd(mean(ContactPerc)), ")")
  ContactPerc = paste0("(", rnd(ContactPerc), ")")
  Tested = paste0(fmt(sdat$TestedN), "/", fmt(sdat$ContactN)) 
  TestedPerc = with(sdat, (TestedN/ContactN)*100)
  AveTestedPerc <- paste0("(", rnd(mean(TestedPerc)), ")")
  TestedPerc = paste0("(", rnd(TestedPerc), ")")
  edat <- filter(edat, Year %in% Args$Years)
  CumTest <- getHIVCumTest(edat) 
  Test1 <- rnd(CumTest$TestedPerc)
  inc_elig <- getHIVIncEligible(Args, ids=IDS)
  inc_elig$EligN <- fmt(inc_elig$EligN)
  inc_elig$N <- fmt(inc_elig$N)
  AveIncElig <- paste0("(", rnd(mean(inc_elig$Perc)), ")")
  inc_elig$Perc <- paste0("(", rnd(inc_elig$Perc), ")")
  out <- data.frame(Year=sdat$Year, Eligible, EligiblePerc,
    Contact, ContactPerc, Tested, TestedPerc, Test1, stringsAsFactors=FALSE)
  out <- left_join(out, inc_elig)
  Ave <- c("Ave.", NA, AveEligPerc, NA, AveContactPerc, NA, AveTestedPerc, NA, NA, NA, AveIncElig)
  out <- rbind(out, Average=Ave)
  out
}


#' @title getFollowUp
#' 
#' @description  Used for inverse probability weights.
#' 
#' @param x
#' 
#' @return vector
#'
#' @keywords internal
#' @export 
getFollowUp <- function(x) {
  xx = vector(length=length(x))
  for (i in seq(x))
    if (i < length(x)) 
      xx[i]  <- as.numeric(x[i+1]=="Contact" & x[i]=="Contact") 
    else
      xx[i] <- as.numeric(x[i]=="Contact")
  xx
}


#' @title getDropOut
#' 
#' @description  Used for inverse probability weights.
#' 
#' @param x
#' 
#' @return vector
#'
#' @keywords internal
#' @export 
getDropOut <- function(x) {
  xx = vector(length=length(x))
  lastConsent <- suppressWarnings(max(which(x=="Contact")))
  for (i in seq(x))
    xx[i] <- as.numeric(i > lastConsent & x[i]!="Contact")
  xx
}


