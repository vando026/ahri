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
  dat <- haven::read_dta(inFile) %>% select(IIntID=IIntId, BSIntID=BSIntId, VisitDate, 
    Comment=PrematureCompletionReason, HIVResult, HIVRefused, matches(addVars))
  dat <- mutate(dat, 
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
#' @param Root The root path to folder containing HIV surveillance datasets. 
#' 
#' @return 
#'
#' @export 
setHIVMiss <- function(Root=setRoot(), dropTasP=TRUE) {
  filep <- file.path(Root, "Source/HIVSurveillance")
  files <- list.files(filep, pattern=".dta$")
  # diff vars for 2005--2009
  set1 <- files[unlist(lapply(files,
    function(x) grepl("200[5-9]", x)))]
  dat1 <- lapply(file.path(filep, set1), 
    function(x) readHIVSurvYear(x, addVars="HIVRefusedBy"))
  dat1 <- do.call(rbind, dat1)
  dat1 <- dplyr::rename(dat1, FormRefusedBy = HIVRefusedBy)
  dat1$FormRefused <- NA
  # From 2010-2017, HIVRefused changes, depends on FormRefused
  set2 <- files[unlist(lapply(files,
    function(x) grepl("201[0-8]", x)))]
  dat2 <- lapply(file.path(filep, set2), 
    function(x) readHIVSurvYear(x, addVars="^FormRefusedBy$|^FormRefused$"))
  dat2 <- do.call(rbind, dat2)
  dat2 <- mutate(dat2, 
    FormRefused = as.character(haven::as_factor(FormRefused)))
  # Form refused by someone else
  dat2$FormRefused[dat2$FormRefusedBy %in% c(2:5)] <- "Refused by other"
  # Incorrectly coded, if Form Refused then HIV Refused
  # dat2$HIVRefused[dat2$FormRefused=="Yes"] = "Yes"
  # dat2 <- select(dat2, -FormRefused)
  adat <- rbind(dat1, dat2)
  adat$HIVRefused[adat$FormRefusedBy %in% c(2:5)] <- "Refused by other"
  adat <- mutate(adat, 
    IIntID = as.integer(IIntID),
    BSIntID = as.integer(BSIntID),
    Year = as.integer(format(VisitDate, "%Y")))
  if (dropTasP) adat <- dropTasPData(adat)
  dat <- select(adat, -c(FormRefusedBy)) %>% 
    arrange(IIntID, VisitDate)
  save(dat, file=file.path(getFiles()$elifile))
  dat
}

##' @title getHIVEligible
##' 
##' @description  Get eligibility for HIV testing.
##' 
##' @param Args 
##' @param dat Default is Null or loads \code{\link{setHIVMiss}}.
##' 
##' @return 
##'
##' @export 
getHIVEligible <- function(Args, dat=NULL) {
  if (is.null(dat))
    load(file=file.path(getFiles()$elifile))
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
#' @return 
#'
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
#' @description  Get number of participants eligible for HIV incidence cohort.
#' 
#' @param Args requires Args, see \code{\link{setArgs}}.
#' @param f Function to do additional data manipulation.
#' 
#' @return 
#'
#' @export 
getHIVIncEligible <- function(Args, f=identity) {
  edat <- getHIVEligible(Args)
  edat <- f(edat)
  # Get all contacted
  edat <- filter(edat, Contact=="Contact")
  # Year first neg
  edat <- mutate(edat, FirstNeg = 
    ifelse(HIVResult=="Negative", as.integer(format(VisitDate, "%Y")), 2100))
  edat <- group_by(edat, IIntID) %>% mutate(FirstNeg = min(FirstNeg))
  edat <- mutate(edat, HIVNeg = as.numeric(Year >= FirstNeg))
  edat <- filter(edat, HIVNeg==1)
  Elig <- group_by(edat, Year) %>% summarize(Elig = n())
  # Tested Negative
  hdat <- setHIV(Args)
  sdat <- filter(hdat, !is.na(HIVNegative))
  Neg <- group_by(sdat, Year) %>% summarize(NegN = n()) 
  dat <- left_join(Elig, Neg)
  dat <- mutate(dat, Perc = round(NegN/Elig*100, 1))
  # Cohort Person Time
  hiv <- getHIV()
  rtdat <- getRTData(hiv)
  Args$imputeMethod <- imputeEndPoint
  edat <- Args$imputeMethod(rtdat)
  edat <- splitAtEarlyPos(edat)
  ptime <- group_by(edat, Year) %>% 
    summarize(PTime = n())
  left_join(dat, ptime)
}

#' @title mkHIVTestTable
#' 
#' @description  Make a table of HIV test participation.
#' 
#' @param Args requires Args, see \code{\link{setArgs}}. In this case, Args$Year must have
#' one additional year to compute HIV cohort person time. 
#' 
#' @return data.frame
#'
#' @export 

mkHIVTestTable <- function(Args) {
  fmt <- function(x) trimws(formatC(x, big.mark=",", format="d"))
  rnd <- function(x) trimws(format(x, digits=1, nsmall=1))
  # Get testing date
  edat <- getHIVEligible(Args)
  sdat <- sumHIVMiss(edat)
  Eligible = paste0(fmt(sdat$EligibleN), "/", fmt(sdat$EnumeratedN))
  EligiblePerc = with(sdat, (EligibleN/EnumeratedN)*100)
  EligiblePerc = paste0("(", rnd(EligiblePerc), ")")
  Contact = paste0(fmt(sdat$ContactN), "/", fmt(sdat$EligibleN)) 
  ContactPerc = with(sdat, (ContactN/EligibleN)*100)
  ContactPerc = paste0("(", rnd(ContactPerc), ")")
  Tested = paste0(fmt(sdat$TestedN), "/", fmt(sdat$ContactN)) 
  TestedPerc = with(sdat, (TestedN/ContactN)*100)
  TestedPerc = paste0("(", rnd(TestedPerc), ")")
  CumTest <- getHIVCumTest(edat) 
  Test1 <- rnd(CumTest$TestedPerc)
  inc_elig <- getHIVIncEligible(Args)
  inc_elig$Elig <- fmt(inc_elig$Elig)
  inc_elig$NegN <- fmt(inc_elig$NegN)
  inc_elig$Perc <- rnd(inc_elig$Perc)
  inc_elig$PTime <- fmt(inc_elig$PTime)
  out <- data.frame(Year=sdat$Year, Eligible, EligiblePerc,
    Contact, ContactPerc, Tested, TestedPerc, Test1, stringsAsFactors=FALSE)
  out <- left_join(out, inc_elig)
  out <- filter(out, Year %in% Args$Year[-length(Args$Year)])
  out[out$Year==Args$Year[1], c("Elig", "NegN", "Perc")] <-  "-"
  out
}
# debugonce(mkHIVTestTable)
# mkHIVTestTable(Args)


#' @title plotHIVTestYear
#' 
#' @description  Function to plot consent rates by age and sex.
#' 
#' @param cyear Years to plot.
#' @param fname Name of file to save. 
#' 
#' @export 
plotHIVTestYear <- function(cyear=c(2005:2017), 
  fname="ConsentRate.png") {

  alainr::getColor()
  dat <- setHIVMiss()
  edat <- getHIVEligible(Args)
  bdat <- getBirthDate(addVars="Female")
  getConsent <- function(iAge) {
    Args <- setArgs(Year=cyear, Age=iAge)
    sdat <- setData(edat, Args, time2="VisitDate", birthdate=bdat)
    sdat <- sumHIVMiss(sdat)
    sdat <- filter(sdat, Year %in% cyear)
    with(sdat, TestedN/ContactN)
  }

  cmal <- list(
    getConsent(list(Mal=c(15,19))),
    getConsent(list(Mal=c(20,24))),
    getConsent(list(Mal=c(25,29))),
    getConsent(list(Mal=c(30,35))),
    getConsent(list(Mal=c(35,39))),
    getConsent(list(Mal=c(40,54)))
  )

  cfem <- list(
    getConsent(list(Fem=c(15,19))),
    getConsent(list(Fem=c(20,24))),
    getConsent(list(Fem=c(25,29))),
    getConsent(list(Fem=c(30,35))),
    getConsent(list(Fem=c(35,39))),
    getConsent(list(Fem=c(40,54)))
  )
  c_all <- getConsent(list(Fem=c(15, 49), Mal=c(15, 54)))

  ages <- c("15-19", "20-24", "25-29", "30-34", "35-39")
  agesf <- c(ages, "40-49")
  agesm <- c(ages, "40-54")

  png(filename=file.path(output, fname),
    units="in", width=8, height=5, pointsize=10, 
    res=200, type="cairo")
  par(mar=c(4.0,4.5,1.5,11))
  plot(cyear, cyear, ylim=c(0, 1),
    ylab="Proportion", xlab="Year", cex.axis=1.2,
    main="HIV tested", cex.main=1.4, xaxt='n',
    type="n", bty="l", font.lab=2, cex.lab=1.2)
  axis(side=1, at=cyear, cex.axis=1.2)
  lines(cyear, c_all, col="black", lwd=3)
  lapply(seq(6), function(x) lines(cyear, cmal[[x]], col=Blues[x+3]))
  lapply(seq(6), function(x) lines(cyear, cfem[[x]], col=Reds[x+3]))
  legend(2017.9, 0.8, c("All", paste("Male:", agesm), paste("Female:", agesf)), ncol=1,
    bty="n", lwd=2, lty=1, col=c("black", Blues[4:9], Reds[4:9]), xpd=TRUE, cex=1.2)
  dev.off()
}

#' @title getFollowUp
#' 
#' @description  Used for inverse probability weights
#' 
#' @param x
#' 
#' @return 
#'
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
#' @description  Used for inverse probability weights
#' 
#' @param x
#' 
#' @return 
#'
#' @export 
getDropOut <- function(x) {
  xx = vector(length=length(x))
  lastConsent <- suppressWarnings(max(which(x=="Contact")))
  for (i in seq(x))
    xx[i] <- as.numeric(i > lastConsent & x[i]!="Contact")
  xx
}


