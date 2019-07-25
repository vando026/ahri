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
  # From 2010-2017, HIVRefused changes, depends on FormRefused
  set2 <- files[unlist(lapply(files,
    function(x) grepl("201[0-8]", x)))]
  dat2 <- lapply(file.path(filep, set2), 
    function(x) readHIVSurvYear(x, addVars="^FormRefusedBy$|^FormRefused$"))
  dat2 <- do.call(rbind, dat2)
  dat2 <- mutate(dat2, 
    FormRefused = as.character(haven::as_factor(FormRefused)))
  # Form refused by someone else
  dat2$FormRefused[dat2$FormRefusedBy %in% c(2:5)] <- "Not Applicable"
  # Incorrectly coded, if Form Refused then HIV Refused
  dat2$HIVRefused[dat2$FormRefused=="Yes"] = "Yes"
  dat2 <- select(dat2, -FormRefused)
  adat <- rbind(dat1, dat2)
  adat$HIVRefused[adat$FormRefusedBy %in% c(2:5)] <- "Not Applicable"
  adat <- mutate(adat, 
    IIntID = as.integer(IIntID),
    BSIntID = as.integer(BSIntID),
    Year = as.integer(format(VisitDate, "%Y")))
  if (dropTasP) adat <- dropTasPData(adat)
  # drop not eligible
  adat <- mutate(adat, NotElig = as.numeric(
    grepl("Mentally|[Dd]ead|Broken|Non-Functional|deaf|[Ss]ick", Comment)))
  adat <- filter(adat, NotElig==0)
  eligible_dat <- select(adat, -c(FormRefusedBy, NotElig)) %>% 
    arrange(IIntID, VisitDate)
  save(eligible_dat, file=file.path(getFiles()$elifile))
  eligible_dat
}

##' @title getHIVEligible
##' 
##' @description  Get eligibility for HIV testing.
##' 
##' @param Args 
##' 
##' @return 
##'
##' @export 
getHIVEligible <- function(Args) {
  load(file=file.path(getFiles()$elifile))
  bdat <- ahri::getBirthDate(addVars="Female")
  dat <- setData(eligible_dat, Args, bdat, time2="VisitDate")
  dat <- mutate(dat, NonContact = as.numeric(
    grepl("Non-[cC]ontact|Refused|Other", Comment)))
  dat$Contact = ""
  dat$Contact[dat$HIVRefused %in% c("Yes", "No")] <- "Yes"
  dat$Contact[dat$NonContact==1 & dat$Contact==""] <- "NonContact"
  # Everyting else not eligible
  dat$Contact[dat$Contact==""] <- "NotEligible"
  dat$Tested <- dat$Contact
  dat$Tested[dat$HIVRefused=="Yes"] <- "No"
  select(dat, -NonContact)
}

#' @title getHIVRefused
#' 
#' @description  Get summary of participants that refused to test. 
#' 
#' @param dat Dataset from \code{\link{getHIVEligible}}.
#' 
#' @return data.frame
#'
#' @export 
getHIVRefused <- function(dat) {
  dat <- filter(dat, Contact != "NotEligible")
  eligible <- group_by(dat, Year) %>% 
    summarize(EligibleN = n())
  # Now replace Contact with HIV refused
  non_contact <- filter(dat, Tested=="NonContact") %>% 
    group_by(Year) %>% summarize(NonContactN=n())
  consent <- filter(dat, Tested=="Yes") %>% 
    group_by(Year) %>% summarize(ConsentN=n())
  refused <- filter(dat, Tested=="No") %>% 
    group_by(Year) %>% summarize(RefusedN=n())
  out <- Reduce(left_join, list(eligible, non_contact, consent, refused))
  out <- mutate(out,
    ContactedN = EligibleN - NonContactN,
    NonContactPerc = NonContactN/EligibleN,
    ContactPerc = 1 - NonContactPerc,
    RefusePerc = RefusedN/EligibleN,
    ConsentPerc = ConsentN/EligibleN,
    ConsentRate = ConsentN/ContactedN)
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
  dat <- filter(dat, Contact != "NotEligible")
  # den <- group_by(dat, Year) %>% summarize(TotalN = n())
  tdat <- filter(dat, Tested %in% c("Yes", "No"))
  tdat <- mutate(dat, Tested = as.numeric(Tested=="Yes"))
  tdat <- arrange(tdat, IIntID, Year) %>% group_by(IIntID) %>% 
    mutate(CumTest = as.numeric(cumsum(Tested)>=ntest))
  out <- group_by(tdat, Year) %>%
    summarize(TestedN = n(), EverTest = sum(CumTest),
    TestedPerc = round(EverTest/TestedN*100, 2))
  out
}

getNegTest <- function(Args) {
  dat <- getHIVEligible(Args)
  dat <- filter(dat, Contact == "Yes")
  dat <- mutate(dat, 
    HIVNegative = as.numeric(HIVResult=="Negative" & !is.na(HIVResult)),
    HIVPositive = as.numeric(HIVResult=="Positive" & !is.na(HIVResult)))
  dat <- arrange(dat, IIntID, VisitDate) %>% 
    group_by(IIntID) %>% 
    mutate(
      CumNeg = cumsum(HIVNegative), 
      CumNeg2 = as.numeric(CumNeg >= 2),
      AnyNeg = as.numeric(any(CumNeg >= 1)),
      Neg2 = as.numeric(any(CumNeg >= 2)))
  # Only look at those with HIV- result
  ndat <- filter(dat, AnyNeg==1)
  ydat <- filter(ndat, HIVNegative==1) %>% 
    arrange(IIntID, VisitDate) %>% 
    summarize(FirstNeg = min(VisitDate, na.rm=TRUE)) %>% 
    mutate(FirstNeg = as.integer(format(FirstNeg, "%Y")))
  ydat2 <- filter(ndat, HIVPositive==1) %>% 
    arrange(IIntID, VisitDate) %>% 
    summarize(FirstPos = min(VisitDate, na.rm=TRUE)) %>% 
    mutate(FirstPos = as.integer(format(FirstPos, "%Y")))
  ndat <- left_join(ndat, ydat, by = "IIntID")    
  ndat <- left_join(ndat, ydat2, by = "IIntID")    
  ndat$FirstPos[is.na(ndat$FirstPos)] = 2017
  ndat <- filter(ndat, Year >= FirstNeg & Year <= FirstPos)
  ndat
}    

#' @title getHIVIncEligible
#' 
#' @description  Get number of participants eligible for HIV incidence cohort.
#' 
#' @param Args requires Args, see \code{\link{setArgs}}.
#' 
#' @return 
#'
#' @export 

getHIVIncEligible <- function(Args) {
  # Get person time from rtdata set
  hiv <- getHIV()
  rtdat <- getRTData(hiv)
  Args$imputeMethod <- imputeEndPoint
  edat <- Args$imputeMethod(rtdat)
  edat <- splitAtEarlyPos(edat) 
  edat <- setData(edat, Args)
  ptime <- group_by(edat, Year) %>% 
    summarize(Cohort = n())
  # Get all tested HIV- in current or previous year
  ndat <- getNegTest(Args)
  ndat <- group_by(ndat, Year) %>% 
    summarize(N = sum(HIVNegative))
  nn = ndat$N + lag(ndat$N, 1)
  ndat <- data.frame(Year=ndat$Year, Elig=nn)
  out <- left_join(ndat, ptime)
  out <- mutate(out, IncPerc = (Cohort/Elig)*100)
  out
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
  fmt <- function(x) trimws(formatC(x, big.mark=",", format="d"))
  rnd <- function(x) trimws(format(x, digits=1, nsmall=1))
  # Get testing date
  edat <- getHIVEligible(Args)
  # All eligible irrespective of age
  census <- group_by(edat, Year) %>% 
   summarize(Total=length(unique(IIntID)))
  # Keep only age eligible and eligible
  edat <- dplyr::rename(edat, AgeAtVisit=Age)
  edat <- setAge(edat, Args)
  edat <- dplyr::rename(edat, Age=AgeAtVisit)
  edat <- filter(edat, Contact != "NotEligible")
  eligible <- group_by(edat, Year) %>% 
   summarize(Total=length(unique(IIntID)))
  Eligible = paste0(fmt(eligible$Total), "/", fmt(census$Total))
  EligiblePerc = (eligible$Total/census$Total)*100
  EligiblePerc = paste0("(", rnd(EligiblePerc), ")")
  pdat <- filter(edat, Contact=="Yes")
  present <- group_by(pdat, Year) %>% 
  present$Total[nrow(present)] <- NA
   summarize(Total=length(unique(IIntID)))
  Present = paste0(fmt(present$Total), "/", fmt(eligible$Total)) 
  PresentPerc = (present$Total/eligible$Total)*100 
  PresentPerc = paste0("(", rnd(PresentPerc), ")")
  test1 <- getHIVCumTest(edat) 
  Test1 <- rnd(test1$TestedPerc)
  inc_elig <- getHIVIncEligible(Args)
  inc_elig$Elig <- fmt(inc_elig$Elig)
  inc_elig$Cohort <- fmt(inc_elig$Cohort)
  inc_elig$IncPerc <- rnd(inc_elig$IncPerc)
  out <- data.frame(Year=eligible$Year, Eligible, EligiblePerc,
    Present, PresentPerc, Test1, stringsAsFactors=FALSE)
  out <- left_join(out, inc_elig)
  out
}
# debugonce(mkHIVTestTable)
# mkHIVTestTable(Args)



#' @title plotHIVTest
#' 
#' @description Make plot of HIV testing rate. 
#' 
#' @param Args.
#' 
#' @return 
#'
#' @export 
plotHIVTest <- function(Args, 
  fname=Args$fname, gfun=png) {
  fmx  <- function(x) format(x, nsmall=2, digits=2)
  edat <- getHIVEligible(Args)
  sdat <- getHIVRefused(edat)
  pd <- select(sdat, ConsentPerc, RefusePerc, NonContactPerc)
  rate <- sdat$ConsentRate
  px <-  t(as.matrix(pd)) 

  if(!is.null(gfun)) {
    if (!exists("output", envir=globalenv()))  
      output <- "~/Downloads"
    gfun(file.path(output,
      paste0(fname, ".", deparse(substitute(gfun)))),
      units="in", width=5.5, height=5.0, pointsize=9, 
      res=200, type="cairo")
  }
  m <- layout(matrix(seq(2), nrow=2), heights=c(8.5, 1))
  YlRed <- RColorBrewer::brewer.pal(9, "YlOrRd")
  xl <- Args$Year
  xlx <- (seq(length(xl))* 1.2) - 0.5
  par(mar=c(3.8, 4.4, 1.0, 1))
  barplot(px,  col=YlRed[c(4, 6, 8)], width=1, space=0.2,
    xlab="Year", ylab="Proportion", font.lab=2)
  lines(xlx, rate, col=YlRed[9], lwd=4)
  plotrix::staxlab(side=1, at=xlx, labels=xl, srt=45)
  consent = px["ConsentPerc", ]
  refuse = px["RefusePerc", ]
  nonc = px["NonContactPerc", ]
  text(xlx, y=0.15, labels=fmx(consent), col="white", font=2, pos=1)
  text(xlx, y=refuse+consent, labels=round(refuse, 2), col="white", font=2, pos=1)
  text(xlx, y=1, labels=round(nonc, 2), col="white", font=2, pos=1)
  text(xlx, y=rate+0.01, labels=fmx(rate), col=YlRed[9], pos=3, font=2)
  par(mar=c(1.1, 4.4, 1.0, 1))
  plot.new()
  legend("bottom", c("Consent rate", "Consent", "Refuse", "Non-Contact"),
    ncol=4, bty="n", inset=c(0, 0), pch=c(NA, rep(15, 3)), pt.cex=3, 
    lty=c(1, rep(NA, 3)), lwd=6, col=c(YlRed[c(9, 4, 6, 8)]))
  if(!is.null(gfun)) dev.off()
}

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

  Blues <- RColorBrewer::brewer.pal(9, "Blues")
  Reds <- RColorBrewer::brewer.pal(9, "Reds")

  getConsent <- function(iAge, nm) {
    Args <- setArgs(Year=cyear, Age=iAge)
    dat <- setHIVMiss()
    edat <- getHIVEligible(Args)
    getHIVRefused(edat)$ConsentRate
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
    units="in", width=7, height=5, pointsize=10, 
    res=200, type="cairo")
  par(mar=c(4.0,4.5,1.5,8))
  plot(cyear, cyear, ylim=c(0, 1),
    ylab="Proportion", xlab="Year",
    main="Consent rate", cex.main=1.4,
    type="n", bty="l", font.lab=2, cex.lab=1.2)
  lines(cyear, c_all, col="black", lwd=3)
  lapply(seq(6), function(x) lines(cyear, cmal[[x]], col=Blues[x+3]))
  lapply(seq(6), function(x) lines(cyear, cfem[[x]], col=Reds[x+3]))
  legend(2016.8, 0.8, c("All", paste("Male:", agesm), paste("Female:", agesf)), ncol=1,
    bty="n", lwd=2, lty=1, col=c("black", Blues[4:9], Reds[4:9]), xpd=TRUE)
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
      xx[i]  <- as.numeric(x[i+1]=="Yes" & x[i]=="Yes") 
    else
      xx[i] <- as.numeric(x[i]=="Yes")
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
  lastConsent <- suppressWarnings(max(which(x=="Yes")))
  for (i in seq(x))
    xx[i] <- as.numeric(i > lastConsent & x[i]!="Yes")
  xx
}


