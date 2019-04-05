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
  dat <- haven::read_dta(inFile) %>% select(IIntID=IIntId, VisitDate, 
    Comment=PrematureCompletionReason, HIVRefused, matches(addVars))
  dat <- mutate(dat, 
    Comment = as.character(haven::as_factor(Comment)),
    HIVRefused = as.character(haven::as_factor(HIVRefused)))
  # replace missing visit dates
  yr <- unique(format(dat$VisitDate[!is.na(dat$VisitDate)], "%Y"))[1]
  dat$VisitDate[is.na(dat$VisitDate)]  <- 
    as.Date(paste0(yr, "-01-01"), origin="1970-01-01")
  # dat$HIVRefused[is.na(dat$HIVRefused)] <- 97 
  filter(dat, !is.na(IIntID))
}

#' @title setHIVMiss
#' 
#' @description  Gets HIV test dates by merging yearly HIV surveillance datasets.
#' 
#' @param Root The root path to folder containing HIV surveillance datasets. 
#' @param Args requires Args, see \code{\link{setArgs}}.
#' 
#' @return 
#'
#' @export 
setHIVMiss <- function(Args, Root=setRoot(), dropTasP=TRUE) {
  filep <- file.path(Root, "Source/HIVSurveillance")
  files <- list.files(filep, pattern=".dta$")
  # diff vars for 2005--2009
  set1 <- files[unlist(lapply(files,
    function(x) grepl("200[5-9]", x)))]
  dat1 <- lapply(file.path(filep, set1), readHIVSurvYear)
  dat1 <- do.call(rbind, dat1)
  # From 2010-2017, HIVRefused changes, depends on FormRefused
  set2 <- files[unlist(lapply(files,
    function(x) grepl("201[0-7]", x)))]
  dat2 <- lapply(file.path(filep, set2), 
    function(x) readHIVSurvYear(x, addVars="FormRefused$"))
  dat2 <- do.call(rbind, dat2)
  dat2 <- mutate(dat2, 
    FormRefused = as.character(haven::as_factor(FormRefused)))
  dat2$HIVRefused[dat2$FormRefused=="Yes"] = "Yes"
  dat2 <- select(dat2, -FormRefused)
  adat <- rbind(dat1, dat2)
  adat <- mutate(adat, IIntID = as.integer(IIntID),
    Year = as.integer(format(VisitDate, "%Y")))
  bdat <- ahri::getBirthDate(addVars="Female")
  adat <- setData(adat, bdat, Args, time2="VisitDate")
  if (dropTasP) {
    bdat <- getBSMax()
    adat <- left_join(adat, bdat, by=c("IIntID", "Year"))
    adat <- dropTasPData(adat)
  }
  adat
}

##' @title getHIVEligible
##' 
##' @description  Get eligibility for HIV testing.
##' 
##' @param dat Dataset from \code{\link{setHIVMiss}}.
##' 
##' @return 
##'
##' @export 
getHIVEligible <- function(dat) {
  dat <- mutate(dat, NonContact = as.numeric(
    grepl("Non-[cC]ontact|Refused|Other", Comment)))
  dat$Contact = ""
  dat$Contact[dat$HIVRefused=="No"] <- "Yes"
  dat$Contact[dat$HIVRefused=="Yes"] <- "Yes"
  dat$Contact[dat$NonContact==1] <- "NonContact"
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
  dat <- filter(dat, Tested %in% c("Yes", "No"))
  dat <- mutate(dat, HIVTested = as.numeric(HIVRefused=="Yes"))
  dat <- arrange(dat, IIntID, Year) %>% group_by(IIntID) %>% 
    mutate(CumTest = as.numeric(cumsum(HIVTested)>=ntest))
  dat <- group_by(dat, Year) %>%
    summarize(TestedN = n(), EverTest = sum(CumTest),
    TestedPerc = round(EverTest/TestedN*100, 2))
  dat
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

getHIVIncEligible <- function(Args, dat=NULL) {
  # Get HIV inc cohort
  hiv <- setHIV(Args)
  rtdat <- getRTData(hiv)
  rtdat <- mutate(rtdat, YearEnter = as.numeric(format(obs_start, "%Y"))) 
  year_in  <- group_by(rtdat, YearEnter) %>% 
    summarize(N=n())
  year_in <- filter(year_in, YearEnter %in% Args$Year) %>% 
    rename(Year=YearEnter)
  sdat <- splitAtEarlyPos(rtdat)
  year_n <- group_by(sdat, Year) %>% 
    summarize(N = n())
  year_n <- filter(year_n, Year %in% Args$Year)
  right_join(year_in, year_n, by="Year")
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
  rnd <- function(x) trimws(format(x, digits=1, nsmall=1))
  # Get testing date
  dat <- setHIVMiss(Args)
  edat <- getHIVEligible(dat)
  sdat <- getHIVRefused(edat)
  Eligible = with(sdat, paste0(fmt(ContactedN), "/", fmt(EligibleN)))
  EligiblePerc = paste0("(", rnd(sdat$ContactPerc*100), ")")
  Consent = with(sdat, paste0(fmt(ConsentN), "/", fmt(ContactedN))) 
  ConsentRate = paste0("(", rnd(sdat$ConsentRate*100), ")")
  b3 <- getHIVIncEligible(Args)
  b3$N.x <- fmt(b3$N.x)
  b3$N.x[length(b3$N.x)] <- "-"
  b3$N.y <- fmt(b3$N.y)
  b4 <- getHIVCumTest(edat)
  b4$CumPerc = paste0("", rnd(b4$TestedPerc), "")
  data.frame(Year=sdat$Year, Eligible, EligiblePerc,
    Consent, ConsentRate, CumTest=b4$CumPerc,
    IncEnter=b3$N.x, IncTotal=b3$N.y,
    stringsAsFactors=FALSE)
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
  dat <- setHIVMiss(Args)
  edat <- getHIVEligible(dat)
  sdat <- getHIVRefused(edat)
  sdat <- filter(sdat, Year != 2017)
  pd <- select(sdat, ConsentPerc, RefusePerc, NonContactPerc)
  rate <- sdat$ConsentRate
  px <-  t(as.matrix(pd)) 

  if(!is.null(gfun)) {
    gfun(file.path(output,
      paste0(fname, ".", deparse(substitute(gfun)))),
      units="in", width=5.5, height=5.0, pointsize=9, 
      res=200, type="cairo")
  }
  m <- layout(matrix(seq(2), nrow=2), heights=c(8.5, 1))
  YlRed <- RColorBrewer::brewer.pal(9, "YlOrRd")
  xl <- Args$Year[Args$Year != 2017]
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
  text(xlx, y=refuse+consent, labels=fmx(refuse), col="white", font=2, pos=1)
  text(xlx, y=1, labels=fmx(nonc), col="white", font=2, pos=1)
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
    dat <- setHIVMiss(Args)
    edat <- getHIVEligible(dat)
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
    type="n", bty="l", font.lab=2, cex.lab=1.4)
  lines(cyear, c_all, col="black", lwd=3)
  lapply(seq(6), function(x) lines(cyear, cmal[[x]], col=Blues[x+3]))
  lapply(seq(6), function(x) lines(cyear, cfem[[x]], col=Reds[x+3]))
  legend(2016.8, 0.8, c("All", paste("Male:", agesm), paste("Female:", agesf)), ncol=1,
    bty="n", lwd=2, lty=1, col=c("black", Blues[4:9], Reds[4:9]), xpd=TRUE)
  dev.off()
}
