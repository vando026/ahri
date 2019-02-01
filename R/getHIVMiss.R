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
  dat$HIVRefused[is.na(dat$HIVRefused)] <- 97 
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
setHIVMiss <- function(Args, Root=setRoot()) {
  filep <- file.path(Root, "Source/HIVSurveillance")
  files <- list.files(filep, pattern=".dta$")
  adat <- lapply(file.path(filep, files), readHIVSurvYear)
  adat <- do.call(rbind, adat)
  adat <- mutate(adat, Year = as.integer(format(VisitDate, "%Y")))
  adat <- rename(adat, obs_end=VisitDate)
  bdat <- getBirthDate(addVars="Female")
  adat <- setData(adat, bdat, Args)
  adat <- rename(adat, VisitDate=obs_end)
  adat
}

##' @title getHIVEligible
##' 
##' @description  Get eligibility for HIV testing.
##' 
##' @param Args requires Args, see \code{\link{setArgs}}.
##' @param dat Dataset, otherwise made with \code{\link{setHIVMiss}}.
##' 
##' @return 
##'
##' @export 
getHIVEligible <- function(Args, dat=NULL) {
  if(is.null(dat)) dat <- setHIVMiss(Args)
  dat <- mutate(dat, NonContact = as.numeric(
    grepl("Non-[cC]ontact|Refused|Other", Comment)))
  dat <- mutate(dat, Contact = 
    ifelse(HIVRefused==1, "Refused",
    ifelse(HIVRefused==2, "Consent",
    ifelse(NonContact==1, "NonContact", "NotEligible"))))
  dat <- filter(dat, Contact != "NotEligible")
  dat
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
  dat <- getHIVEligible(Args, dat=dat)
  eligible <- group_by(dat, Year) %>% 
    summarize(EligibleN = n())
  non_contact <- filter(dat, Contact=="NonContact") %>% 
    group_by(Year) %>% summarize(NonContactN=n())
  consent <- filter(dat, Contact=="Consent") %>% 
    group_by(Year) %>% summarize(ConsentN=n())
  refused <- filter(dat, Contact=="Refused") %>% 
    group_by(Year) %>% summarize(RefusedN=n())
  out <- Reduce(left_join, list(eligible, non_contact, consent, refused))
  out <- mutate(out,
    ContactedN = EligibleN - NonContactN,
    NonContactPerc = NonContactN/EligibleN,
    RefusePerc = RefusedN/EligibleN,
    ConsentPerc = ConsentN/EligibleN,
    ConsentRate = ConsentN/ContactedN)
  out
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
getHIVCumTest <- function(Args, ntest=1, dat=NULL) {
  if(is.null(dat)) dat <- setHIVMiss(Args)
  # HIVRefusedYes = 1, HIVRefusedNo = 2
  dat <- filter(dat, HIVRefused %in% c(1, 2))
  dat <- mutate(dat, HIVTested = as.numeric(HIVRefused==2))
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
  fmt <- function(x)
    as.numeric(format(as.Date(x, origin="1970-01-01"), "%Y"))
 
  hdat <- getHIV(Args)
  hdat <- getRTData(hdat)
  hdat <- splitAtEarlyPos(hdat) 
  bdat <- getBirthDate(Args$inFiles$epifile)
  adat <- setData(hdat, bdat,  Args)
  year_n <- group_by(adat, Year) %>%  
    summarize(IncN=length(unique(IIntID)))

  hArgs = Args
  hArgs$Years  <- c(2003:2017)
  hiv <- setHIV(hArgs)
  early_neg <- getDatesMin(hiv, "HIVNegative", "early_neg")
  early_neg <- mutate(early_neg, YearNeg = fmt(early_neg)) 
  early_pos <- getDatesMin(hiv, "HIVPositive", "early_pos")
  early_pos <- mutate(early_pos, YearPos = fmt(early_pos)) 
  tdat <- left_join(early_neg, early_pos, by="IIntID") %>%
    select(IIntID, YearNeg, YearPos) 

  # dat <- getHIVContact(Args)
  if(is.null(dat)) dat <- setHIVMiss(Args)
  dat <- left_join(dat, tdat, by=c("IIntID"))
  dat <- arrange(dat, IIntID, VisitDate) 
  # Any negative is eligible
  dat <- group_by(dat, IIntID) %>% mutate(Elig = 
      as.numeric((Year >= YearNeg & !is.na(YearNeg))))
  # But before HIV+
  dat$Elig[dat$Year >= dat$YearPos] <- 0
  elig <- arrange(dat, IIntID, VisitDate) %>%
    group_by(Year) %>% 
    summarize(IncEligN = sum(Elig))
  out <- left_join(elig, year_n, by="Year")
  mutate(out, IncPerc = round(IncN/IncEligN * 100, 2))
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
  edat <- getHIVEligible(Args, dat=dat)
  sdat <- getHIVRefused(Args, dat=edat)
  Eligible = with(sdat, paste0(fmt(ContactedN), "/", fmt(EligibleN)))
  EligiblePerc = paste0("(", rnd(sdat$ContactPerc*100), ")")
  Consent = with(sdat, paste0(fmt(ConsentN), "/", fmt(ContactedN))) 
  ConsentPerc = paste0("(", rnd(sdat$ConsentPerc*100), ")")
  b3 <- getHIVIncEligible(Args, dat=dat)
  b3$Elig <- with(b3, paste0(fmt(IncN), "/", fmt(IncEligN)))
  b3$IncPerc = paste0("(", rnd(b3$IncPerc), ")")
  b4 <- getHIVCumTest(Args, dat=dat)
  b4$CumPerc = paste0("", rnd(b4$TestedPerc), "")
  data.frame(Year=sdat$Year, Eligible, EligiblePerc,
    Consent, ConsentPerc, CumTest=b4$CumPerc,
    IncElig=b3$Elig, IncPerc=b3$IncPerc)
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
  dat <- getHIVRefused(Args)
  dat <- filter(dat, Year != 2017)
  pd <- select(dat, ConsentPerc, RefusePerc, NonContactPerc)
  rate <- dat$ConsentRate
  px <-  t(as.matrix(pd)) 

  if(!is.null(gfun)) {
    gfun(file.path(output,
      paste0(fname, ".", deparse(substitute(gfun)))),
      units="in", width=5.5, height=5.0, pointsize=9, 
      res=200, type="cairo")
  }
  m <- layout(matrix(seq(2), nrow=2), heights=c(8.5, 1))
  YlRed <- RColorBrewer::brewer.pal(9, "YlOrRd")
  xl <- unique(dat$Year)
  xlx <- (seq(length(xl))* 1.2) - 0.5
  par(mar=c(3.8, 4.4, 1.0, 1))
  barplot(px,  col=YlRed[c(4, 6, 8)], width=1, space=0.2,
    xlab="Year", ylab="Proportion", font.lab=2)
  lines(xlx, rate, col=YlRed[9], lwd=4)
  plotrix::staxlab(side=1, at=xlx, labels=xl, srt=45)
  consent = px["ConsentPerc", ]
  refuse = px["RefusePerc", ]
  nonc = px["NonContactPerc", ]
  text(xlx, y=0.15, labels=round(consent, 2), col="white", font=2, pos=1)
  text(xlx, y=refuse+consent, labels=round(refuse, 2), col="white", font=2, pos=1)
  text(xlx, y=1, labels=round(nonc, 2), col="white", font=2, pos=1)
  text(xlx, y=rate-0.05, labels=round(rate, 2), col=YlRed[9], pos=1, font=2)
  par(mar=c(1.1, 4.4, 1.0, 1))
  plot.new()
  legend("bottom", c("Consent rate", "Consent", "Refuse", "Non-Contact"),
    ncol=4, bty="n", inset=c(0, 0), pch=c(NA, rep(15, 3)), pt.cex=3, 
    lty=c(1, rep(NA, 3)), lwd=6, col=c(YlRed[c(9, 4, 6, 8)]))
  if(!is.null(gfun)) dev.off()
}



