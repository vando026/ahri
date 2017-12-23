#' @title aggregateInc
#' 
#' @description Gets the aggregated sero_event counts and total person-years.
#' 
#' @param dat dataset from \code{\link{censorData}}. 
#' 
#' @return data.frame
#'
#' @examples
#' edat <- censorData(rtdat, Args)
#' adat <- getAgeData(edat, Args)
#' inc <- aggregateInc(adat)

aggregateInc <- function(dat) {
  aggregate(cbind(sero_event, pyears=Time/365.25) ~ 
    Year+AgeCat+Female, data=dat, FUN=sum)
}

#' @title doIncData
#' 
#' @description Function used to prepare the data for \code{\link{getIncidence}}.
#' 
#' @param rtdat dataset from \code{\link{getRTData}}. 
#' 
#' @param Args takes list from \code{\link{setArgs}}.
#'
#' @return data.frame

doIncData <- function(rtdat, Args) {
  dat <- Args$imputeMethod(rtdat)
  edat <- censorData(dat, Args) 
  adat <- getAgeData(edat, Args)
  idat <- aggregateInc(adat)
  idat
}

#' @title calcInc
#' 
#' @description Calculates the crude and adjusted incidence.
#' 
#' @param dat Dataset from \code{\link{aggregateInc}}.
#' 
#' @param wdat Dataset of weights from \code{\link{getWeightsKZN}}.
#' 
#' @param calcBy Results by Year, Age, or Sex.
#'
#' @return data.frame
#'
#' @importFrom epitools ageadjust.direct
#'
#' @examples
#' edat <- censorData(rtdat,Args)
#' adat <- getAgeData(edat, Args)
#' inc <- aggregateInc(adat)
#' wdat <- getWeightsKZN() 
#' calcInc(inc, wdat)

calcInc <- function(dat, wdat, calcBy="Year") { 
  # dat <- merge(dat, wdat, by="AgeCat")
  dat$AgeCat <- factor(dat$AgeCat)
  dat$Total <- 1 #remove, only for test
  dat <- split(dat, dat[calcBy])
  dat <- sapply(dat, function(x) ageadjust.direct(
    x["sero_event"],x["pyears"],stdpop=x["Total"]))
  dat <- data.frame(t(dat)*100)
  dat
}

sumEst <- function(dat, name) {
  dat <- lapply(dat, `[`, name)
  out <- do.call("cbind", dat)
  out <- data.frame(t(apply(out, MARGIN=1, 
    FUN=quantile, probs=c(0.5, 0.025, 0.975))))
  names(out) <- c("rate", "lower", "upper")
  out
}

getAggData <- function(dat, calcBy="Year") {
  dat <- lapply(dat, function(x)
    aggregate(x[, c("sero_event", "pyears")], 
      by=list(x[, calcBy]), FUN=sum))
  sero <- sumEst(dat, "sero_event")
  pyears <- sumEst(dat, "pyears")
  list(sero=sero, pyears=pyears)
}

getRate <- function(dat) {
  crude <- sumEst(dat, "crude.rate")
  adj <- sumEst(dat, "adj.rate")
  list(crude.rate=crude, adj.rate=adj)
}

#' @title getEstimates
#' 
#' @description Once dates are imputed get the estimates.
#' 
#' @param dat takes dataset from a function (i.e., \code{doIncData}.)
#'
#' @param Args takes list from \code{\link{setArgs}}.
#'
#' @return data.frame
#'
#' @import dplyr
#'
#' @examples
#' getIncidence <- function(Args) {
#'   hiv   <- getHIV(Args)
#'   rtdat <- getRTData(hiv)
#'   set.seed(Args$Seed)
#'   dat <- lapply(seq(Args$nSimulations),
#'     function(i) doIncData(rtdat, Args))
#'   out <- getEstimates(dat, Args) 
#'   out
#' }

getEstimates <- function(dat, Args) {

  # wdat <- getWeights(Args)
  wdat <- 1
  Year <- lapply(dat, 
    function(x) calcInc(x, wdat, calcBy="Year"))

  Age <- lapply(dat, 
    function(x) calcInc(x, wdat, calcBy="AgeCat"))
  
  if (Args$nSimulations==1) 
    return(list(dat=dat, Year=Year, Age=Age))

  dat_year <- getAggData(dat, calcBy="Year")
  dat_age <- getAggData(dat, calcBy="AgeCat")
  est_year <- getRate(Year)
  est_age <- getRate(Age)
  list(YearD=dat_year, AgeD=dat_age,
    Year=est_year, Age=est_age)
}

#' @title getIncidence
#' 
#' @description Calculates the crude and adjusted incidence.
#' 
#' @param Args takes list from \code{\link{setArgs}}.
#'
#' @return data.frame
#'
#' @import dplyr

getIncidence <- function(Args) {
  hiv   <- getHIV(Args)
  rtdat <- getRTData(hiv)
  set.seed(Args$Seed)
  dat <- lapply(seq(Args$nSimulations),
    function(i) doIncData(rtdat, Args))
  out <- getEstimates(dat, Args) 
  out
}

#' @title smoothInc
#' 
#' @description get smoothes incidence estimates.
#' 
#' @param dat takes dataset from \code{\link{getIncidence}}.
#'
#' @param bwidth bandwith for \code{ksmooth} function. 
#'
#' @return data.frame

smoothInc <- function(dat, bwidth=1) {
  dat <- as.data.frame(dat)
  Year <- as.integer(rownames(dat))
  ks <-  ksmooth(Year, dat$rate,
    "normal", bandwidth = bwidth)
  ks
}

#' @title incTab
#' 
#' @description Make table for excel.
#' 
#' @param obj takes dataset. 
#'
#' @param age Do for age or year.
#'
#' @return data.frame

incTab <- function(obj, Age=FALSE) {
  if (Age==FALSE)
    with(obj, cbind(YearD$sero, YearD$pyears, Year$adj.rate))
  else
    with(obj, cbind(AgeD$sero, AgeD$pyears, Age$adj.rate))
}

#' @title saveInc
#' 
#' @description Save to .Rdata file.
#' 
#' @param obj takes object. 
#'
#' @param out File path to write. 

saveInc <- function(obj, out=output) {
  save(obj, file=file.path(output, 
    paste0(deparse(substitute(obj)), ".Rdata")))
}
