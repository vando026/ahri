#' @title aggregateInc
#' 
#' @description Gets the aggregated sero_event counts and total person-years by Year, Age,
#' and sex.
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
  aggregateInc(adat)
}

gammaCI <- function(x) {
  epitools::ageadjust.direct(x["sero_event"],x["pyears"],stdpop=x["Total"]) * 100
}


#' @title getAdjRate
#' 
#' @description Calculates adjusted incidence rate and standard errors following formula in Boyle and Parkin, 
#' (https://www.iarc.fr/en/publications/pdfs-online/epi/sp95/sp95-chap11.pdf) on page 137.
#' 
#' @return data.frame

getAdjRate <- function(x, crude=FALSE) {
  ry <- 100
  # Calculate age specific rates
  x$rate <- with(x, (sero_event/pyears))
  adj.rate <- sum(with(x, rate * (Total/sum(Total)))) * ry
  # Calculate adjusted SE
  numer <- with(x, sum((rate*ry*(Total^2)*ry)/pyears))
  denom <- (sum(x$Total))^2
  adj.se <- sqrt(numer/denom)
  c(rate=adj.rate, se=adj.se)
}

#' @title getCrudeRate
#' 
#' @description Calculates crude incidence rate and standard errors following work of Ulm, 1990, AJE.
#' 
#' @return data.frame

getCrudeRate <- function(x) {
  ry <- 100
  rate <- with(x, sum(sero_event)/sum(pyears)) * ry
  se <- rate/sqrt(sum(x$sero_event))
  c(rate=rate, se=se)
}

#' @title calcInc
#' 
#' @description Calculates the crude and adjusted incidence.
#' 
#' @param dat Dataset from \code{\link{aggregateInc}}.
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
#' calcInc(inc, Args)

calcInc <- function(dat, wdat, Args, 
  calcBy="Year", fun=getAdjRate) { 
  dat <- merge(dat, wdat, by=c("Year", "AgeCat"))
  dat$AgeCat <- factor(dat$AgeCat)
  dat <- split(dat, dat[calcBy])
  dat <- sapply(dat, fun)
  data.frame(t(dat))
}


getAggData <- function(dat, Args, calcBy="Year") {
  getDat <- function(dat, name) {
    dat <- lapply(dat, `[`, name)
    do.call("cbind", dat)
  }
  nm <- c("sero_event", "pyears")
  dat <- lapply(dat, function(x)
    aggregate(x[, nm], by=list(x[, calcBy]), FUN=sum))
  adat <- lapply(nm, function(x) getDat(dat, x))
  adat <- lapply(adat, function(x) apply(x, MARGIN=1, mean))
  out <- do.call("cbind", adat)
  colnames(out) <- nm
  rownames(out) <- dat[[1]]$Group.1
  out
}


#' @title doMIEst
#' 
#' @description Calculates the standard errors for multiple imputation following formula given in P. Allison Missing Data
#' book, pg 30, in '~/Dropbox/Textbooks/Statistics'.
#' 
#' @return data.frame

doMIEst <- function(dat, 
  Args=eval.parent(quote(Args))) {
  
  getError <- function(x, M=Args$nSimulations) {
    x <- as.data.frame(x)
    # M <- Args$nSimulations
    var1 <- sum(x$se^2)/M
    var2 <- with(x, sum((rate - mean(rate))^2))
    se <- sqrt(var1 + ((1+(1/M)) * (1/(M-1) * var2)))
    est <- mean(x$rate)
    ci <- est + c(-1, 1) * qnorm(0.05/2, lower=FALSE) * se
    c(rate=est, se=se, lci=ci[1], uci=ci[2])
  }

  # Group data by year
  nm <- rownames(dat[[1]])
  collect <- lapply(seq(nm),
    function(y) lapply(dat, function(x) x[y, ]))
  bind <- lapply(collect, function(x) do.call("rbind", x))
  out <- lapply(bind, getError)
  out <- do.call("rbind", out)
  rownames(out) <- nm
  out
}

#' @title doSIEst
#' 
#' @description Calculates the standard errors for single imputation such as mid-point or
#' end-point.
#' 
#' @return data.frame

doSIEst <- function(x) {
  x <- data.frame(x)
  z <- qnorm(0.05/2, lower=FALSE)
  x$lci <- x$rate - z * x$se
  x$uci <- x$rate + z * x$se
  x
}


#' @title getEstimates
#' 
#' @description Once dates are imputed get the estimates.
#' 
#' @param dat takes dataset from a function (i.e., \code{doIncData}.)
#'
#' @param Args takes list from \code{\link{setArgs}}.
#' 
#' @param By calculate by Year or AgeCat.
#'
#' @return data.frame
#'
#' @import dplyr
#'
#' @examples
#' getIncidence <- function(Args) {
#'   hiv   <- getHIV(Args)
#'   rtdat <- getRTData(hiv)
#'   dat <- lapply(seq(Args$nSimulations),
#'     function(i) doIncData(rtdat, Args))
#'   out <- getEstimates(dat, Args) 
#'   out
#' }

getEstimates <- function(dat, Args, By='Year') {

  # Get events and pyears by year 
  aggdat <- getAggData(dat, Args, calcBy=By)
  # Calc weights once here
  wdat <- getWeights(Args)
  # For each iteration of dat, merge weight and calc inc
  crate <- lapply(dat, 
    function(x) calcInc(x, wdat, Args, calcBy=By, fun=getCrudeRate))
  arate <- lapply(dat, 
    function(x) calcInc(x, wdat, Args, calcBy=By, fun=getAdjRate))

  getEst <- function(fun) {
      list(AggDat = aggdat, 
        CrudeRate = fun(crate),
        AdjRate = fun(arate))
  }
  
  if (Args$nSimulations==1) {
  # For mid- or end-point imputation
    getEst(doSIEst)
  } else {
  # For random-point imputation
    getEst(doMIEst) 
  }
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
  dat <- lapply(seq(Args$nSimulations),
    function(i) doIncData(rtdat, Args))
  Year <- getEstimates(dat, Args) 
  Age <- getEstimates(dat, Args, By='AgeCat') 
  list(Year=Year, Age=Age)
}

#' @title smoothInc
#' 
#' @description get smoothed incidence estimates.
#' 
#' @param dat takes dataset from \code{\link{getIncidence}}.
#'
#' @param bwidth bandwith for \code{ksmooth} function. 
#'
#' @return data.frame
#' @examples
#' smoothInc(dat$Year$Est$adj.rate)

smoothInc <- function(dat, bwidth=1) {
  dat <- as.data.frame(dat)
  ksmooth(
    as.integer(rownames(dat)),
    dat[, grep("rate", colnames(dat))],
    "normal", bandwidth = bwidth)
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

incTab <- function(root="", Age=FALSE) {
  if (Age==FALSE)
    with(get(root, envir=globalenv()),
      cbind(Agg$sero, Agg$pyears, Est$crude.rate, Est$adj.rate))
  else
    with(get(root, envir=globalenv()),
      cbind(Agg$sero, Agg$pyears, Est$adj.rate))
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
