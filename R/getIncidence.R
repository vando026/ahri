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
  idat <- aggregateInc(adat)
  idat
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

calcInc <- function(dat, wdat, Args, calcBy="Year") { 
  dat <- merge(dat, wdat, by=c("Year", "AgeCat"))
  dat$AgeCat <- factor(dat$AgeCat)
  dat <- split(dat, dat[calcBy])
  dat <- sapply(dat, function(x) ageadjust.direct(
    x["sero_event"],x["pyears"],stdpop=x["Total"]))
  dat <- data.frame(t(dat)*100)
  dat
}


sumEst_qtile <- function(out) {
  out <- sapply(c(0.5, 0.025, 0.975),
    function(x) apply(out, MARGIN=1, FUN=quantile, x))
  colnames(out) <- c("rate", "lower", "upper")
  data.frame(out)
}

sumEst_sd <- function(out) {
  out <- sapply(c(mean, sd),
    function(x) apply(out, MARGIN=1, FUN=x))
  colnames(out) <- c("rate", "sd")
  data.frame(out)
}

sumEst <- function(dat, name, Args) {
  dat <- lapply(dat, `[`, name)
  out <- do.call("cbind", dat)
  Args$sumEstRule(out)
}

getAggData <- function(dat, Args, calcBy="Year") {
  nm <- c("sero_event", "pyears")
  dat <- lapply(dat, function(x)
    aggregate(x[, nm], by=list(x[, calcBy]), FUN=sum))
  lapply(setNames(nm, nm), 
    function(x) sumEst(dat, x, Args))
}

getRate <- function(dat, Args) {
  nm <- c("crude.rate", "adj.rate")
  lapply(setNames(nm, nm),
    function(x) sumEst(dat, x, Args)) 
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
#'   set.seed(Args$Seed)
#'   dat <- lapply(seq(Args$nSimulations),
#'     function(i) doIncData(rtdat, Args))
#'   out <- getEstimates(dat, Args) 
#'   out
#' }

getEstimates <- function(dat, Args, By='Year') {

  # Calc weights once here
  wdat <- getWeights(Args)
  # For each iteration of dat, merge weight and calc inc
  ldat <- lapply(dat, 
    function(x) calcInc(x, wdat, Args, calcBy=By))

  if (Args$nSimulations==1) 
    return(as.data.frame(ldat))

  aggdat <- getAggData(dat, Args, calcBy=By)
  estdat <- getRate(ldat, Args)
  list(Agg=aggdat, Est=estdat)
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
