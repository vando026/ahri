#' @title aggregateInc
#' 
#' @description Gets the aggregated sero_event counts and total person-years by Year, Age,
#' and sex.
#' 
#' @param dat dataset from \code{\link{splitImputeData}}. 
#' 
#' @return data.frame
#'
#' @examples
#' edat <- splitImputeData(rtdat, Args)
#' adat <- getAgeData(edat, Args)
#' inc <- aggregateInc(adat)

aggregateInc <- function(dat) {
  aggregate(cbind(sero_event, pyears=Time/365.25) ~ 
    Year+AgeCat+Female, data=dat, FUN=sum)
}

#' @title getIncData
#' 
#' @description Function used to prepare the data for \code{\link{getIncidence}}.
#' 
#' @param rtdat dataset from \code{\link{getRTData}}. 
#' 
#' @param Args takes list from \code{\link{setArgs}}.
#'
#' @return data.frame

getIncData <- function(rtdat, wdat, Args) {
  idat <- Args$imputeMethod(rtdat)
  edat <- splitImputeData(idat, Args$Years) 
  adat <- getAgeData(edat, Args)
  adat <- aggregateInc(adat)
  dat <- merge(adat, wdat, by=c("Year", "AgeCat"))
  dat$AgeCat <- factor(dat$AgeCat)
  dat
}

getAggData <- function(dat, Args, 
  calcBy=eval.parent(quote(By))) {
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


#' @title calcInc
#' 
#' @description Calculates the crude and adjusted incidence.
#' 
#' @param dat Dataset from \code{\link{aggregateInc}}.
#' 
#' @param fun Function to use for calculations
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
#' calcInc(inc, getCrudeMI, calcBy="Year")

calcInc <- function(dat, fun, calcBy="Year") {
  dat <- data.frame(dat)
  dat <- split(dat, dat[calcBy])
  fun(dat)
}

#' @title getCrudeMI
#' 
#' @description Calculates the rate and var according to code in
#' epitools::ageadjust.direct
#' 
#' @return data.frame

getCrudeMI <- function(dat) {
  crude <- function(x) {
    sero <- sum(x$sero_event)
    pyears <- sum(x$pyears)
    dsr <- sero/pyears
    dsr.var <- sero/pyears^2
    c(dsr=dsr, dsr.var=dsr.var, wm=0)
  }
  t(sapply(dat, crude))
}

#' @title getAdjMI
#' 
#' @description Calculates the rate and var according to code in
#' epitools::ageadjust.direct
#' 
#' @return data.frame

getAdjMI <- function(dat) {
  adj <- function(x) {
    stdwt <- with(x, Total/sum(Total))
    rate <- with(x, sero_event/pyears)
    dsr <- sum(stdwt * rate)
    dsr.var <- sum((stdwt^2) * with(x, sero_event/pyears^2))
    wm <- max(stdwt/x$pyears)
    c(dsr=dsr, dsr.var=dsr.var, wm=wm)
  }
  t(sapply(dat, adj))
}

#' @title getEstMI
#' 
#' @description Calculates the standard errors for multiple imputation following formula given in P. Allison Missing Data
#' book, pg 30, in '~/Dropbox/Textbooks/Statistics'.
#' 
#' @return data.frame

getEstMI  <- function(dat, fun,
  Args=eval.parent(quote(Args)), 
  calcBy=eval.parent(quote(By))) {

  getCI <- function(x, M=Args$nSimulations) {
    x <- as.data.frame(x)
    var1 <- sum(x$dsr.var)/M
    var2 <- with(x, sum((dsr - mean(dsr))^2))
    dsr.var <- var1 + ((1+(1/M)) * (1/(M-1) * var2)) 
    dsr <- mean(x$dsr)
    wm <- mean(x$wm)
    gamma.lci <- qgamma(0.05/2, shape = (dsr^2)/dsr.var, 
      scale = dsr.var/dsr)
    gamma.uci <- qgamma(1 - 0.05/2, shape = ((dsr + wm)^2)/(dsr.var + wm^2),
      scale = (dsr.var + wm^2)/(dsr + wm))
    c(rate = dsr, lci = gamma.lci, uci = gamma.uci)*100
  }

  # Get rate and vars by iteration
  dat <- lapply(dat, 
    function(x) calcInc(x, fun, calcBy=calcBy))

  # Group data by year
  nm <- rownames(dat[[1]])
  collect <- lapply(seq(nm),
    function(y) lapply(dat, function(x) x[y, ]))
  bind <- lapply(collect, function(x) do.call("rbind", x))
  out <- lapply(bind, getCI)
  out <- do.call("rbind", out)
  rownames(out) <- nm
  out
}

#' @title getEstSI
#' 
#' @description Calculates the rate and confidence intervals for single imputation such as mid-point or
#' end-point.
#' 
#' @return data.frame

getEstSI <- function(dat, fun,
    calcBy=eval.parent(quote(By))) {
    out <- calcInc(dat, fun, calcBy=calcBy)
    out <- do.call("rbind", out)
    names(out) <- c("rate", "lci", "uci")
    out
}

getAdjSI <- function(x) {
  getEst <- function(x) {
  as.data.frame(t(with(x, 
    ageadjust.direct(sero_event, pyears,
      stdpop=Total)[2:4] * 100)))
  }
  lapply(x, getEst)
}

getCrudeSI <- function(x) {
  getEst <- function(x) {
    with(x, pois.exact(sum(sero_event), 
      sum(pyears))[3:5] * 100)
 }
  lapply(x, getEst)
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

getEstimates <- function(dat, Args, By="Year") {

  # Get events and pyears by year 
  aggdat <- getAggData(dat, Args)

  if (Args$nSimulations==1) {
    crude <- getEstSI(dat, getCrudeSI)
    adj <- getEstSI(dat, getAdjSI)
  } else {
    crude <- getEstMI(dat, getCrudeMI)
    adj <- getEstMI(dat, getAdjMI)
  }
  list(AggDat = aggdat, CrudeRate = crude, AdjRate = adj)
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
  wdat <- getWeights(Args)
  hiv   <- getHIV(Args)
  rtdat <- getRTData(hiv)
  dat <- lapply(seq(Args$nSimulations),
    function(i) getIncData(rtdat, wdat, Args))
  Year <- getEstimates(dat, Args) 
  Age <- getEstimates(dat, Args, By='AgeCat') 
  list(Year=Year, Age=Age)
}


###############################################################################################
######################################## Misc Inc Funs ########################################
###############################################################################################

#' @title smoothInc
#' 
#' @description get smoothed incidence estimates.
#' 
#' @param dat takes dataset from \code{\link{getIncidence}}.
#'
#' @param bwidth bandwith for \code{ksmooth} function. 
#'
#' @return data.frame
#'
#' @export
#'
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

incTab <- function(obj) {
  with(obj, cbind(AggDat, CrudeRate, AdjRate))
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
