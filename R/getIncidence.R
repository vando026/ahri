#' @title getIncData
#' 
#' @description Function used to prepare the data for \code{\link{getIncidence}}.
#' 
#' @param rtdat dataset from \code{\link{getRTData}}. 
#' @param idat dataset from \code{\link{getBirthData}}. 
#' @param Args takes list from \code{\link{setArgs}}.
#'
#' @export
#' 
#' @return data.frame
#' @examples
#' hiv   <- getHIV(Args)
#' rtdat <- getRTData(hiv)
#' idat <- getBirthDate(Args$inFiles$epifile)
#' dat <- Args$imputeMethod(rtdat)
#' edat <- splitAtSeroDate(dat, splitYears=Args$Years) 
#' adat <- getAgeData(edat, idat,  Args)
#' getIncData(rtdat, idat, Args)

getIncData <- function(rtdat, idat, Args) {
  dat <- Args$imputeMethod(rtdat)
  edat <- splitAtSeroDate(dat, splitYears=Args$Years) 
  adat <- getAgeData(edat, idat,  Args)
  adat
}

#' @title AggFunc
#' 
#' @description Function to create aggregates of sero events and pyears by Var
#' 
#' @param Var as in Sex or AgeCat
#' 
#' @return data.frame

AggFunc <- function(Var) {
  function(dat) {
    F1 <- as.formula(paste(
      "cbind(sero_event, pyears=Time/365.25) ~ ", Var))
    aggregate(F1, data=dat, FUN=sum)
  }
}

#' @title AggByYear
#' 
#' @description Aggregates sero events and pyears by Year.
#' 
#' @param dat dataset.
#' 
#' @return data.frame
#'
#' @export
#' 
#' @examples
#' dat <- getIncData(rtdat, idat, Args)
#' AggByYear(dat)

AggByYear <- AggFunc("Year")

#' @title AggByAge
#' 
#' @description Aggregates sero events and pyears by AgeCat.
#' 
#' @param dat dataset. 
#' 
#' @return data.frame
#'
#' @export
#' 
#' @examples
#' dat <- getIncData(rtdat, idat, Args)
#' AggByAge(dat)

AggByAge <- AggFunc("AgeCat")

#' @title getAggData
#' 
#' @description Function to create aggregates of sero events and pyears by Var
#' 
#' @param Var as in Sex or AgeCat
#' 
#' @return data.frame

getAggData <- function(dat) {
  getDat <- function(name) {
    function(dat) {
      dat <- lapply(dat, `[`, name)
      do.call("cbind", dat)
    }
  }
  getSero <- getDat("sero_event")
  getPYear <- getDat("pyears")
  lapply(c(getSero, getPYear), 
    function(f) f(dat))
}


#' @title doPoisYear
#' 
#' @description DO poisson regression for incidence rates by year or age. 
#' 
#' @param dat takes dataset from a function (i.e., \code{doIncData}.)
#'
#' @return data.frame
#' @examples
#' doPoisYear <- poisFunc("as.factor(Year)")
#' doPoisAge <- poisFunc("as.factor(AgeCat)")

doPoisYear <- function(dat) {
  dat$tscale <- dat$Time/365.25
  mod <- glm(sero_event ~ as.factor(Year) + Age + offset(log(tscale)),
    data=dat, family=poisson)
  nyears <- seq(unique(dat$Year))
  ndat <- data.frame(Age = mean(dat$Age), tscale=1,
    Year = factor(nyears, levels = nyears, labels = levels(as.factor(dat$Year))))
  predict.glm(mod, ndat, type="response", se.fit=TRUE)[c("fit", "se.fit")]
}

doPoisAge <- function(dat) {
  dat$tscale <- dat$Time/365.25
  mod <- glm(sero_event ~ AgeCat + offset(log(tscale)),
    data=dat, family=poisson)
  nage <- seq(unique(dat$AgeCat))
  ndat <- data.frame(tscale=1,
    AgeCat = factor(nage, levels = nage, labels = levels(as.factor(dat$AgeCat))))
  predict.glm(mod, ndat, type="response", se.fit=TRUE)[c("fit", "se.fit")]
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
#' @export
#' 
#' @examples

calcInc <- function(rtdat, idat, Args) {
  dat <- getIncData(rtdat, idat, Args)
  nm <- c("Year", "Age", "poisYear", "poisAge")
  funs <- list(AggByYear, AggByAge, doPoisYear, doPoisAge)
  adat <- lapply(setNames(funs, nm), function(f) f(dat))
  adat
}


#' @title getEstFunc
#' 
#' @description gets estimates from \code{\link{calcInc}}.
#' 
#' @param obs the object name.
#' 
#' @param name the name of vector.
#' 
#' @return function

getEstFunc <- function(obj, name) {
  function(dat, nsim=Args$nSim) {
    sapply(seq(nsim),
      function(i) dat[[i]][[obj]][[name]] )
  }
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
#' @export
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

getEstimates <-  function(dat, nsim=2) {
  funs <- list(
    getSero = getEstFunc("Year", "sero_event"),
    getPYear = getEstFunc("Year", "pyears"),
    getAgeSero = getEstFunc("Age", "sero_event"),
    getAgePYear = getEstFunc("Age", "pyears"),
    getPoisYearEst = getEstFunc("poisYear", "fit"),
    getPoisYearSE = getEstFunc("poisYear", "se.fit"),
    getPoisAgeEst = getEstFunc("poisAge", "fit"),
    getPoisAgeSE = getEstFunc("poisAge", "se.fit")
  )
  lapply(funs, function(f) f(dat)) 
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


#' @title getIncidence
#' 
#' @description Calculates the crude and adjusted incidence.
#' 
#' @param Args takes list from \code{\link{setArgs}}
#'
#' @return data.frame
#'
#' @export
#'
#' @import parallel

getIncidence <- function(Args) {
  hiv   <- getHIV(Args)
  rtdat <- getRTData(hiv)
  idat <- getBirthDate(Args$inFiles$epifile)
  dat <- mclapply(seq(Args$nSimulations), 
    function(i) calcInc(rtdat, idat, Args), 
    mc.cores=Args$mcores)
  dat
  # out <- getEstimates(dat) 
  # out
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
#' smoothInc(dat, x="time", y="rate")

smoothInc <- function(dat, x="time", y="rate", bwidth=1) {
  dat <- as.data.frame(dat)
  with(dat, ksmooth(x,  y, "normal", bandwidth = bwidth))
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

