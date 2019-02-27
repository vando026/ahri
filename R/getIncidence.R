#' @title getIncData
#' 
#' @description Function that imputes sero date, splits at censoring date and set the age.
#' 
#' @param rtdat dataset from \code{\link{getRTData}}. 
#' @param bdat dataset from \code{\link{getBirthDate}}. 
#' @param Args takes list from \code{\link{setArgs}}.
#'
#' @return data.frame
#' 
#' @export
#'
#' @examples
#' hiv   <- getHIV(Args)
#' rtdat <- getRTData(hiv)
#' bdat <- getBirthDate(Args$inFiles$epifile)
#' getIncData(rtdat, bdat, Args)

getIncData <- function(rtdat, bdat, Args) {
  dat <- Args$imputeMethod(rtdat)
  edat <- splitAtSeroDate(dat) 
  setData(edat, bdat,  Args)
}

#' @title AggFunc
#' 
#' @description Function to create aggregates of sero events and pyears by right-hand side
#' formula.
#' 
#' @param RHS right hand side of the formula, as in Sex or AgeCat or "Sex + Cat".
#' 
#' @return data.frame
#' 
#' @examples
#' AggByYear <- AggFunc("Year")
#' AggByAge <- AggFunc("AgeCat")
#' # Show for one imputation 
#' Args <- setArgs(nSim=1, imputeMethod=imputeEndPoint)
#' hiv   <- getHIV(Args)
#' rtdat <- getRTData(hiv)
#' bdat <- getBirthDate(Args$inFiles$epifile)
#' ydat <- getIncData(rtdat, bdat, Args)
#' inc <- AggByYear(ydat)

AggFunc <- function(RHS) {
  function(dat) {
    F1 <- as.formula(paste(
      "cbind(sero_event, pyears=Time/365.25) ~ ", RHS))
    out <- aggregate(F1, data=dat, FUN=sum)
    rownames(out) <- out[[1]]
    out
  }
}

AggByYear <- AggFunc("Year")
AggByAge <- AggFunc("AgeCat")


getPredYear <- function(Args) {
  dat <- setHIV(Args)
  mn_age <- tapply(dat$Age, dat$Year, mean)
  nyears <- sort(unique(dat$Year))
  data.frame(Age = mn_age, tscale=1,
    Year = factor(nyears, levels = nyears, 
    labels = levels(as.factor(dat$Year))))
}

#' @title doPoisYear
#' 
#' @description Do poisson regression for incidence rates by year.
#' 
#' @param dat Dataset from a function \code{\link{getIncData}}.
#'
#' @return data.frame
#'
#' @export
doPoisYear <- function(ndata) {
  function(dat) {
    dat$tscale <- dat$Time/365.25
    mod <- glm(sero_event ~ -1 + as.factor(Year) + Age + as.factor(Year):Age 
      + offset(log(tscale)), data=dat, family=poisson)
    data.frame(predict.glm(mod, ndata, se.fit=TRUE)[c(1,2)])
  }
}

#' @title doPoisAge
#' 
#' @description Do poisson regression for incidence rates by age. 
#' 
#' @param dat Dataset from a function \code{\link{getIncData}}.
#'
#' @return data.frame
#'
#' @export
doPoisAge <- function(dat) {
  dat$tscale <- dat$Time/365.25
  mod <- glm(sero_event ~ AgeCat + offset(log(tscale)),
    data=dat, family=poisson)
  nage <- seq(unique(dat$AgeCat))
  ndat <- data.frame(tscale=1,
    AgeCat = factor(nage, levels = nage, labels = levels(dat$AgeCat)))
  out <- data.frame(predict.glm(mod, ndat, se.fit=TRUE)[c(1, 2)])
  rownames(out) <- ndat$AgeCat
  out
}

#' @title calcInc
#' 
#' @description Calculates the crude and adjusted incidence.
#' 
#' @param rtdat Dataset from \code{\link{getRTData}}.
#' 
#' @param bdat Dataset from \code{\link{getBirthDate}}.
#' 
#' @param Args provide arguments from \code{\link{setArgs}}.
#' 
#' @return data.frame
#'
#' @export
#' 
#' @examples
#' hiv   <- getHIV(Args)
#' rtdat <- getRTData(hiv)
#' bdat <- getBirthDate(Args$inFiles$epifile)
#' calcInc(rtdat, bdat, Args)

calcInc <- function(rtdat, bdat, pdat, Args) {
  dat <- getIncData(rtdat, bdat, Args)
  doPoisYear_ <- doPoisYear(pdat)
  nm <- c("Year", "Age", "poisYear", "poisAge")
  funs <- list(AggByYear, AggByAge, doPoisYear_, doPoisAge)
  lapply(setNames(funs, nm), function(f) f(dat))
}

#' @title combineEst
#' 
#' @description Collect all estimates into single matrix
#' 
#' @param dat Dataset from \code{\link{calcInc}}.
#'
#' @return list 
#'
#' @export
#'
#' @examples
#' hiv   <- getHIV(Args)
#' rtdat <- getRTData(hiv)
#' bdat <- getBirthDate(Args$inFiles$epifile)
#' dat <- mclapply(seq(Args$nSim), 
#'   function(i) calcInc(rtdat, bdat, Args), 
#'   mc.cores=Args$mcores)
#' cdat <- combineEst(dat) 

combineEst <-  function(dat) {
  getEst <- function(dat, obj) {
    out <- sapply(dat, "[[", obj)
    rownames(out) <- rownames(dat[[1]][[obj[1]]])
    out
  }
  nms <- list(
    getSero = c("Year", "sero_event"),
    getPYear = c("Year", "pyears"),
    getAgeSero = c("Age", "sero_event"),
    getAgePYear = c("Age", "pyears"),
    getPoisYearEst = c("poisYear", "fit"),
    getPoisYearSE = c("poisYear", "se.fit"),
    getPoisAgeEst = c("poisAge", "fit"),
    getPoisAgeSE = c("poisAge", "se.fit"))
  lapply(nms, function(x) getEst(dat, x))
}

#' @title getCrudeRate
#' 
#' @description Calculate crude incidence rate estimates using the standard formula. 
#' 
#' @param dat Dataset from \code{\link{combineEst}}. 
#' 
#' @return  data.frame
#'
#' @export 
#'
#' @examples
#' Args <- setArgs(nSim=10)
#' hiv   <- getHIV(Args)
#' rtdat <- getRTData(hiv)
#' bdat <- getBirthDate(Args$inFiles$epifile)
#' dat <- mclapply(seq(Args$nSim), 
#'   function(i) calcInc(rtdat, bdat, Args))
#' cdat <- combineEst(dat)
#' getCrudeRate(cdat)

getCrudeRate <- function(dat) {
  out <- lapply(dat, rowMeans)
  Year <- data.frame(sero=out$getSero, pyears=out$getPYear)
  Age <- data.frame(sero=out$getAgeSero, pyears=out$getAgePYear)
  getInc <- function(dat)  dat$sero/dat$pyear * 100 
  lapply(list(Year=Year, Age=Age), 
    function(x) {x$rate = getInc(x); x})
}


#' @title calcRubin
#' 
#' @description  Calculates standard error according to Rubin's rules. 
#' 
#' @param est Estimates from m imputations
#' @param se Estimates from m imputations
#' 
#' @return 
#'
#' @export 
#'
#' @examples
#' e <- c(2, 4) # m = 2
#' s <- c(0.5, 0.5) # m = 2
#' x <- calcRubin(e, s)

calcRubin <- function(est, se) {
  m <- length(est)
  mn <- mean(est)
  if (m > 1) {
    var_with <- mean(se^2)
    var_betw <- sum((est - mn)^2)/(m-1)
    se <- sqrt(var_with + var_betw*(1 + (1/m)))
    rdf <- (m - 1) * (1 + (var_with/((1+ (1/m)) * var_betw)))^2
    tdf <- qt(1 - (0.05/2), rdf)
  } else {
    tdf <- 1.96 
  }
  ci <- exp(mn + c(-1, 1) * (tdf * se))
  c(rate=exp(mn), se=se, lci=ci[1], uci=ci[2])
}


#' @title getAdjRate
#' 
#' @description Calculate adjusted incidence rate estimates using Poisson models and
#' Rubin's rules
#' 
#' @param dat Dataset from \code{\link{combineEst}}. 
#' 
#' @return  data.frame
#'
#' @export 
#'
#' @examples
#' Args <- setArgs(nSim=10)
#' hiv   <- getHIV(Args)
#' rtdat <- getRTData(hiv)
#' bdat <- getBirthDate(Args$inFiles$epifile)
#' dat <- mclapply(seq(Args$nSim), 
#'   function(i) calcInc(rtdat, bdat, Args))
#' cdat <- combineEst(dat)
#' geteAdjRate(cdat)

getAdjRate <- function(dat) {
  calcPredict <- function(est, se) {
    est1 <- split(est, rownames(est))
    se <- split(se, rownames(se))
    out <- Map(calcRubin, est1, se)
    dat <- data.frame(do.call(rbind, out))
    dat[] <- lapply(dat[], `*`, 100)
    rownames(dat) <- rownames(est)
    dat
  }
  list(
    Year = calcPredict(dat[["getPoisYearEst"]], dat[["getPoisYearSE"]]),
    Age = calcPredict(dat[["getPoisAgeEst"]], dat[["getPoisAgeSE"]])
  )
}

#' @title getIncidence
#' 
#' @description Calculates the crude and adjusted incidence rates.
#' 
#' @param Args takes list from \code{\link{setArgs}}.
#'
#' @return data.frame
#'
#' @import parallel
#'
#' @export

getIncidence <- function(Args) {
  hiv   <- getHIV(Args)
  rtdat <- getRTData(hiv)
  bdat <- getBirthDate()
  pdat <- getPredYear(Args)
  dat <- mclapply(seq(Args$nSim), 
    function(i) {
      cat(i, "")
      calcInc(rtdat, bdat, pdat, Args)},
      mc.cores=Args$mcores)
  cdat <- combineEst(dat) 
  crude <- getCrudeRate(cdat[1:4])
  adj <- getAdjRate(cdat[5:8])
  list(crude=crude, adj=adj)
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

