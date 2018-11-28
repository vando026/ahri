#' @title getIncData
#' 
#' @description Function that imputes sero date, splits at censoring date and set the age.
#' 
#' @param rtdat dataset from \code{\link{getRTData}}. 
#' @param idat dataset from \code{\link{getBirthDate}}. 
#' @param Args takes list from \code{\link{setArgs}}.
#'
#' @return data.frame
#' 
#' @export
#'
#' @examples
#' hiv   <- getHIV(Args)
#' rtdat <- getRTData(hiv)
#' idat <- getBirthDate(Args$inFiles$epifile)
#' getIncData(rtdat, idat, Args)

getIncData <- function(rtdat, idat, Args) {
  dat <- Args$imputeMethod(rtdat)
  edat <- splitAtSeroDate(dat, splitYears=Args$Years) 
  adat <- getAgeData(edat, idat,  Args)
  adat
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
#' idat <- getBirthDate(Args$inFiles$epifile)
#' ydat <- getIncData(rtdat, idat, Args)
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

#' @title doPoisYear
#' 
#' @description Do poisson regression for incidence rates by year or age. 
#' 
#' @param dat Dataset from a function \code{\link{getIncData}}.
#'
#' @return data.frame
#'
#' @examples
#' doPoisYear <- poisFunc("Year")
#' pdat <- getIncData(dat) 
#' doPoisYear(pdat)
#' doPoisAge <- poisFunc("AgeCat")

doPoisYear <- function(dat) {
  dat$tscale <- dat$Time/365.25
  mod <- glm(sero_event ~ as.factor(Year) + Age + offset(log(tscale)),
    data=dat, family=poisson)
  nyears <- seq(unique(dat$Year))
  ndat <- data.frame(Age = mean(dat$Age), tscale=1,
    Year = factor(nyears, levels = nyears, labels = levels(as.factor(dat$Year))))
  out <- predict.glm(mod, ndat, type="response", se.fit=TRUE)
  out <- data.frame(out[c("fit", "se.fit")])
  rownames(out) <- ndat$Year
  out
}

doPoisAge <- function(dat) {
  dat$tscale <- dat$Time/365.25
  mod <- glm(sero_event ~ AgeCat + offset(log(tscale)),
    data=dat, family=poisson)
  nage <- seq(unique(dat$AgeCat))
  ndat <- data.frame(tscale=1,
    AgeCat = factor(nage, levels = nage, labels = levels(as.factor(dat$AgeCat))))
  out <- predict.glm(mod, ndat, type="response", se.fit=TRUE)
  out <- data.frame(out[c("fit", "se.fit")])
  rownames(out) <- ndat$AgeCat
  out
}

#' @title calcInc
#' 
#' @description Calculates the crude and adjusted incidence.
#' 
#' @param rtdat Dataset from \code{\link{getRTData}}.
#' 
#' @param idat Dataset from \code{\link{getBirthDate}}.
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
#' idat <- getBirthDate(Args$inFiles$epifile)
#' calcInc(rtdat, idat, Args)

calcInc <- function(rtdat, idat, Args) {
  dat <- getIncData(rtdat, idat, Args)
  nm <- c("Year", "Age", "poisYear", "poisAge")
  funs <- list(AggByYear, AggByAge, doPoisYear, doPoisAge)
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
#' idat <- getBirthDate(Args$inFiles$epifile)
#' dat <- mclapply(seq(Args$nSim), 
#'   function(i) calcInc(rtdat, idat, Args), 
#'   mc.cores=Args$mcores)
#' cdat <- combineEst(dat) 

combineEst <-  function(dat) {
  getFunc <- function(dat) {
    function(obj) {
      out <- sapply(seq(length(dat)),
        function(i) dat[[i]][[obj]])
      rownames(out) <- rownames(dat[[1]][[obj[1]]])
      out
    }
  }
  getEst <- getFunc(dat)
  nms <- list(
    getSero = c("Year", "sero_event"),
    getPYear = c("Year", "pyears"),
    getAgeSero = c("Age", "sero_event"),
    getAgePYear = c("Age", "pyears"),
    getPoisYearEst = c("poisYear", "fit"),
    getPoisYearSE = c("poisYear", "se.fit"),
    getPoisAgeEst = c("poisAge", "fit"),
    getPoisAgeSE = c("poisAge", "se.fit"))
  lapply(nms, getEst)
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
#' idat <- getBirthDate(Args$inFiles$epifile)
#' dat <- mclapply(seq(Args$nSim), 
#'   function(i) calcInc(rtdat, idat, Args))
#' cdat <- combineEst(dat)
#' getCrudeRate(cdat)

getCrudeRate <- function(dat) {
  out <- sapply(dat, rowMeans)
  Year <- data.frame(sero=out[[1]], pyears=out[[2]])
  Age <- data.frame(sero=out[[3]], pyears=out[[4]])
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
  if (m==1) return(list(mn=est, se=se))
  mn <- mean(est)
  var_with <- mean(se^2)
  var_betw <- sum((est - mn)^2)/(m-1)
  var_tot <- var_with + var_betw*(1 + (1/m))
  se <- sqrt(var_tot)
  rdf <- (m - 1) * (1 + (var_with/((1+ (1/m)) * var_betw)))^2
  tdf <- qt(1 - (0.05/2), rdf)
  lb <- mn - (tdf * se)
  ub <- mn + (tdf * se)
  c(rate=mn, se=se, lci=lb, uci=ub)
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
#' idat <- getBirthDate(Args$inFiles$epifile)
#' dat <- mclapply(seq(Args$nSim), 
#'   function(i) calcInc(rtdat, idat, Args))
#' cdat <- combineEst(dat)
#' geteAdjRate(cdat)

getAdjRate <- function(dat) {
  calcPredict <- function(est, se) {
    est <- split(est, rownames(est))
    se <- split(se, rownames(se))
    out <- Map(calcRubin, est, se)
    dat <- dplyr::bind_rows(out)
    dat[] <- lapply(dat[], `*`, 100)
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
  idat <- getBirthDate(Args$inFiles$epifile)
  dat <- mclapply(seq(Args$nSim), 
    function(i) {cat(i, ""); calcInc(rtdat, idat, Args)},
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

