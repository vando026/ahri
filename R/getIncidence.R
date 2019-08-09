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
#' hiv   <- getHIV()
#' rtdat <- getRTData(hiv)
#' bdat <- getBirthDate(Args$inFiles$epifile)
#' getIncData(rtdat, bdat, Args)
getIncData <- function(rtdat, bdat, Args) {
  dat <- Args$imputeMethod(rtdat)
  edat <- splitAtSeroDate(dat) 
  setData(edat, Args, time2="obs_end", birthdate=bdat)
}

#' @title AggFunc
#' 
#' @description  Get aggregated events and pyears by Var.
#' 
#' @param RHS RHS of formula.
#' 
#' @return 
#'
#' @export 
AggFunc <- function(RHS) {
  function(dat) {
    F1 <- as.formula(paste(
      "cbind(sero_event, pyears=Time/365.25) ~ ", RHS))
    out <- aggregate(F1, data=dat, FUN=sum)
    rownames(out) <- out[[1]]
    out
  }
}

#' @title AggByYear
#' 
#' @description Aggregates data to get sero events and pyears by year.
#' formula.
#' 
#' @param dat Dataset. 
#' 
#' @return data.frame
#' 
#' @export 
#' 
#' @examples
#' # Show for one imputation 
#' Args <- setArgs(nSim=1, imputeMethod=imputeEndPoint)
#' hiv   <- getHIV()
#' rtdat <- getRTData(hiv)
#' bdat <- getBirthDate(Args$inFiles$epifile)
#' ydat <- getIncData(rtdat, bdat, Args)
#' inc <- AggByYear(ydat)
AggByYear <- AggFunc("Year")

#' @title AggByAge
#' 
#' @description Aggregates data to get sero events and pyears by age category.
#' formula.
#' 
#' @param dat Dataset. 
#' 
#' @return data.frame
#' 
#' @export 
AggByAge <- AggFunc("AgeCat")


#' @title calcCrudeInc
#' 
#' @description  Calculates crude incidence rates by year using poisson regression.
#' 
#' @param dat Dataset from \code{\link{AggByYear}}.
#' 
#' @return 
#'
#' @export 
calcCrudeInc <- function(dat) {
  dat <- AggByYear(dat)
  dat <- mutate(dat, Year = as.factor(Year))
  mod <- glm(sero_event ~ - 1 + Year + offset(log(pyears)),
    data=dat, family=poisson)
  data.frame(fit=mod$coef, se.fit=summary(mod)$coef[, 2])
}


#' @title doPoisYear
#' 
#' @description Do poisson regression and incidence rates by year.
#' 
#' @param dat Dataset from a function \code{\link{getAgeYear}}.
#' @param age_dat Dataset from function \code{\link{getAgeYear}}.
#'
#' @return data.frame
#'
#' @export
doPoisYear <- function(dat, 
  age_dat=eval.parent(quote(age_dat))) {
  dat <- mutate(dat, tscale = Time/365.25,
    Year = as.factor(Year))
  mod <- glm(sero_event ~ -1 + Year + Age + Year:Age 
    + offset(log(tscale)), data=dat, family=poisson)
  data.frame(predict.glm(mod, age_dat, se.fit=TRUE)[c(1,2)])
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


#' @title setInc
#' 
#' @description Sets the data and functions to calculate incidence estimates.
#' 
#' @param rtdat Dataset from \code{\link{getRTData}}.
#' 
#' @param Args provide arguments from \code{\link{setArgs}}.
#' 
#' @param fun A list of functions to compute, default is \code{\link{miCompute}}.
#' 
#' @return data.frame
#'
#' @export
#' 
#' @examples
#' hiv   <- getHIV()
#' rtdat <- getRTData(hiv)
#' bdat <- getBirthDate(Args$inFiles$epifile)
#' setInc(rtdat, bdat, doPoisAge, Args)
setInc <- function(rtdat, Args, fun=miCompute()) {
  age_dat <- getAgeYear(Args)
  bdat=getBirthDate()
  function(i) {
    cat(i, "")
    dat <- getIncData(rtdat, bdat, Args)
    lapply(fun, function(f) f(dat))
  }
}


#' @title combineEst
#' 
#' @description Collect all estimates into single matrix
#' 
#' @param dat Dataset from \code{\link{setInc}}.
#' @param get_names Names of the estimates to be collected. 
#'
#' @return list 
#'
#' @export
#'
#' @examples
#' hiv   <- getHIV()
#' rtdat <- getRTData(hiv)
#' bdat <- getBirthDate(Args$inFiles$epifile)
#' calcInc <- setInc(rtdat, Args, miCompute())
#' dat <- mclapply(seq(Args$nSim), calcInc,
#'   mc.cores=Args$mcores)
#' cdat <- combineEst(dat) 
combineEst <-  function(dat, get_names=miCombine()) {
  getEst <- function(dat, obj) {
    out <- as.matrix(sapply(dat, "[[", obj))
    rownames(out) <- rownames(dat[[1]][[obj[1]]])
    out
  }
  lapply(get_names, function(x) getEst(dat, x))
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

calcRubin <- function(est, se, fun=exp) {
  doCalc <- function(est, se, func=fun) {
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
    ci <- func(mn + c(-1, 1) * (tdf * se))
    c(rate=func(mn), lci=ci[1], uci=ci[2])
  }
  est <- split(est, rownames(est))
  se <- split(se, rownames(se))
  out <- Map(doCalc, est, se)
  out <- data.frame(do.call(rbind, out))
  out[] <- lapply(out[], `*`, 100)
  out
}

#' @title getMeans
#' 
#' @description  Helper function to get means of incidence estimates. 
#' 
#' @param dat A data.frame
#' 
#' @return 
#'
#' @export 
# Get standard mean estimates 
getMeans <- function(v1, v2) {
  function(dat) {
    data.frame(
      sero = rowMeans(dat[[v1]]),
      pyears = rowMeans(dat[[v2]]))
  }
}

#' @title getRubin
#' 
#' @description  Helper function to get incidence estimates using Rubins rules. 
#' 
#' @param dat A data.frame.
#' 
#' @return 
#'
#' @export 
getRubin <- function(v1, v2) {
  function(dat) {
    calcRubin(dat[[v1]], dat[[v2]]) 
  }
}

#' @title miCompute
#' 
#' @description  Compute aggregates of seroevents and person years or incidence rates, used in \code{\link{setInc}}. 
#' 
#' @param flist A list of functions. 
#' 
#' @return  list 
#'
#' @export 
#' @examples 
#' mi_compute <- list(year = AggByYear, 
#'   crude = calcCrudeInc, 
#'   age_adj = doPoisYear)
miCompute <- function(flist=list(
  year = AggByYear, crude = calcCrudeInc, 
  age_adj = doPoisYear)) {
  return(flist) 
}

#' @title miCombine
#' 
#' @description  Combine the results from \code{\link{miCompute}}.
#' 
#' @param slist A list of names.
#' 
#' @return  list
#'
#' @export 
miCombine <- function(slist=list(
  sero=c("year", "sero_event"),
  pyears=c("year", "pyears"),
  crude_est=c("crude", "fit"),
  crude_se=c("crude", "se.fit"),
  adj_est=c("age_adj", "fit"),
  adj_se=c("age_adj", "se.fit"))) {
  return(slist)
}

#' @title miExtract
#' 
#' @description  Extract parameter estimates and standard errors from \code{\link{miCombine}}.
#' 
#' @param flist A list of functions. 
#' 
#' @return list 
#'
#' @export 

miExtract <- function(flist=list(
  agg=getMeans("sero", "pyears"),
  crude=getRubin("crude_est", "crude_se"),
  adj=getRubin("adj_est", "adj_se"))) {
  return(flist)
}

#' @title getIncidence
#' 
#' @description Calculates the incidence rates.
#' 
#' @param Args Takes list from \code{\link{setArgs}}.
#' @param Compute Takes list from \code{\link{miCompute}}.
#' @param Combine Takes list from \code{\link{miCombine}}.
#' @param Extract Takes list from \code{\link{miExtract}}.
#'
#' @return data.frame
#'
#' @export

getIncidence <- function(Args, Compute=miCompute(), 
  Combine=miCombine(), Extract=miExtract()) {
  hiv <- getHIV()
  rtdat <- getRTData(hiv)
  calcInc <- setInc(rtdat, Args, fun=Compute)
  dat <- parallel::mclapply(seq(Args$nSim),
    calcInc, mc.cores=Args$mcores)
  cdat <- combineEst(dat, get_names=Combine) 
  lapply(Extract, function(f) f(cdat))
}



