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
#' rtdat <- getRTData(getHIV())
#' getIncData(rtdat, bdat=getBirthDate(), Args)
getIncData <- function(rtdat, bdat, Args) {
  dat <- Args$imputeMethod(rtdat)
  edat <- splitAtSeroDate(dat) 
  setData(edat, Args, time2="obs_end", birthdate=bdat)
}

#' @title AggFunc
#' 
#' @description  A function factory for creating specific AggByFuncs, see for example
#' \code{\code{AggByYear}}.
#' 
#' @param RHS The variable name as string that you want to aggregate by.
#' 
#' @export 
#' @examples
#' \donttest{
#' AggByYear <- AggFunc("Year")
#' }

AggFunc <- function(RHS) {
  function(dat) {
    F1 <- stats::as.formula(paste(
      "cbind(sero_event, pyears=Time/365.25) ~ ", RHS))
    out <- stats::aggregate(F1, data=dat, FUN=sum)
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
#' getFiles <- setFiles(file.path(Sys.getenv("HOME"), "AHRI_Data/2019"))
#' hiv   <- getHIV()
#' rtdat <- getRTData(hiv)
#' bdat <- getBirthDate()
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


#' @title doPoisCrude
#' 
#' @description  Calculates crude incidence rates by year using poisson regression.
#' 
#' @param dat Dataset from \code{\link{AggByYear}}.
#' 
#' @return data.frame
#'
#' @export 
doPoisCrude <- function(dat) {
  dat <- AggByYear(dat)
  dat <- mutate(dat, Year = as.factor(.data$Year))
  mod <- stats::glm(sero_event ~ - 1 + Year + offset(log(pyears)),
    data=dat, family=poisson)
  data.frame(fit=mod$coef, se.fit=summary(mod)$coef[, 2])
}


#' @title calcPoisExact 
#' 
#' @description  Calculate crude incidence rates using the Poisson exact method. 
#' 
#' @param dat A dataset from \code{\link{AggByYear}} or \code{\link{AggByAge}}.
#' @param byVar Either "AgeCat" or "Year".
#' @param fmt If TRUE, format by 100 person-years and round off to three decimal places. 
#' 
#' @return data.frame
#'
#' @import epitools
#' @export 
calcPoisExact <- function(dat, byVar="Year", fmt=TRUE) {
  dat <- split(dat, dat[, byVar])
  dat <- do.call(rbind, lapply(dat, function(x)
    pois.exact(x$sero_event, x$pyears)))
  if (fmt==TRUE) {
    vars <- c("rate", "lower", "upper")
    dat[vars] <- lapply(dat[vars], function(x) round(x*100, 3))
  }
  dat <- dplyr::rename(dat, sero_event=x, 
    pyears=pt, lci=lower, uci=upper)
  dat
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
  mod <- stats::glm(sero_event ~ -1 + Year + Age + Year:Age 
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
  mod <- stats::glm(sero_event ~ AgeCat + offset(log(tscale)),
    data=dat, family=poisson)
  nage <- seq(unique(dat$AgeCat))
  ndat <- data.frame(tscale=1,
    AgeCat = factor(nage, levels = nage, labels = levels(dat$AgeCat)))
  out <- data.frame(predict.glm(mod, ndat, se.fit=TRUE)[c(1, 2)])
  rownames(out) <- ndat$AgeCat
  out
}


#' @title calcPoisCI
#' 
#' @description  Calculates standard errors for Poisson estimates from a single imputed
#' dataset.
#' 
#' @param est Estimates from m imputations, must be a data.frame[, "fit", drop=FALSE]
#' @param se Estimates from m imputations, must be a data.frame[, "se.fit", drop=FALSE]
#' @param by100 If TRUE (default) calculate incidence per 100 person-years.
#' @param fun A function to transform the data, default is to exponentiate, \code{exp}.
#' @param pval If TRUE (default) calculate the p-values.
#' @return data.frame
#'
#' @export 
#'
#' @examples
#' Args <- setArgs(Years=c(2008:2018), 
#'   Age=list(All=c(15, 45)),
#'   imputeMethod=imputeRandomPoint)
#' hiv <- getHIV()
#' rtdat <- getRTData(hiv)
#' idat <- getIncData(rtdat, bdat=getBirthDate(), Args)
#' pois_yr <- doPoisYear(idat, age_dat)
#' calcPoisCI(est=pois_yr[, "fit", drop=FALSE], se=pois_yr[, "se.fit", drop=FALSE])

calcPoisCI <- function(est, se, fun=exp, by100=TRUE, pval=FALSE) {
  doCalc <- function(est, se, func=fun, Pval=pval) {
    m <- length(est)
    mn <- unlist(est)
    se <- unlist(se)
    tdf <- 1.96 
    ci <- func(mn + c(-1, 1) * (tdf * se))
    out <- c(rate=func(mn), lci=ci[1], uci=ci[2])
    if (Pval) {
      pvalue <- round(2*pnorm(-abs(mn/se)), 4)
      out <- c(out, pval=pvalue)
    }
    return(out)
  }
  est <- split(est, rownames(est))
  se <- split(se, rownames(se))
  out <- Map(doCalc, est, se)
  out <- data.frame(do.call(rbind, out))
  if (by100) out[] <- lapply(out[], `*`, 100)
  out
}


#' @title MIdata
#' 
#' @description  Function to generate dataset with imputed serodate.
#' 
#' @param rtdat Dataset from \code{\link{getRTData}}.
#' @param Args Takes list from \code{\link{setArgs}}.
#' @param f Function to perform additional operation. 
#' 
#' @return data.frame
#'
#' @export 
#' @examples
#' Args <- setArgs(nSim=2, mcores=1)
#' rtdat <- getRTData(dat=getHIV())
#' MIdata(rtdat, Args)
MIdata <- function(rtdat, Args, f=identity) {
  MIget <- function(...) {
    dat <- imputeRandomPoint(rtdat)
    edat <- splitAtSeroDate(dat) 
    out <- setData(edat, Args, time2="obs_end", birthdate=bdat)
    f(out)
  }
  bdat=getBirthDate()
  parallel::mclapply(seq(Args$nSim),
    function(i) { 
      cat(i, "")
      MIget(rtdat, Args, bdat, f=f)},
      mc.cores=Args$mcores)
}


#' @title MIpois
#' 
#' @description Run poission regressions.
#' 
#' @param mdat List of datasets from \code{mitools}.
#' @param pdat Dataset for predicting values.
#' @param sformula List of string formulas.
#' 
#' @return List
#' @export
MIpois <- function(mdat, pdat, sformula) {
  mkVars <- function(dat) 
    dplyr::mutate(dat, tscale = .data$Time/365.25, Year = as.factor(.data$Year))
  mdat <- lapply(mdat, mkVars)
  mdat <- mitools::imputationList(mdat)
  mods <- with(mdat, stats::glm(as.formula(sformula), family=poisson))
  mres <- mitools::MIcombine(mods)
  res <- list(mres=mres, object=mods[[1]])
  MIpredict(res, pdat, sformula)
}

#' @title MIpredict
#' 
#' @description Used with \code{mitools} to get incidence rate estimates per 100
#' person-years after multiple imputation.
#' 
#' @param res Results from the object generated by \code{MIcombine}.
#' @param dat Dataframe of values for prediction.
#' @param sformula String formula to build model matrix.
#' 
#' @return data.frame
#'
#' @export 
#' @examples
#' rtdat <- getRTData(dat=getHIV())
#' Args <- setArgs(nSim=2)
#' mdat <- MIdata(rtdat, Args)
#' mkVars <- function(dat) 
#'    dplyr::mutate(dat, tscale = .data$Time/365.25, Year = as.factor(.data$Year))
#' mdat <- lapply(mdat, mkVars)
#' mdat <- mitools::imputationList(mdat)
#' F1 <- "sero_event ~ -1 + Year + Age + Year:Age + offset(log(tscale))" 
#' mods <- with(mdat, glm(as.formula(F1), family=poisson))
#' betas <- mitools::MIextract(mods,fun=coef)
#' vars <- mitools::MIextract(mods, fun=vcov)
#' res <-  mitools::MIcombine(betas, vars) 
#' res <- list(mres=res, object=mods[[1]])
#' MIpredict(res, dat=getAgeYear(Args), F1)

MIpredict <- function(res, dat, sformula)  {
  obj <- res$object
  res <- res$mres
  Terms <- stats::delete.response(stats::terms(obj))
  m <- stats::model.frame(Terms, dat, xlev = obj$xlevels)
  mat <- stats::model.matrix(Terms, m, contrasts.arg = obj$contrasts)
  pred <- mat %*% stats::coef(res)
  se <-  sqrt(diag(mat %*% stats::vcov(res) %*% t(mat)))
  fit <- exp(pred)
  se.fit <- se * abs(exp(pred))
  Qt <- c(-1, 1) * stats::qnorm((1 - 0.95)/2, lower.tail = FALSE)
  CI <- sapply(Qt, "*", se.fit)
  out <- data.frame(fit=fit, se.fit=se.fit, 
    lci=fit+CI[, 1], uci=fit+CI[, 2])
  out <- data.frame(lapply(out, "*", 100))
  rownames(out) <- rownames(dat)
  out
}

#' @title MIaggregate
#' 
#' @description  Aggregates results after mutiple imputation.
#' 
#' @param dat List of results.
#' @param get_names Names used to collect dataset columns into a single matrix.
#' 
#' @return data.frame
#'
#' @export 
#' @examples
#' Args <- setArgs(nSim=2, mcores=1)
#' rtdat <- getRTData(dat=getHIV())
#' mdat <- MIdata(rtdat, Args)
#' mdat <- mitools::imputationList(mdat)
#' inc <- with(mdat, fun=AggByYear)
#' MIaggregate(inc)

MIaggregate <-  function(dat, get_names=c("sero_event", "pyears")) {
  getEst <- function(dat, obj) {
    out <- as.matrix(sapply(dat, "[[", obj))
    rowMeans(out)
  }
  out <- data.frame(lapply(get_names, function(x) getEst(dat, x)))
  colnames(out) <- get_names
  rownames(out) <- rownames(dat[[1]])
  out
}


#' @title getFormula
#' 
#' @description  Gets default glm string formulas to run crude and age adjusted incidence.
#' 
#' @return list
#'
#' @export 
getFormula <- function() {
  list(
    crude = "sero_event ~ -1 + Year + offset(log(tscale))",
    adj = "sero_event ~ -1 + Year + Age + Year:Age + offset(log(tscale))")
}


#' @title getIncidence
#' 
#' @description  Default function for using \code{mitools} to do incidence rate
#' estimation.
#' 
#' @param Args takes list from \code{\link{setArgs}}.
#' @param formulas A list of formulas for the poisson regression models, see
#' \code{\link{getFormula}}.
#' 
#' @return list
#'
#' @export 
getIncidence <- function(Args, formulas=getFormula()) {
  hiv <- getHIV()
  rtdat <- getRTData(hiv)
  age_dat <- getAgeYear(Args)
  mdat <- MIdata(rtdat, Args)
  pois_inc <- lapply(formulas, 
    function(x) MIpois(mdat, age_dat, x))
  agg_inc <- lapply(mdat, AggByYear)
  agg_inc <- MIaggregate(agg_inc)
  list(agg=agg_inc, pois_inc=pois_inc)
}
