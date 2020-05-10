#' @title getIncData
#' 
#' @description Function that imputes sero date, splits at censoring date and set the age.
#' 
#' @param rtdat dataset from \code{\link{getRTData}}. 
#' @param bdat dataset from \code{\link{getBirthDate}}. 
#' @param Args takes list from \code{\link{setArgs}}.
#' @param func Function to perform additional operation. 
#'
#' @return data.frame
#' @export
#'
#' @examples
#' rtdat <- getRTData(dat=getHIV())
#' getIncData(rtdat, bdat=getBirthDate(), Args=setArgs())
getIncData <- function(rtdat, bdat, Args, func=identity) {
  dat <- Args$imputeMethod(rtdat)
  edat <- splitAtSeroDate(dat) 
  edat <- setData(edat, Args, time2="obs_end", birthdate=bdat)
  edat <- mutate(edat, tscale = Time/365.25)
  func(edat)
}

#' @title AggFunc
#' 
#' @description  A function factory for creating specific AggByFuncs, see for example
#' \code{\link{AggByYear}}.
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
    out <- stats::aggregate(F1, data=dat, FUN=sum, drop=FALSE)
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
#' @examples
#' Args <- setArgs(Years=c(2008:2018), 
#'   Age=list(All=c(15, 45)),
#'   imputeMethod=imputeRandomPoint)
#' hiv <- getHIV()
#' rtdat <- getRTData(hiv)
#' idat <- getIncData(rtdat, bdat=getBirthDate(), Args)
#' idat_yr <- AggByYear(idat)
#' calcPoisExact(idat_yr, byVar="Year")

calcPoisExact <- function(dat, byVar="Year", fmt=TRUE) {
  dat <- split(dat, dat[, byVar])
  dat <- do.call(rbind, lapply(dat, function(x)
    pois.exact(x$sero_event, x$pyears)))
  if (fmt==TRUE) {
    vars <- c("rate", "lower", "upper")
    dat[vars] <- lapply(dat[vars], function(x) round(x*100, 3))
  }
  dat <- dplyr::rename(dat, sero_event=.data$x, 
    pyears=.data$pt, lci=.data$lower, uci=.data$upper)
  dat
}


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


#' @title getAgeYear
#' 
#' @description  Calculate mean age by year. This is mostly used in the \code{predict}
#' part of \code{\link{doPoisYear}}. 
#' 
#' @param dat A dataset with an Age and Year variable. 
#' 
#' @return data.frame
#'
#' @export 
#' @examples
#' Args <- setArgs(Years=c(2008:2018), 
#'   Age=list(All=c(15, 45)),
#'   imputeMethod=imputeRandomPoint)
#' rtdat <- getRTData(dat=getHIV())
#' idat <- getIncData(rtdat, bdat=getBirthDate(), Args)
#' age_dat <- getAgeYear(dat=setHIV(Args))
#' pois_yr <- doPoisYear(idat, age_dat)

getAgeYear <- function(dat) {
  group_by(dat, Year) %>% 
  summarize(Age = mean(Age)) %>% 
  mutate(Year = factor(Year), tscale=1)
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
  dat <- mutate(dat, Year = as.factor(.data$Year))
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
#' @param dat Estimates from glm, see for example \code{\link{doPoisYear}}.
#' @param by100 If TRUE (default) calculate incidence per 100 person-years.
#' @param func A function to transform the data, default is to exponentiate, \code{exp}.
#' @param pval If TRUE (default) calculate the p-values.
#' @return data.frame
#'
#' @export 
#'
#' @examples
#' Args <- setArgs(Years=c(2008:2018), 
#'   Age=list(All=c(15, 45)),
#'   imputeMethod=imputeRandomPoint)
#' age_dat <- getAgeYear(dat=setHIV(Args))
#' rtdat <- getRTData(dat=getHIV())
#' idat <- getIncData(rtdat, bdat=getBirthDate(), Args)
#' pois_yr <- doPoisYear(idat, age_dat)
#' calcPoisCI(pois_yr)

calcPoisCI <- function(dat, func=exp, by100=TRUE, pval=FALSE) {
  tdf <- 1.96 
  lci <- func(dat$fit - (tdf * dat$se.fit))
  uci <- func(dat$fit + (tdf * dat$se.fit))
  mn <- func(dat$fit)
  out <- data.frame(rate=mn, lci, uci)
  if (pval)  out$pvalue <- round(2*stats::pnorm(-abs(dat$fit/dat$se.fit)), 4)
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
  bdat=getBirthDate()
  parallel::mclapply(seq(Args$nSim),
    function(i) { 
      cat(i, "")
      getIncData(rtdat, bdat, Args, func=f)},
      mc.cores=Args$mcores)
}


#' @title MIpredict
#' 
#' @description Used with \code{mitools} to get incidence rate estimates per 100
#' person-years after multiple imputation.
#' 
#' @param object Results from the object generated by \code{MIcombine}.
#' @param newdata Dataframe of variables for prediction.
#' 
#' @return data.frame
#'
#' @export 
#' @examples
#' rtdat <- getRTData(dat=getHIV())
#' Args <- setArgs(nSim=2, Years=c(2008:2018))
#' mdat <- MIdata(rtdat, Args)
#' mdat <- mitools::imputationList(mdat)
#' F1 <- "sero_event ~ -1 + as.factor(Year) + Age +
#'   as.factor(Year):Age + offset(log(tscale))" 
#' mods <- with(mdat, glm(as.formula(F1), family=poisson))
#' betas <- mitools::MIextract(mods,fun=coef)
#' var <- mitools::MIextract(mods, fun=vcov)
#' res <-  mitools::MIcombine(betas, var) 
#' MIpredict(object=mods, newdata=getAgeYear(setHIV(Args)))

MIpredict <- function(object,  newdata)  {
  res <- mitools::MIcombine(object)
  object <- object[[1]]
  Terms <- stats::delete.response(stats::terms(object))
  m <- stats::model.frame(Terms, newdata, xlev = object$xlevels)
  mat <- stats::model.matrix(Terms, m, contrasts.arg = object$contrasts)
  pred <- mat %*% stats::coef(res)
  se <-  sqrt(diag(mat %*% stats::vcov(res) %*% t(mat)))
  fit <- exp(pred)
  se.fit <- se * abs(exp(pred))
  Qt <- c(-1, 1) * stats::qnorm((1 - 0.95)/2, lower.tail = FALSE)
  CI <- sapply(Qt, "*", se.fit)
  out <- data.frame(fit=fit, se.fit=se.fit, 
    lci=fit+CI[, 1], uci=fit+CI[, 2])
  out <- data.frame(lapply(out, "*", 100))
  rownames(out) <- object$xlevels[[1]]
  out
}

#' @title MIaggregate
#' 
#' @description  Aggregates results after mutiple imputation.
#' 
#' @param dat List of results.
#' @param col_names Names used to collect dataset columns into a single matrix.
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

MIaggregate <-  function(dat, col_names=c("sero_event", "pyears")) {
  if (!(class(dat) %in% c("imputationList", "list")))
    stop("Data must be a list of >= 1 dataset")
  same <- sapply(dat, function(x) nrow(x))
  if (abs(max(same) - min(same)) != 0)
    stop("Your datasets have different nrows")
  getEst <- function(dat, col_name) {
    out <- as.matrix(sapply(dat, "[[", col_name))
    rowMeans(out, na.rm=TRUE)
  }
  dat0 <- dat[[1]]
  dat0 <- dat0[, !(colnames(dat0) %in% col_names), drop=FALSE]
  out <- data.frame(lapply(col_names, function(x) getEst(dat, x)))
  colnames(out) <- col_names
  cbind(dat0, out)
}


#' @title getIncidence
#' 
#' @description  Calculate annual HIV incidence rates using the single random-point
#' approach and multiple imputation. 
#' 
#' @param Args takes list from \code{\link{setArgs}}.
#' @param sformula A list of formulas for the poisson regression models.
#' @param aggFun Function to aggregate sero events and person-time by, see
#' \code{link{AggFunc}}.
#' @param newdata Dataset of variables to predict incidence, see \code{\link{MIpredict}}.
#' 
#' @return list
#' @export 
#' @examples
#' Args <- setArgs(Age=list(All=c(15, 49)), Years=c(2004:2018), nSim=2)
#' age_dat <- getAgeYear(dat=setHIV(Args))
#' sformula = "sero_event ~ -1 + as.factor(Year) + Age + as.factor(Year):Age + offset(log(tscale))"
#' getIncidence(Args, sformula, AggByYear, age_dat)
getIncidence <- function(
  Args=setArgs(), sformula="", aggFun=NULL, newdata=NULL) {
  if (Args$nSim < 2) stop('nSim in setArgs() must be > 1')
  rtdat <- getRTData()
  mdat <- MIdata(rtdat, Args)
  agg_inc <- lapply(mdat, aggFun)
  agg_inc <- MIaggregate(agg_inc)
  mdat <- mitools::imputationList(mdat)
  mods <- with(mdat, stats::glm(as.formula(sformula), family=poisson))
  pois_inc <- MIpredict(mods, newdata)
  list(agg=agg_inc, pois_inc=pois_inc)
}
