#' @title intCensParse
#' 
#' @description  Code to get IntCens results from output.
#' 
#' @param File File path to output.txt.
#'
#' @export
#'
intCensParse <- function(File=NULL) {

  out <- readLines(File)
  out <- gsub("\\t", " ", out)
  out <- out[out!=""]

  # get lines of surv data
  time_ln <- grep("^Time[[:space:]]+Estimate", out)
  surv_dat <- out[(time_ln+1):length(out)] 
  surv_dat <- strsplit(surv_dat, "[[:space:]]+")
  surv_dat <- data.frame(do.call(rbind, surv_dat), stringsAsFactors=FALSE)
  surv_dat[] <- lapply(surv_dat[],  as.numeric)
  colnames(surv_dat) <-  c("Time", "Estimate")

  # Now get estimates into R object
  Cov_ln <- grep("^Covariate", out)
  Cov_ln2 <- grep("[Co]?[vV]ariance", out)
  emat <- out[(Cov_ln+1):(Cov_ln2-1)] 
  emat <- strsplit(emat, "[[:space:]]+")
  emat <- data.frame(do.call(rbind, emat), stringsAsFactors=FALSE)
  emat[, -1] <- lapply(emat[, -1], as.numeric)
  colnames(emat) <-  strsplit(out[Cov_ln], "[[:space:]]+")[[1]]

  # Covariance estimates
  ofset <- ifelse(grepl("^Var", out[Cov_ln2]), 1, 2) # needed
  cmat <- out[(Cov_ln2+ofset):(time_ln-2)] 
  cmat <- gsub("^\\s+|:", "", cmat)
  cmat <- strsplit(cmat, "[[:space:]]+")
  cmat <- data.frame(do.call(rbind, cmat), stringsAsFactors=FALSE)
  cmat[,-1] <- lapply(cmat[,-1], as.numeric)
  nm <- cmat[,1]; cmat <- cmat[,-1, drop=FALSE]
  colnames(cmat) <-  rownames(cmat) <- nm

  list(sdat=surv_dat, edat=emat, cdat=cmat)
}


#' @title intCensImpute
#' 
#' @description  Uses the G-transformation to impute events
#' 
#' @param dat A dataset.
#' @param Results Results from \code{\link{intCensParse}}.
#' @param Args provide arguments from \code{\link{setArgs}}.
#' @param start_date If null, start_date is the first obs_start date of ID, else it is
#' same start_date for everyone.
#' 
#' @return 
#'
#' @export 

intCensImpute <- function(dat, Results, Args, start_date=NULL) {

  message("Running intCensImpute...")
  G = function(x)  return(x)

  # simulate from the multivariate normal distribution of the 
  # regression parameter estimates
  betaMeans <- Results$edat[, "Estimate"]
  betaCovariance  <- Results$cdat
  regParamsSim = mvtnorm::rmvnorm(n=Args$nSim,
    mean = betaMeans, sigma = as.matrix(betaCovariance))

  # step function for the baseline hazard
  survTime <- Results$sdat[, "Time"]
  Estimate <- Results$sdat[, "Estimate"]
  baselineHazard  <-  stepfun(x=c(0, survTime),
    y=c(0, Estimate, max(Estimate)), right=FALSE)

  # Work only with HIV+
  dat <- data.frame(dat[!is.na(dat$early_pos), ])
  allIDs <- sort(unique(dat$IIntID))

  doFunc <- function(oneID, dat, Args) {
    cat(oneID, "")
    oneIDdata <- dat[dat$IIntID==oneID, ]
    stopifnot(nrow(oneIDdata)>0)
    start_time <- ifelse(is.null(start_date), 
      as.character(oneIDdata$obs_start[1]), start_date)
    leftTime <- as.integer(
      difftime(oneIDdata$late_neg[1], start_time, units='days'))
    rightTime <- as.integer(
      difftime(oneIDdata$early_pos[1], start_time, units='days'))

    #vector of random seroconversion times
    SeroTimes = rep(NA,Args$nSim)

    # Get all the knots in censor interval
    jumpTimesIndicesSample = which((knots(baselineHazard)>=leftTime) &
      (knots(baselineHazard)<=rightTime))
    jumpTimesIndices = which((knots(baselineHazard)>=0) &
      (knots(baselineHazard)<=rightTime))
    variableNames <- Results$edat[, "Covariate"]
    if(length(jumpTimesIndicesSample)<1) {
      print(sprintf("=====Issue for %s ", oneID))
    } else if(length(jumpTimesIndicesSample)>=1) {
      covariateValues = matrix(data=NA,
        nrow=length(jumpTimesIndices),
        ncol=length(variableNames))
    # Here we assign the covariate value to each knot on ID time
      for(i in seq_len(length(variableNames))) {
        oneVariable = variableNames[i]
        Z = stepfun(x = oneIDdata$Time,
          y = c(oneIDdata[,oneVariable],max(oneIDdata[,oneVariable])),
          right=FALSE)
        covariateValues[,i] = Z(knots(baselineHazard)[jumpTimesIndices])
      }
      xbase <- knots(baselineHazard)[jumpTimesIndices]
      Lambda <- c(0, diff(baselineHazard(xbase)))
      AllSeroTimes = c(knots(baselineHazard)[jumpTimesIndicesSample],rightTime)
      for(asim in seq_len(Args$nSim))
        {
          M = cumsum(exp(as.vector(covariateValues %*% matrix(data=regParamsSim[asim,],ncol=1)))*Lambda)
          valF = 1 - exp(-G(M))
          distF = stepfun(x=xbase, y=c(valF,max(valF)), right=FALSE)
          seroDist <- c(0, diff(distF(AllSeroTimes)))
          mysum = sum(seroDist)
          if(0==mysum) {
            SeroTimes[asim] <- as.integer(runif(1, leftTime+1, rightTime))
          } else {
            seroDist = seroDist/mysum
            SeroTimes[asim] = sample(x=AllSeroTimes,size=1,prob=seroDist)
          }
          if(any(SeroTimes[asim]<leftTime))
            stop('Random seroconversion time smaller than allowed\n')
          if(any(SeroTimes[asim]>rightTime))
            stop('Random seroconversion time larger than allowed\n')
        }
    }
    names(SeroTimes) <- paste0("s", seq(Args$nSim))
    c(IIntID=oneID, start_date=as.Date(start_time), 
      obs_start=oneIDdata$obs_start[1], 
      late_neg=leftTime, early_pos=rightTime, SeroTimes) 
  }
  out <- parallel::mclapply(allIDs, 
    function(i) doFunc(i, dat, Args),
    mc.cores=Args$mcores)
  data.frame(do.call("rbind", out))
}




#' @title UniReg
#' 
#' @description  Wrapper for IntCens fuction by Zeng et al 2016.
#' 
#' @param InFile txt file to be input
#' 
#' @param OutFile txt file to be output
#' 
#' @param Model equation to be given
#'
#' @param ID name of subject ID
#'
#' @export
#'
#' @examples
#' UniReg(
#' InFile = file.path(output, "TestSim.txt"),
#' OutFile = file.path(output, "TestSim_Out.txt"), r = 0.0,
#' Model = "(time, d) = v1 + v2 + v3",
#' ID = "id")


UniReg <- function(InFile, OutFile, Model, ID=NULL, inf="Inf",
  iter=5000, cthresh=0.0001, r=0.0, printout=FALSE, ign_stout=TRUE) {
    InFile <- paste("--in", InFile)
    OutFile <- paste("--out", OutFile)
    Model <- paste("--model", shQuote(Model))
    ID <- ifelse(is.null(ID), "", paste("--subject_id", ID))
    Sep <- paste("--sep", shQuote(" "))
    inf <- paste("--inf_char", inf)
    R <- paste("--r", r)
    iter <- paste("--max_itr", iter)
    cthresh <- paste("--convergence_threshold", cthresh)
    if (Sys.getenv("OS") == "Windows_NT") {
      xpath <- file.path(.libPaths()[1], "IntCens2", "unireg.exe")
      system(command=paste(xpath, InFile, OutFile, Model, 
        ID, Sep, iter, R, inf, cthresh, collapse=" "),
        show.output.on.console=printout)
    } else {
      xpath <- file.path(.libPaths()[1], "IntCens2", "unireg")
      system(command=paste(xpath, InFile, OutFile, Model, 
        ID, Sep, iter, R, inf, cthresh, collapse=" "),
        ignore.stdout=ign_stout)
    }
}

#' @title SetUniReg
#' 
#' @description  Helper function to run \code{\link{UniReg}}.
#' 
#' @param  modVars Variables to feed into UniReg model.
#' @param  aName Name of file to be written.
#' 
#' @return list
#'
#' @export 
SetUniReg <- function(modVars, aName) {
  UniReg(
    InFile=file.path(derived, paste0(aName,".txt")), 
    OutFile=file.path(derived, paste0(aName, "_out.txt")), 
    Model = paste0("(Time, sero_event) = ", modVars), 
    ID="IIntID", printout=TRUE, ign_stout=FALSE, cthresh=0.01)
}



#' @title imputeIntCensPoint
#' 
#' @description  Imputes the dates from \code{\link{intCensImpute}}.
#' 
#' @param rtdat The dataset to impute.
#' @param sdates The imputed dateset.
#' 
#' @return 
#'
#' @export 
imputeIntCensPoint <- function(rtdat, sdates, i) {
  sdat <- sdates[, c("IIntID", "start_date", paste0("s", i))]
  names(sdat) <- c("IIntID", "start_date", "sero_days")
  sdat <- mutate(sdat,
    sero_date =  as.Date(start_date + sero_days, origin="1970-01-01"))
  left_join(rtdat, sdat, by="IIntID") 
}


icCompute <- function(flist=list(
  year = AggByYear, crude = calcCrudeInc)) {
  return(flist) 
}

icCombine <- function(slist=list(
  sero=c("year", "sero_event"),
  pyears=c("year", "pyears"),
  crude_est=c("crude", "fit"),
  crude_se=c("crude", "se.fit"))) {
  return(slist)
}

icExtract <- function(flist=list(
  agg=getMeans("sero", "pyears"),
  crude=getRubin("crude_est", "crude_se"))) {
  return(flist)
}

#' @title setIncIC
#' 
#' @description Sets the data and functions to calculate incidence estimates for IntCens.
#' 
#' @param dat Dataset of covariate values.
#' @param rtdat Dataset from \code{\link{getRTData}}.
#' @param sdates Dataset of imputes events from \code{\link{intCensImpute}}.
#' @param bdat Dataset of birthdays from \code{\link{getBirthDate}}.
#' @param Args provide arguments from \code{\link{setArgs}}.
#' @param fun A list of functions to compute, default is \code{\link{miCompute}}.
#' 
#' @return data.frame
#'
#' @export
setIncIC <- function(dat, rtdat, 
  sdates, bdat, Args, fun=miCompute()) {
  message('Calculating incidence...')
  function(i) {
    cat(i, "")
    dat <- imputeIntCensPoint(rtdat, sdates, i)
    dat <- splitAtSeroDate(dat) 
    dat <- setData(dat, Args,  bdat)
    lapply(fun, function(f) f(dat))
  }
}


#' @title getIncidenceIC
#' 
#' @description Calculates the incidence rates using IntCens methods.
#' 
#' @param Args Takes list from \code{\link{setArgs}}.
#' @param Compute Takes list from \code{\link{icCompute}}.
#' @param Combine Takes list from \code{\link{icCombine}}.
#' @param Extract Takes list from \code{\link{icExtract}}.
#'
#' @return data.frame
#'
#' @export
getIncidenceIC <- function(Args, dat, rtdat, 
  Compute=icCompute(), Combine=icCombine(), 
  Extract=icExtract()) {
  Results <- intCensParse(File=
    file.path(derived, paste0(Args$aname,"_out.txt")))
  sdates <- intCensImpute(dat, Results, Args)
  bdat=getBirthDate()
  calcInc <- setIncIC(dat, rtdat, sdates, bdat,
    Args, fun=Compute)
  dat <- parallel::mclapply(seq(Args$nSim),
    calcInc, mc.cores=Args$mcores)
  cdat <- combineEst(dat, get_names=Combine) 
  lapply(Extract, function(f) f(cdat))
}
