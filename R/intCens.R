#' @title readUniRegResults
#' 
#' @description  Read the results from the \code{\link{UniReg}} model into .Rdata
#' format. 
#' 
#' @param File File path to \code{UniReg} output.txt.
#'
#' @export
#'
readUniRegResults <- function(File=NULL) {

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


#' @title gImpute
#' 
#' @description  Uses the G-imputation method to impute the infection times.
#' 
#' @param dat A dataset.
#' @param Results Results from \code{\link{readUniRegResults}}.
#' @param Args provide arguments from \code{\link{setArgs}}.
#' @param start_date If null, start_date is the first obs_start date of ID, else it is
#' same start_date for everyone. Must be a string in the following YYYY-MM-DD format: e.g. "2005-01-23".
#' 
#' @return 
#'
#' @export 

gImpute <- function(dat, Results, Args, start_date=NULL) {

  message("Running intCensImpute...")
  # G = function(x)  return(x)
  G = function(x)  return(log(1 + x))

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
    oneIDdata <- dat[dat$IIntID==oneID, ]
    stopifnot(nrow(oneIDdata)>0)
    browser()
    start_time <- ifelse(is.null(start_date), 
      as.character(oneIDdata$obs_start[1]), start_date)
    if (oneIDdata$late_neg[1] < start_time) {
      print(oneIDdata)
      stop("Reconcile: Latest HIV negative date (late_neg) is before observation start (obs_start).")
    }
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
      message(sprintf("=====Issue for %s ", oneID))
      message("  No infection times imputed")
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




#' @title uniReg
#' 
#' @description  Wrapper for Intcens executable by Zeng et al 2016. See
#' http://dlin.web.unc.edu/software/intcens/ for details. The executable is shipped with 
#' the \code{ahri} package. Type in the R console \code{system.file("intcens", "unireg.exe", package =
#' "ahri")}. 
#' 
#' @param InFile txt file to be input
#' 
#' @param OutFile txt file to be output
#' 
#' @param Model equation to be given
#'
#' @param ID name of subject ID
#' @param iter Number of iterations
#' @param cthreshold Threshold for convergence
#' @param r Threshold for convergence
#' @param printout Print results to screen
#' @param ign_stout For Linux systems
#'
#' @export
#'
#' @examples
#' UniReg(
#' InFile = file.path(output, "TestSim.txt"),
#' OutFile = file.path(output, "TestSim_Out.txt"), r = 0.0,
#' Model = "(time, d) = v1 + v2 + v3",
#' ID = "id")


uniReg <- function(InFile, OutFile, Model, ID=NULL, inf="Inf",
  iter=5000, cthresh=0.0001, r=1.0, printout=FALSE, ign_stout=TRUE) {
    InFile <- paste("--in", InFile)
    OutFile <- paste("--out", OutFile)
    Model <- paste("--model", shQuote(Model))
    ID <- ifelse(is.null(ID), "", paste("--subject_id", ID))
    Sep <- paste("--sep", shQuote(" "))
    inf <- paste("--inf_char", inf)
    R <- paste("--r", r)
    iter <- paste("--max_itr", iter)
    cthresh <- paste("--convergence_threshold", cthresh)
    xpath <- system.file("intcens", "unireg.exe", package = "ahri")
    if (Sys.getenv("OS") == "Windows_NT") {
      system(command=paste(xpath, InFile, OutFile, Model, 
        ID, Sep, iter, R, inf, cthresh, collapse=" "),
        show.output.on.console=printout)
    } else {
      system(command=paste(xpath, InFile, OutFile, Model, 
        ID, Sep, iter, R, inf, cthresh, collapse=" "),
        ignore.stdout=ign_stout)
    }
}

#' @title setUniReg
#' 
#' @description  Helper function to run \code{\link{uniReg}}.
#' 
#' @param  Vars Variables to feed into uniReg model.
#' @param  aName Name of output txt file.
#' 
#' @return list
#'
#' @export 
setUniReg <- function(Vars, aName) {
  uniReg(
    InFile=file.path(derived, paste0(aName,".txt")), 
    OutFile=file.path(derived, paste0(aName, "_out.txt")), 
    Model = paste0("(Time, sero_event) = ", Vars), 
    ID="IIntID", printout=TRUE, ign_stout=FALSE, cthresh=0.01)
}


#' @title uniRegOne
#' 
#' @description  Run IntCens on each variable and make table
#' 
#' @param Vars Vector of RHS character varnames. 
#' @param aName File name of the IntCens results from \code{\link{intCensParse}}. 
#'
#' @export
#' @keywords internal
#' @examples 
#' uniRegOne(c("Age0", "Age2"), aName="icens_mal")
uniRegOne <- function(Vars, Args) {
  i <- 1
  for(vari in Vars) {
    setUniReg(vari, Args$aname)
    res <- intCensParse(File=
      file.path(derived, paste0(Args$aname,"_out.txt")))
    res <- res$edat
    if (i==1)
      dat <- res
    else  
      dat <- rbind(dat, res)
    i <- i + 1
  }
  dat
}

#' @title getGImpute
#' 
#' @description  Adds the ith column of imputed infection times from \code{\link{gImpute}} to an
#' existing dataset.
#' 
#' @param rtdat An existing dataset generated from \code{\link{getRTData}}. 
#' @param gdat The imputed dateset from \code{\link{gImpute}}.
#' @param i The ith imputed infection time in gdat.
#' 
#' @return 
#'
#' @export 
getGImpute <- function(rtdat, gdat, i) {
  gdat <- gdat[, c("IIntID", "start_date", paste0("s", i))]
  names(gdat) <- c("IIntID", "start_date", "sero_days")
  gdat <- mutate(gdat,
    sero_date =  as.Date(start_date + sero_days, origin="1970-01-01"))
  left_join(rtdat, gdat, by="IIntID") %>% 
    select(-c(start_date, sero_days))
}


