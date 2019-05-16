#' @title IntCensParse
#' 
#' @description  Code to get IntCens results from output.
#' 
#' @param File File path to output.txt.
#'
#' @export
#'
IntCensParse <- function(File=NULL) {

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
#' @param dat A dataset
#' @param Results Results from \code{\link{IntCensParse}}.
#' @param Args
#' 
#' @return 
#'
#' @export 

intCensImpute <- function(dat, Results, Args) {
  G = function(x)  return(x)

  # simulate from the multivariate normal distribution of the 
  # regression parameter estimates
  betaMeans <- Results$edat[, "Estimate"]
  betaCovariance  <- Results$cdat
  regParamsSim = rmvnorm(n=Args$nSim,
    mean = betaMeans, sigma = as.matrix(betaCovariance))

  # step function for the baseline hazard
  survTime <- Results$sdat[, "Time"]
  Estimate <- Results$sdat[, "Estimate"]
  baselineHazard  <-  stepfun(x=c(0, survTime),
    y=c(0, Estimate, max(Estimate)),
    right=FALSE)

  # Work only with HIV+
  dat <- as.data.frame(dat[!is.infinite(dat$early_pos), ])
  allIDs = sort(unique(dat$IIntID))

  doFunc <- function(oneID, dat, Args) {
    # message(sprintf("Running for %s ", oneID))
    oneIDdata <- dat[dat$IIntID==oneID, ]
    stopifnot(nrow(oneIDdata)>0)
    leftTime <- with(oneIDdata, 
      as.numeric(difftime(late_neg[1], obs_start0[1], units='days')))
    rightTime <- with(oneIDdata,
      as.numeric(difftime(early_pos[1], obs_start0[1], units='days')))

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
      for(i in seq_len(length(variableNames)))
      {
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
          # exPlot(oneID, distF, baselineHazard, leftTime, rightTime)
          if (Args$MoreArgs$iSmooth & length(xbase)>=4) {
            smEst <- smooth.spline(xbase, valF, df=4)
            valFS <- predict(smEst, xbase)
            distF  <-  stepfun(x=xbase, y=c(valFS$y, max(valFS$y)), right=FALSE)
          } 
          seroDist <- c(0, diff(distF(AllSeroTimes)))
          # Its likely that the smoothing can give a neg probability
          seroDist[seroDist<0] <- 0
          mysum = sum(seroDist)
          if(0==mysum) {
            SeroTimes[asim] = sample(x=AllSeroTimes,size=1)
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
    c(IIntID=oneID, late_neg=leftTime, early_pos=rightTime, SeroTimes) 
  }
  out <- mclapply(allIDs, function(i) doFunc(i, dat, Args))
  data.frame(do.call("rbind", out))
}


#' @title imputeIntCensPoint
#' 
#' @description  Imputes the dates from \code{\link{intCensImpute}}.
#' 
#' @param dat The imputed dateset.
#' 
#' @return 
#'
#' @export 
imputeIntCensPoint <- function(dat) {
  sdates <- get("sdates", envir=parent.frame())
  i <- get("i", envir=environment())
  si <- paste0("s", i)
  sdates <- sdates[, c("IIntID", si)]
  names(sdates) <- c("IIntID", "sero_days")
  dat <- left_join(dat, sdates, by="IIntID")
  dat <- mutate(dat,
    sero_date= ifelse(sero_event==1, obs_start + sero_days, NA),
    sero_date = as.Date(sero_date, origin="1970-01-01"))
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
#' 
#' @return list
#'
#' @export 
SetUniReg <- function(modVars) {
  function(aname) {
    UniReg(
      InFile=file.path(derived, paste0(aname,".txt")), 
      OutFile=file.path(derived, paste0(aname, "_out.txt")), 
      Model = paste0("(Time, sero_event) = ", modVars), 
      ID="IIntID", printout=TRUE, ign_stout=FALSE, cthresh=0.01)
    }
}
