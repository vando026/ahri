#' @title getIntCensDates
#' 
#' @description  Make HIV test dates for IntCens model
#' 
#' @param Args requires Args, see \code{\link{setArgs}}.
#' 
#' @return data.frame
#'
#' @export 

getIntCensDates <- function(Args) {
  #
  yr = getYearDates(Args$Years)
  getDates <- function(i) {
    yrs <- yr[yr>i$obs_start & yr <i$obs_end] 
    s1 <- unique(c(yrs, i$obs_start, i$obs_end))
    cbind(IIntID=i$IIntID, VisitDate=s1)
  }
  # GetDates
  dat <- getHIV(Args)
  early_neg <- getDatesMin(dat, "HIVNegative", "early_neg")
  late_neg <- getDatesMax(dat, "HIVNegative", "late_neg")
  early_pos <- getDatesMin(dat, "HIVPositive", "early_pos")
  dat <- distinct(dat, IIntID, Female)
  pdat <- Reduce(left_join, 
    list(dat, early_neg, late_neg, early_pos))
  pdat <- group_by(pdat, IIntID) %>% summarize(
    obs_start = min(early_neg, late_neg, early_pos, na.rm=TRUE),
    obs_end = max(early_neg, late_neg, early_pos, na.rm=TRUE)) 
  # Split by ID
  pdat <- split(pdat, pdat$IIntID)
  tdat <- as_tibble(do.call(rbind, lapply(pdat, getDates)))
  tdat <- Reduce(left_join, list(tdat, early_pos, dat))
  tdat <- arrange(tdat, IIntID, VisitDate)
  tdat <- mutate(tdat, 
    event = as.numeric(!is.na(early_pos) & VisitDate==early_pos),
    VisitDate = as.Date(VisitDate),
    Year = format(VisitDate, "%Y"))
  tdat <- select(tdat, -(early_pos))
  tdat <- setData(tdat, time2="VisitDate")
  tdat <- data.frame(tdat)
  Origin <- as.Date(paste0(Args$Year[1], "-01-01"), origin="1970-01-01")
  tdat$Time <- as.numeric(difftime(tdat$VisitDate, Origin), units="days")
  tdat
}


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
