#' @title IntCensParse
#' 
#' @description  Code to get IntCens results from output
#' 
#' @param File File path to output
#'
#' @export
#'
IntCensParse <- function(File=NULL) {

  out <- readLines(File)
  out <- gsub("\\t", " ", out)

  # get lines of surv data
  time_ln <- grep("^\\<Time\\>[[:space:]]+Estimate", out)
  surv_dat <- out[(time_ln+1):length(out)] 
  surv_dat <- strsplit(surv_dat, " ")
  surv_dat <- do.call(rbind, surv_dat)
  surv_dat <- data.frame(apply(surv_dat, 2, as.numeric))
  colnames(surv_dat) <-  c("Time", "Estimate")

  # Now get estimates into R object
  Cov_ln <- grep("^\\<Covariate\\>", out)
  Cov_ln2 <- grep("[Co]?[vV]ariance", out)-1
  emat <- out[(Cov_ln+1):Cov_ln2] 
  emat <- strsplit(emat, " ")
  emat <- data.frame(do.call(rbind, emat), stringsAsFactors=FALSE)
  emat[, -1] <- apply(emat[, -1], 2, as.numeric)
  colnames(emat) <-  unlist(strsplit(out[Cov_ln], " "))

  # # Now covariance estimates into R object
  line1 <- unlist(strsplit(grep("[Co]?[vV]ariance", out, value=TRUE), ' '))
  lset <- ifelse(length(line1)==1, 1, 2) 
  cov_ln <- grep("[Co]?[vV]ariance", out) + lset
  cov_ln2 <- grep("Cummulative Hazard", out)-1
  cmat <- out[cov_ln:cov_ln2] 
  cmat <- strsplit(cmat, " ")
  cmat <- data.frame(do.call(rbind, cmat), stringsAsFactors=FALSE)
  nm <- gsub(":", "", cmat[, 2])
  cmat <- cmat[, -c(1,2)]
  cmat[, -1] <- apply(cmat[, -1], 2, as.numeric)
  colnames(cmat) <-  rownames(cmat) <- nm

  list(sdat=surv_dat, edat=emat, cmat=cmat)
}


#' @title UniReg
#' 
#' @description  wrapper for IntCens fuction
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
