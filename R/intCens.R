#' @title IntCensParse
#' 
#' @description  Code to get IntCens results from output
#' 
#' @param file file path to output
#'
#' @importFrom epitools binom.exact
#' 
#' @export
#'
IntCensParse <- function(file=NULL) {

  # Set temp folder
  tmp <- ifelse(Sys.getenv("R_PLATFORM")!="", 
    tempdir(), Sys.getenv("TEMP"))

  out <- readLines(file)

  # get lines of surv data
  time_ln <- grep("^\\<Time\\>[[:space:]]+Estimate", out)
  surv_dat <- out[time_ln:length(out)] 
  write.table(surv_dat, file=file.path(tmp, "surv_dat.txt"), quote=FALSE, 
    row.names=FALSE, col.names=FALSE)
  sdat <- read.table(file=file.path(tmp, "surv_dat.txt"), header=TRUE)
  # Now get estimates into R object
  Cov_ln <- grep("^\\<Covariate\\>", out)
  Cov_ln2 <- grep("[Co]?[vV]ariance", out)-1
  emat <- out[Cov_ln:Cov_ln2] 
  write.table(emat, file=file.path(tmp, "emat.txt"), quote=FALSE, 
    row.names=FALSE, col.names=FALSE)
  edat <- read.table(file=file.path(tmp, "emat.txt"),
   stringsAsFactors=FALSE, header=TRUE)

  # Now covariance estimates into R object
  line1 <- unlist(strsplit(grep("[Co]?[vV]ariance", out, value=TRUE), ' '))
  lset <- ifelse(length(line1)==1, 1, 2) 
  cov_ln <- grep("[Co]?[vV]ariance", out) + lset
  cov_ln2 <- grep("Cummulative Hazard", out)-1
  cmat <- out[cov_ln:cov_ln2] 
  write.table(cmat, file=file.path(tmp, "cmat.txt"), quote=FALSE, 
    row.names=FALSE, col.names=FALSE)
  cdat <- read.table(file=file.path(tmp, "cmat.txt"), 
    stringsAsFactors=FALSE, header=FALSE)
  cmat <- as.matrix(cdat[, -1])

  # get Likelihood for AIC
  str1 <- grep("^\\<Log-Likelihood\\>", out, value=TRUE)
  lhood <- regmatches(str1, gregexpr("-?\\d+(\\.\\d+)?", str1))
  aic <- 2*nrow(cmat) - 2*as.numeric(lhood)

  lout <- list(sdat=sdat, edat=edat, cdat=cmat, aic=aic)
  lout
}

#' @title getIntCens
#' 
#' @description  wrapper for IntCens fuction
#' 
#' @param aname name of input file
#' 
#' @param RHS variables
#'
#' @param out_name name of output file
#'
#' @import IntCens
#' 
#' @export
#'

getIntCens <- function(input, RHS, out_name=NULL) {
  if (is.null(out_name)) out_name <- paste0(input, '_out')
  unireg(input = file.path(derived, paste0(input,".txt")), 
    output = file.path(derived, paste0(out_name, ".txt")), 
    model = paste0("(TimeOrigin, sero_event) = ", RHS), 
    r = 0, subject_id = "IIntID")
}

#' @title uniRegTab
#' 
#' @description  wrapper for IntCens fuction
#' 
#' @param input name of input file
#' 
#' @param output name of output file
#'
#' @param RHS variables
#'
#' @import IntCens
#' 
#' @export
#'

uniRegTab <- function(input, output,  RHS) {
  fin <- file.path(derived, paste0(input, ".txt"))
  fout <- file.path(derived, paste0(output, "_uni_out.txt"))
  fname <- file.path(derived, paste0(output, "_unireg1.txt"))

  # Run unireg for each var
  icount=1
  for(vari in RHS) {
    unireg(input = fin, output = fout, 
      model = paste0("(TimeOrigin, sero_event) = ", vari),
      r = 0, subject_id = "IIntID")
    # Parse results
    results <- IntCensParse(file=fout)
    res <- results$edat
    print(res)
    if(icount==1) {
      write.table(res, fname, row.names=FALSE,
        col.names=TRUE)
    } else {
      write.table(res, fname, row.names=FALSE,
        append=TRUE, col.names=FALSE)
    }
    icount  <- icount + 1
  }
}

