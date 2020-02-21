#' @title setArgs
#' 
#' @description  Set the arguments for functions. 
#' 
#' @param inFiles list of files to pass to function.
#' 
#' @param Years numeric vector of years.
#'
#' @param Age list of ages as in \code{list(Fem=c(15, 49), Mal=c(15, 54)}.
#'
#' @param AgeCat numeric vector of age categories, e.g. \code{seq(15, 55, by=5)}.
#'
#' @param nSim number of simulations to perform.
#'
#' @param ResRule proportion of time spent in DSA, greater than >.
#'
#' @param aname root name to associate with output or filenames.
#'
#' @param imputeMethod select either \code{\link{imputeRandomPoint}} or
#' \code{\link{imputeMidPoint}}.
#'
#' @param printout print out results.
#'
#' @param mcores number of cores to use for parallel package (used in
#' \code{\link{getIncidence}}).
#'
#' @param MoreArgs a list, which adds more arguments if needed.
#'
#' @param setFun a function used by \code{\link{setData}} by which the data can be further subset.
#'
#' @param addVars a function used by \code{\link{setData}} by which additional variables
#' can be added. 
#'
#' @return list
#'
#' @export
#'
#' @examples
#' inFiles <- getFiles()
#' Args <- setArgs(inFiles, Years=c(2004:2015), All=list(Mal=c(15, 54)))
#' Args <- setArgs(inFiles, Years=c(2004:2015), MoreArgs=list(knots=2))

setArgs <- function( 
  inFiles=getFiles(),
  Years=c(2005:2018),
  Age=list(All=c(15, 54)),
  AgeCat=NULL,
  AgeBy=5,
  ResRule=0,
  nSim=500, 
  imputeMethod=imputeRandomPoint,
  aname='filename',
  printout=FALSE,
  mcores=1,
  setFun=identity,
  addVars=identity,
  MoreArgs=NULL) {
  #
  if (is.null(AgeCat)) {
    AgeCat=seq(min(unlist(Age)),
      max(unlist(Age))+1, AgeBy)
  }
  Sex <- ifelse(
    setequal(names(Age), c("Mal", "Fem")), 
    "All", names(Age))
  FemCode=switch(Sex,
    Mal=0,Fem=1,All=c(0, 1))
  #
  getFiles <- inFiles
  as.list(environment())
}

