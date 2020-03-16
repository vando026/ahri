#' @title setArgs
#' 
#' @description  Set the arguments for functions. 
#' 
#' @param Years numeric vector of years.
#'
#' @param Age list of ages as in \code{list(Fem=c(15, 49), Mal=c(15, 54)}.
#'
#' @param AgeCat numeric vector of age categories, e.g. \code{seq(15, 55, by=5)}.
#'
#' @param AgeBy integer value for size age categories.
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
#' Args <- setArgs(Years=c(2004:2015), Age=list(Mal=c(15, 54), Fem=c(15, 49)))

setArgs <- function( 
  Years=c(2005:2018),
  Age=list(All=c(15, 54)),
  AgeCat=NULL,
  AgeBy=5,
  ResRule=0,
  nSim=1, 
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
      max(unlist(Age)) + AgeBy, AgeBy)
  }
  Sex <- ifelse(
    setequal(names(Age), c("Mal", "Fem")), 
    "All", names(Age))
  FemCode=switch(Sex,
    Mal=0,Fem=1,All=c(0, 1))
  as.list(environment())
}

#' @title getCol
#' 
#' @description Save RColorbrewer colors to global environment 
#' 
#' @return 
getColor <- function() {
  return(list(
    Blues = RColorBrewer::brewer.pal(9, "Blues"),
    YlRed = RColorBrewer::brewer.pal(9, "YlOrRd"),
    Blues = RColorBrewer::brewer.pal(9, "Blues"),
    Reds =  RColorBrewer::brewer.pal(9, "Reds"),
    Greens = RColorBrewer::brewer.pal(9, "Greens"),
    Set1 = RColorBrewer::brewer.pal(9, "Set1")))
}
