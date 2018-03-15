#' @title setArgs
#' 
#' @description  Set the arguments for functions. 
#' 
#' @param inFiles list of files to pass to function.
#' 
#' @param Years numeric vector of years.
#'
#' @param Sex character of "All", "Fem", "Mal".
#'
#' @param Age list of ages as in \code{list(Fem=c(15, 49)}.
#'
#' @param AgeCat numeric vector of age categories.
#'
#' @param nSimulations number of simulations to perform.
#'
#' @param ResRule proportion of time spent in DSA, greater than >.
#'
#' @param aname root name to associate with output or filenames.
#'
#' @param imputeMethod select either \code{imputeRandomPoint} or \code{imputeMidPoint}.
#'
#' @param printout print out results.
#'
#' @param MoreArgs a list, which adds more arguments if needed.
#'
#' @return list
#'
#' @examples
#' inFiles <- getFiles()
#' Args <- setArgs(inFiles, Years=c(2004:2015), Sex="Mal")
#' Args <- setArgs(inFiles, Years=c(2004:2015), MoreArgs=list(knots=2))

setArgs <- function( 
  inFiles=getFiles(),
  Years=c(2004:2015),
  Age=list(All=c(15, 54)),
  AgeCat=seq(15, 55, 5),
  ResRule=0,
  nSimulations=500, 
  imputeMethod=imputeRandomPoint,
  aname='filename',
  printout=FALSE,
  MoreArgs=NULL) {

  Sex <- ifelse(
    setequal(names(Age), c("Mal", "Fem")), 
    "All", names(Age))

  out <- list(
    inFiles=inFiles,
    Years=Years,
    Sex=Sex,
    Age=Age,
    AgeCat=AgeCat,
    FemCode=switch(Sex,
      Mal=0,Fem=1,All=c(0, 1)),
    ResRule=ResRule,
    nSimulations=nSimulations,
    imputeMethod=imputeMethod,
    aname=aname,
    printout=printout
  )
  if (!is.null(MoreArgs))
    out <- append(out, MoreArgs)
  return(out)
}

