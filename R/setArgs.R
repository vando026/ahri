#' @title setFiles
#' 
#' @description  Set the arguments for functions. 
#' 
#' @param inFiles list of files to pass to function.
#' 
#' @param Years numeric vector of years.
#'
#' @param Sex character of "All", "Fem", "Mal".
#'
#' @param Age numeric vector of ages.
#'
#' @param AgeCat numeric vector of age categories.
#'
#' @param Seed set the random seed.
#'
#' @param nSimulations number of simulations to perform.
#'
#' @param ResRule proportion of time spent in DSA, greater than >.
#'
#' @param aname root name to associate with output or filenames.
#'
#' @param printout print out results.
#'
#' @return list
#'
#' @examples
#' inFiles <- getFiles()
#' Args <- setArgs(inFiles, Years=c(2004:2015), Sex="Mal")

setArgs <- function( 
  inFiles=NULL,
  Years=c(2004:2015),
  Sex="All",
  Age=setAge(),
  AgeCat=seq(15, 55, 5),
  ResRule=0,
  nSimulations=5, 
  Seed=300500,
  imputeMethod=imputeRandomPoint,
  aname='filename',
  printout=FALSE) {

  out <- list(
    inFiles=inFiles,
    Years=Years,
    Sex=Sex,
    Age=setAge(Age),
    AgeCat=AgeCat,
    FemCode=switch(Sex,
      Mal=0,Fem=1,All=c(0, 1)),
    ResRule=ResRule,
    nSimulations=nSimulations,
    imputeMethod=imputeMethod,
    Seed=Seed,
    printout=printout
  )
  return(out)
}

setAge <- function(newList=NULL) {
  defList=list(
    Fem=c(15, 49), 
    Mal=c(15, 54),
    All=c(15, 54))
  if (is.null(newList)) 
    return(defList)
  modifyList(defList, newList)
}
