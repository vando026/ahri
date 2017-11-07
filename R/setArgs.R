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
#' @param byVars string characters to age and sex adjustment.
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
  Age=list(Mal=c(15, 54), Fem=c(15, 49)),
  AgeCat=seq(15, 55, 5),
  ResRule=0,
  nSimulations=5, 
  byVars=c("Year"),
  Seed=NULL,
  aname='filename',
  printout=FALSE) {

  out <- list(
    inFiles=inFiles,
    Years=Years,
    Sex=Sex,
    Age=Age,
    AgeCat=AgeCat,
    FemCode=switch(Sex,
      Mal=0,Fem=1,All=c(0, 1)),
    byVars=byVars,
    LHS=paste(byVars, collapse='+'),
    ResRule=ResRule,
    nSimulations=nSimulations,
    Seed=Seed,
    printout=printout
  )
  return(out)
}

