#' @title setFiles
#' 
#' @description  set the arguments 
#' 
#' @param inFiles list of files to pass to function
#' 
#' @param Years numeric vector of years 
#'
#' @param Sex character of "All", "Fem", "Mal"
#'
#' @param Age numeric vector of ages
#'
#' @param AgeCat numeric vector of age categories
#'
#' @param ResRule proportion of time spent in DSA, greater than >
#'
#' @param printout print out results 
#'
#' @return list
#'
#' @examples
#' Args <- setArgs(inFiles="ARTemis.dta", Years=c(2004:2015))

setArgs <- function( 
  inFiles=NULL,
  Years=c(2004:2015),
  Sex="All",
  Age=c(15, 54),
  AgeCat=seq(15, 55, 5),
  ResRule=0,
  nSimulations=5, 
  printout=FALSE) {

  out <- list(
    inFiles=inFiles,
    Years=Years,
    Age=Age,
    AgeCat=AgeCat,
    Sex=Sex,
    FemCode=
      switch(Sex, Male=0, Female=1, All=c(0, 1)),
    ResRule=ResRule,
    Seed=Seed,
    printout=printout,
  )
  return(out)
}

