#' @title setAge
#' 
#' @description  sets age according to arguments
#' 
#' @param dat must have a variable named AgeAtVisit  
#' 
#' @param Args requires Args, see \code{\link{setArgs}}
#'
#' @return data.frame
#'
#' @export 

setAge <- function(dat, Args) {
  if ("All" %in% names(Args$Age)) {
    dat <- filter(dat, !(AgeAtVisit < Args$Age[["All"]][1]) &
      !(AgeAtVisit > Args$Age[["All"]][2]))
  } 
  if ("Mal" %in% names(Args$Age)) {
    dat <- filter(dat, !(Female==0 & AgeAtVisit < Args$Age[["Mal"]][1]) &
      !(Female==0 & AgeAtVisit > Args$Age[["Mal"]][2]))
  } 
  if ("Fem" %in% names(Args$Age)) {
    dat <- filter(dat, !(Female==1 & AgeAtVisit < Args$Age[["Fem"]][1]) &
      !(Female==1 & AgeAtVisit > Args$Age[["Fem"]][2]))
  }
  dat
}
