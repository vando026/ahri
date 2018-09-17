#' @title getAgeData
#' 
#' @description  gets Age in a given surveillance year
#' 
#' @param dat dataset for which age is needed at a given episode.
#'
#' @param idat individuals dataset from \code{\link{getFiles}}.
#'
#' @param Args takes a list from \code{\link{setArgs}}. 
#'
#' @return data.frame
#'
#' @import dplyr
#'
#' @export
#' 
#' @examples
#' rtdata <- getRTData(hiv)
#' sdat <- splitRTData(rtdata)
#' adat <- getAgeData(sdata, idat)

getAgeData <- function(dat, idat,
  Args=eval.parent(quote(Args))) {

  adat <- left_join(dat, idat, by="IIntID")
  adat <- filter(adat, !is.na(DateOfBirth)) %>%
    mutate(Age = floor(as.numeric(
    difftime(obs_end, DateOfBirth, units='weeks'))/52.25))

  # Make Categories
  adat <- mutate(adat, AgeCat = 
    cut(Age, breaks=Args$AgeCat, 
    labels=NULL, right=FALSE))

  if ("All" %in% names(Args$Age)) {
    adat <- filter(adat, !(Age < Args$Age[["All"]][1]) &
      !(Age > Args$Age[["All"]][2]))
  } 
  if ("Mal" %in% names(Args$Age)) {
    adat <- filter(adat, !(Female==0 & Age < Args$Age[["Mal"]][1]) &
      !(Female==0 & Age > Args$Age[["Mal"]][2]))
  }
  if ("Fem" %in% names(Args$Age)) {
    adat <- filter(adat, !(Female==1 & Age < Args$Age[["Fem"]][1]) &
      !(Female==1 & Age > Args$Age[["Fem"]][2]))
  }

  adat[, !(names(adat) %in% "DateOfBirth")] 
}

#' @title makeAgeVars
#' 
#' @description  Centers age variable and takes square and cube.
#' 
#' @param dat dataset for which age is needed at a given episode.
#'
#' @return data.frame
#'
#' @import dplyr
#'
#' @export
#' 
#' @examples
#' adat <- getAgeData(sdata)
#' adat <- makeAgeVars(adat)

makeAgeVars <- function(dat){
  dat <- mutate(dat, 
    Age0 = Age - mean(Age), 
    Age2 = Age0^2, Age3 = Age0^3)
  tbl_df(dat) 
}
