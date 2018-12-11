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

#' @title getAgeData
#' 
#' @description  gets and sets Age of participant in a surveillance year.
#' 
#' @param dat dataset for which age is needed at a given episode.
#'
#' @param idat Individuals dataset (Date of Birth variable) from \code{\link{getBirthDate}}.
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
#' idat <- getBirthDate(Args$inFiles$epifile)
#' sdat <- splitAtEarlyPos(rtdat)
#' adat <- getAgeData(sdat, idat)

getAgeData <- function(dat, idat,
  Args=eval.parent(quote(Args))) {
  # merge datasets
  adat <- left_join(dat, idat, by="IIntID")
  adat <- filter(adat, !is.na(DateOfBirth)) %>%
    mutate(AgeAtVisit = floor(as.numeric(
    difftime(obs_end, DateOfBirth, units='weeks'))/52.25))
  # Make Categories
  adat <- mutate(adat, AgeCat = 
    cut(AgeAtVisit, breaks=Args$AgeCat, 
    labels=NULL, right=FALSE))
  # Filter by Age limits
  adat <- setAge(adat, Args)
  adat$AgeCat <- droplevels(adat$AgeCat)
  select(adat, -(DateOfBirth)) %>% rename(Age = AgeAtVisit)
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

