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
    dat <- filter(dat, !(Age < Args$Age[["All"]][1]) &
      !(Age > Args$Age[["All"]][2]))
  } 
  if ("Mal" %in% names(Args$Age)) {
    dat <- filter(dat, !(Female==0 & Age < Args$Age[["Mal"]][1]) &
      !(Female==0 & Age > Args$Age[["Mal"]][2]))
  } 
  if ("Fem" %in% names(Args$Age)) {
    dat <- filter(dat, !(Female==1 & Age < Args$Age[["Fem"]][1]) &
      !(Female==1 & Age > Args$Age[["Fem"]][2]))
  }
  dat
}

#' @title getBirthDate
#' 
#' @description  Gets birth dates from \code{\link{getEpisodes}} data
#' 
#' @param inFile File path to the dataset, default is set to \code{\link{getFiles}}
#'
#' @param addVars String for a regular expression to select additional vars
#' 
#' @return data.frame
#'
#' @import dplyr
#'
#' @export 
#'
#' @examples
#' Args <- setArgs()
#' getBirthDate(addVars="Female")

getBirthDate <- function(addVars=" ") {
  dat <- getIndividual() 
  dat <- select(dat, IIntID, DateOfBirth, contains(addVars))
  dat <- filter(dat, !is.na(DateOfBirth))
  dat <- filter(dat, as.numeric(format(DateOfBirth, "%Y")) > 1910)
  dat
}


#' @title makeAgeVars
#' 
#' @description  Function for making age variables
#' 
#' @param dat dataset for which age is needed at a given episode.
#' @param visitdate Variable name as string for Date of visit.
#' @param age_cut Vector of ages to make age categories, if NULL then mean centered age
#' and age-squared variables made.
#' @param bdat Dataset of birthdates, if NULL then it uses \code{\link{getBirthDate}}.
#'
#' @return data.frame
#'
#' @import dplyr
#'
#' @export
#' 
#' @examples
#' adat <- setAge(sdata)
#' adat <- makeAgeVars(adat)

makeAgeVars <- function(dat, visitdate=NULL, age_cut=NULL, bdat=NULL){
  if (is.null(bdat)) bdat <- getBirthDate()
  dat <- data.frame(left_join(dat, bdat, by="IIntID"))
  dat$Age <- floor(as.numeric(difftime(
    dat[,visitdate], dat[,"DateOfBirth"], units='weeks'))/52.25)
  dat <- select(dat, -(DateOfBirth))
  if(!is.null(age_cut)) {
    dat <- mutate(dat, AgeCat = cut(Age, breaks=age_cut,
      include.lower=TRUE, right=FALSE, labels=NULL))
    dat$AgeCat <- droplevels(dat$AgeCat)
  }
  tibble::as_tibble(dat) 
}

#' @title setData
#' 
#' @description  Sets data according to values in \code{\link{setArgs}}.
#' 
#' @param dat Dataset for which age is needed at a given episode.
#'
#' @param Args takes a list from \code{\link{setArgs}}. 
#'
#' @param time2 A date variable that is used to calculate age from Birthdate to time2.
#'
#' @return data.frame
#'
#' @import dplyr
#'
#' @export
#' 
#' @examples
#' rtdata <- getRTData(hiv)
#' sdat <- splitAtEarlyPos(rtdat)
#' adat <- setData(sdat, Args)

setData <- function(dat, Args, time2=NULL, birthdate=NULL) {
  # Filter by Age limits
  if (!is.null(time2)) {
    dat <- makeAgeVars(dat, visitdate=time2,
      age_cut=Args$AgeCat, bdat=birthdate)
  } 
  dat <- setAge(dat, Args)
  dat <- filter(dat, Female %in% Args$FemCode)
  dat <- filter(dat, Year %in% Args$Years)
  # further actions to take place
  dat <- Args$setFun(dat)
  dat <- Args$addVars(dat)
  as_tibble(dat)
}

