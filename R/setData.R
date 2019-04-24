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

getBirthDate <- function(
  inFile=getFiles()$epifile, addVars=" ") {
  dat <- getEpisodes(inFile) 
  dat <- select(dat, IIntID, DateOfBirth=DoB, contains(addVars))
  dat <- distinct(dat, IIntID, .keep_all=TRUE)
  dat <- filter(dat, !is.na(DateOfBirth))
  dat <- filter(dat, as.numeric(format(DateOfBirth, "%Y")) > 1910)
  dat
}

#' @title setData
#' 
#' @description  Sets data according to values in \code{\link{setArgs}}.
#' 
#' @param dat Dataset for which age is needed at a given episode.
#'
#' @param bdat Date of Birth variable from \code{\link{getBirthDate}}.
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
#' bdat <- getBirthDate(Args$inFiles$epifile)
#' sdat <- splitAtEarlyPos(rtdat)
#' adat <- setData(sdat, Args, bdat)

setData <- function(dat, Args,
  bdat=getBirthDate(), 
  time2 = "obs_end") {
  # For specific datasets
  if(!("AgeAtVisit" %in% names(dat))) {
    dat <- data.frame(left_join(dat, bdat, by="IIntID"))
    dat$AgeAtVisit <- floor(as.numeric(difftime(
      dat[,time2], dat[,"DateOfBirth"], units='weeks'))/52.25)
    dat <- select(dat, -(DateOfBirth))
  }
  # Keep sex
  dat <- filter(dat, Female %in% Args$FemCode)
  # Filter by Age limits
  dat <- setAge(dat, Args)
  # Make Categories
  dat <- mutate(dat, AgeCat = 
    cut(AgeAtVisit, breaks=Args$AgeCat, 
    labels=NULL, right=FALSE))
  dat$AgeCat <- droplevels(dat$AgeCat)
  # Filter by year
  dat <- filter(dat, Year %in% Args$Years)
  # Further subsetting to take place if needed
  dat <- Args$setFun(dat)
  dat <- rename(dat, Age = AgeAtVisit)
  dat <- Args$addVars(dat)
  as_tibble(dat)
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
#' adat <- setAge(sdata)
#' adat <- makeAgeVars(adat)

makeAgeVars <- function(dat){
  dat <- mutate(dat, 
    Age0 = Age - mean(Age), 
    Age2 = Age0^2, Age3 = Age0^3)
  tbl_df(dat) 
}

