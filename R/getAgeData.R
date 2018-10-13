#' @title getAgeData
#' 
#' @description  gets Age of participant in a surveillance year
#' 
#' @param dat dataset for which age is needed at a given episode.
#'
#' @param idat Individuals dataset (Date of Birth variable) from \code{\link{getFiles}}.
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
