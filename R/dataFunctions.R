#' @title getAgeData
#' 
#' @description  gets Age in a given surveillance year
#' 
#' @param dat dataset for which age is needed at a given episode.
#'
#' @param Args takes a list from \code{\link{setArgs}}. 
#'
#' @return data.frame
#'
#' @import dplyr
#'
#' @examples
#' rtdata <- getRTData(hiv)
#' sdat <- splitRTData(rtdata)
#' adat <- getAgeData(sdata)

getAgeData <- function(dat, 
  Args=eval.parent(quote(Args))) {

  idat <- read_csv(Args$inFiles$indfile, 
    col_types=cols_only(
      IIntID="i", DateOfBirth="D"))
  idat <- filter(idat, !duplicated(IIntID))

  adat <- left_join(dat, idat, by="IIntID")
  adat <- filter(adat, !is.na(DateOfBirth)) %>%
    mutate(Age = floor(as.numeric(
    difftime(obs_end, DateOfBirth, units='weeks'))/52.25))

  # Make Categories
  adat <- mutate(adat, AgeCat = 
    cut(Age, breaks=Args$AgeCat, 
    labels=NULL, right=FALSE))

  adat <- filter(adat, !(Female==0 & Age < Args$Age[["Mal"]][1]) &
    !(Female==0 & Age > Args$Age[["Mal"]][2]))
  adat <- filter(adat, !(Female==1 & Age < Args$Age[["Fem"]][1]) &
    !(Female==1 & Age > Args$Age[["Fem"]][2]))

  adat[, !(names(adat) %in% "DateOfBirth")] 
}
