#' @title getAgeWeights
#' 
#' @description  Get age weights from all residents in DSA.
#' 
#' @param Args requires Args, see \code{\link{setArgs}}.
#' 
#' @return 
#'
#' @export 

getAgeWeights <- function(Args) {
  dat <- getDemResident(Args)
  dat <- filter(dat, Year %in% Args$Year)
  tapply(dat$Age, dat$Year, mean)
}

#' @title getAgeYear
#' 
#' @description  Calculate Age distribution by year of resident.
#' 
#' @param Args.
#' 
#' @return 
#'
#' @export 
getAgeYear <- function(Args) {
  mn_age <- getAgeWeights(Args)
  dat <- data.frame(Age = mn_age, tscale=1,
    Year = factor(Args$Year, levels = Args$Year, 
    labels = levels(as.factor(Args$Year))))
  rownames(dat) <- Args$Year
  dat
}
