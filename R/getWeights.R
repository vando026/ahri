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
  mn_age <- mn_age[rownames(mn_age) %in% Args$Year]
  data.frame(Age = mn_age, tscale=1,
    Year = factor(Args$Year, levels = Args$Year, 
    labels = levels(as.factor(Args$Year))))
}
