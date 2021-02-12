#' @title getMortalityData
#' 
#' @description  gets mortality data.
#' 
#' @param Args see \code{\link{setArgs}}.
#' @param startDate string variable at which person-time starts. Use 
#' HIVPositive for AIDS-related mortality, otherwise only those with an HIVNegative test
#' or those with the EarliestTest from HIVSurveillance. Or use ObservationStart from the Episodes dataset. 
#' 
#' @return data.frame
#'
#' @import dplyr
#' @keywords internal


getMortalityData <- function(Args, 
  startVar="HIVPositive", dropHIVPos=FALSE) {
  #
  edat <- getEpisodes()
  idat <- getIndividual()
  hiv <- getHIV()
  # Get start date
  if (startVar=="ObservationStart") {
    dat <- edat
  } else {
    dat <- dplyr::mutate(hiv, EarliestTest = pmin(HIVNegative, HIVPositive, na.rm=TRUE))
  }
  dat <- dplyr::select(dat, IIntID, obs_start = matches(startVar)) %>%
    dplyr::filter(is.finite(obs_start))
  startdat <- getDatesMin(dat, "obs_start", "obs_start")
  #
  # Get all death dates
  dodat  <- dplyr::select(idat, IIntID, DoD) %>% 
    dplyr::filter(is.finite(DoD))
  # Get last observation date
  enddat <- getDatesMax(edat, "ObservationEnd", "end_date")
  # Make obs_end as death or last obs date
  enddat <- dplyr::left_join(enddat, dodat, by="IIntID")
  enddat <- dplyr::mutate(enddat,
    obs_end = ifelse(is.finite(DoD), DoD, end_date),
    event = as.numeric(is.finite(DoD)))
  #
  # merge obs_start and obs_end dates 
  sdat <- dplyr::left_join(startdat, enddat, by="IIntID") %>% 
    dplyr::select(IIntID, obs_start, obs_end, event)
  sdat <- dplyr::filter(sdat, obs_end>obs_start)
  # Only HIV-negative mortality
  if(startVar=="ObservationStart" & dropHIVPos) {
     HIVPos <-  dplyr::group_by(hiv, IIntID) %>% 
      summarize(HIVResult = max(HIVResult, na.rm=TRUE)) 
     sdat <- dplyr::left_join(sdat, HIVPos, by="IIntID")
     sdat <- dplyr::filter(sdat, HIVResult %in% c(NA, 0))
  }
  # split data
  tdat <- splitData2(sdat)
  bdat <- getBirthDate(addVars="Female") 
  tdat <- setData(tdat, Args, time2="obs_start", birthdate=bdat)
  tdat <- dplyr::mutate(tdat, Days = as.numeric(obs_end-obs_start))
  if(any(tdat$Days > 366)) stop("Days > 366")
  tdat
} 

#' @title calcMortality
#' 
#' @description Calculates annual mortality rates from \code{\link{getMortalityData}}.
#' 
#' @param  dat a dataset
#' 
#' @return table
#'
#' @import epitools
#' @keywords internal
#' @export 

calcMortality <- function(dat) {
  dat <- group_by(dat, Year) %>% 
    summarize(N=length(unique(IIntID)), 
      Count=sum(event), PYears=sum(Days)/365.25) %>%
    mutate(rate = Count/PYears * 100)
  out <- epitools::pois.exact(dat$Count, dat$PYears)
  out <- select(out, Deaths=x, PTime=pt, Rate=rate, LB=lower, UB=upper)
  out[c(3:5)] <- lapply(out[c(3:5)], "*", 100)
  cbind(Year=dat$Year, N=dat$N, out)
}
