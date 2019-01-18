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
#' @export 


getMortalityData <- function(Args, 
  startVar="HIVPositive", dropHIVPos=FALSE) {
  #
  edat <- getEpisodes(Args$inFiles$epifile)
  hiv <- getHIV(Args)
  # Get start date
  if (startVar=="ObservationStart") {
    dat <- edat
  } else {
    dat <- mutate(hiv, EarliestTest = pmin(HIVNegative, HIVPositive, na.rm=TRUE))
  }
  dat <- select(dat, IIntID, obs_start = matches(startVar)) %>%
    filter(is.finite(obs_start))
  startdat <- getDatesMin(dat, "obs_start", "obs_start")
  #
  # Get all death dates
  dodat  <- select(edat, IIntID, DoD) %>% 
    filter(is.finite(DoD)) %>% 
    distinct(IIntID, .keep_all=TRUE)
  # Get last observation date
  enddat <- getDatesMax(edat, "ObservationEnd", "end_date")
  # Make obs_end as death or last obs date
  enddat <- left_join(enddat, dodat, by="IIntID")
  enddat <- mutate(enddat,
    obs_end = ifelse(is.finite(DoD), DoD, end_date),
    event = as.numeric(is.finite(DoD)))
  #
  # merge obs_start and obs_end dates 
  sdat <- left_join(startdat, enddat, by="IIntID") %>% 
    select(IIntID, obs_start, obs_end, event)
  sdat <- filter(sdat, obs_end>obs_start)
  # Only HIV-negative mortality
  if(startVar=="ObservationStart" & dropHIVPos) {
     HIVPos <-  group_by(hiv, IIntID) %>% 
      summarize(HIVResult = max(HIVResult, na.rm=TRUE)) 
     sdat <- left_join(sdat, HIVPos, by="IIntID")
     sdat <- filter(sdat, HIVResult %in% c(NA, 0))
  }
  # split data
  tdat <- splitData2(sdat)
  bdat <- getBirthDate(Args$inFiles$epifile, addVars="Female") 
  tdat <- setData(tdat, bdat)
  tdat <- mutate(tdat, Days = as.numeric(obs_end-obs_start))
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
