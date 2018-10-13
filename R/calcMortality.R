#' @title getMortalityData
#' 
#' @description  gets mortality data.
#' 
#' @param Args see \code{\link{setArgs}}.
#' @param startDate string variable at which person-time starts. Use early_pos
#' for HIV-positive all-cause mortality, otherwise obs_start (earliest HIV-neg) for all-cause
#' mortality.
#' 
#' @return data.frame
#'
#' @import dplyr
#' @export 


getMortalityData <- function(Args, startVar="HIVPositive") {
  # Get start date
  hiv <- getHIV(Args) %>% select(
    IIntID, obs_start = matches("HIVPositive")) %>%
    filter(is.finite(obs_start))
  startdat <- group_by(hiv, IIntID) %>%
    mutate(obs_start = min(obs_start, na.rm=TRUE)) %>%
    distinct(IIntID, .keep_all=TRUE)
  # Get all death dates
  dodat <- getEpisodes() %>% 
    select(IIntID, DoD) %>% 
    filter(is.finite(DoD)) %>% 
    distinct(IIntID, .keep_all=TRUE)
  dodat <- filter(dodat, 
    as.numeric(format(DoD, "%Y")) %in% Args$Years)
  # Get last observation date
  enddat <- setEpisodes(Args) %>% 
    group_by(IIntID) %>% 
    summarize(end_date=max(ObservationEnd, na.rm=TRUE))
  # Make obs_end as death or last obs date
  enddat <- left_join(enddat, dodat, by="IIntID")
  enddat <- mutate(enddat, 
    obs_end = as.Date(
      ifelse(is.finite(DoD), DoD, end_date), 
      origin="1970-01-01"),
    event = as.numeric(is.finite(DoD)))
  # merge obs_start and obs_end dates 
  sdat <- left_join(startdat, enddat, by="IIntID") %>% 
    select(IIntID, obs_start, obs_end, event)
  sdat <- filter(sdat, obs_end>obs_start)
  # split data
  tdat <- splitData2(sdat, years=Args$Years)
  bdat <- getBirthDate(addVars="Female") 
  tdat <- getAgeData(tdat, bdat, Args)
  tdat <- mutate(tdat, Days = as.numeric(obs_end-obs_start))
  tdat
} 

#' @title calcMortality
#' 
#' @description Calculates mortality rates from \code{\link{getMortalityData}}.
#' 
#' @param  dat a dataset
#' 
#' @return table
#'
#' @import epitools
#' @export 

calcMortality <- function(dat) {
  dat <- group_by(dat, Year) %>% 
    summarize(Count=sum(event), PYears=sum(Days)/365.25) %>%
    mutate(rate = Count/PYears * 100)
  out <- epitools::pois.exact(dat$Count, dat$PYears)
  out <- select(out, Deaths=x, PTime=pt, Rate=rate, LB=lower, UB=upper)
  out[c(3:5)] <- lapply(out[c(3:5)], "*", 100)
  out
}




