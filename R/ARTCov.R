#' @title ARTCov
#' 
#' @description  Calculate ART coverage for AHRI data.
#' 
#' @param Args Requires a list with inFiles=artemis_data_path, see \code{\link{setArgs}}
#'
#' @param cutoff value between 1 and 12, if ART initiation is after this value then no ART
#' usage for that entire year
#' 
#' @param calcBy string variable to calc the estimates by
#' 
#' @param fmt format to percentage and round to two decimal places
#'
#' @return data.frame
#'
#' @import dplyr
#' 
#' @importFrom epitools binom.exact
#' 
#' @export


ARTCov <- function(
  Args, calcBy="Year", cutoff=9, fmt=FALSE) {

  art <- tbl_df(foreign::read.dta(Args$inFiles$artemis)) %>% 
    select(IIntID=IIntId, DateOfInitiation)
  art <- filter(art, !duplicated(IIntID))
  art <- filter(art, !is.na(DateOfInitiation))
  art <- mutate(art, 
    YearART = as.numeric(format(DateOfInitiation, "%Y")),
    MonthART = as.numeric(format(DateOfInitiation, "%m")))

  hdat <- getHIV(Args)
  hpos <- filter(hdat, HIVResult==1) %>% 
    select(IIntID, Year, Female, AgeCat, HIVResult) 

  # Merge with ART data
  adat <- left_join(hpos, art, by="IIntID")
  adat <- arrange(adat, IIntID, Year) 

  adat <- group_by(adat, IIntID) %>% 
    mutate(OnARTYear = ifelse(Year >= YearART, 1, 0))
  adat <- mutate(adat, OnARTYear = ifelse(is.na(OnARTYear), 0, OnARTYear))

  # Ok if month of Init is after September, dont assign OnART to that year
  adat <- mutate(adat, OnART =
    ifelse((YearART==Year) & MonthART>=9 & !is.na(MonthART), 0, OnARTYear))

  sdat <- group_by(adat, Year) %>% 
    summarize(ARTCount=sum(OnART),
    N=(sum(HIVResult)))
  sdat <- split(sdat, sdat[calcBy])
  sdat <- lapply(sdat, function(x) binom.exact(x$ARTCount, x$N))
  sdat <- do.call('rbind', sdat)
  if (fmt==TRUE) 
    sdat[c(3:5)] <- lapply(sdat[c(3:5)], function(x) round(x*100,2))
  sdat     
}

