## Description: Function to make ART coverage
## Project: ahri
## Author: AV / Created: 27Oct2017 


#' @title ARTCov
#' 
#' @description  Calculate ART coverage for AHRI data
#' 
#' @param Args Requires a list with inFiles=artemis_data_path, see \code{\link{setArgs}}.
#'
#' @return data.frame
#'
#' @import foreign, dplyr, epitools
#'
#' @export
#' 
#' @examples
#' Args <- setArgs(inFiles="ARTemis.dta")
#' Cov <- ARTCov(Args)



ARTCov <- function(Args) {

  art <- tbl_df(read.dta(Args$inFiles$artemis)) %>% 
    select(IIntID=IIntId, DateOfInitiation) %>% 
    mutate(YearART = format(DateOfInitiation, "%Y"),
    MonthART = as.numeric(format(DateOfInitiation, "%m"))) %>% 
    filter(!duplicated(IIntID))

  hdat <- acdat(Args)
  hpos <- filter(hdat, HIVResult==1) %>% 
    select(IIntID, Year, Female, AgeCat, HIVResult) 

  # Merge with ART data
  adat <- left_join(hpos, art, by="IIntID")
  adat <- arrange(adat, IIntID, Year) 

  adat <- group_by(adat, IIntID) %>% 
    mutate(OnART = ifelse(Year >= YearART, 1, 0),
    OnART = ifelse(is.na(OnART), 0, OnART))

  # Ok if month of Init is after September, dont assign OnART to that year
  adat <- mutate(adat, OnART1 =
    ifelse((YearART==Year) & MonthART>=9 & !is.na(MonthART), 0, OnART))

  getCI <- function(x, Round=2) {
    pbar = mean(x)
    N = length(x)
    SE = sqrt(pbar*(1-pbar)/N)
    E =  qnorm(1-(0.05/2))*SE 
    CI = pbar + c(-E, E)  
    out <- round(c(pbar, CI)*100, Round)
    out
  }

  getEst <- function(dat, F1) {
    out <- aggregate(as.formula(F1), data=dat, 
      FUN=getCI, simplify=TRUE)
    out <- do.call(data.frame, out)
    names(out) <- c("Group", "est", "lb", "ub")
    out
  }


  # Get overall ART coverage
  AllART <- getEst(dat=adat, F1="OnART1 ~Year")
  FemART <- getEst(dat=adat[adat$Female==1, ], F1="OnART1 ~Year")
  MalART <- getEst(dat=adat[adat$Female==0, ], F1="OnART1 ~Year")
  return(list(All=AllART, Males=MalART, Females=FemART))
}

