#' @title getHIV
#' 
#' @description  get all valid HIV tests from surveillance
#' 
#' @param Args requires Args, see \code{\link{setArgs}}.
#'
#' @return data.frame
#'
#' @import dplyr, readr
#'
#' @export
#' 
#' @examples
#' Args <- setArgs(inFiles="ARTemis.dta")
#' HIV <- getHIV(Args)

getHIV <- function(Args) {

  hiv <- read_csv(Args$inFiles$hivfile, 
    col_types=cols_only(
      ResidencyBSIntId="i",
      IIntId="i",
      VisitDate="D",
      HIVResult="i",
      Sex="i",
      AgeAtVisit="i"))

  hiv <- filter(hiv, Sex %in% c(1,2))
  hiv <- mutate(hiv, Female=ifelse(Sex==2, 1, 0))
  hiv <- rename(hiv, IIntID=IIntId, BSIntID=ResidencyBSIntId) %>% 
   select(-Sex) %>% arrange(IIntID, VisitDate)

  # Get the bounds for obs period
  hiv <- mutate(hiv, Year=as.integer(format(VisitDate, "%Y"))) %>%
    filter(Year %in% Args$Years)

  hiv <- filter(hiv, !(Female==0 & AgeAtVisit < Args$Age[["Mal"]][1]) &
    !(Female==0 & AgeAtVisit > Args$Age[["Mal"]][2]))
  hiv <- filter(hiv, !(Female==1 & AgeAtVisit < Args$Age[["Fem"]][1]) &
    !(Female==1 & AgeAtVisit > Args$Age[["Fem"]][2]))

  hiv <- filter(hiv, Female %in% Args$FemCode)

  # Only deal with valid test results
  hiv <- filter(hiv, HIVResult %in% c(0,1))
  hiv <- mutate(hiv, 
    HIVNegative = ifelse(HIVResult==0, VisitDate, NA), 
    HIVPositive = ifelse(HIVResult==1, VisitDate, NA))

  hiv <- mutate(hiv, AgeCat = cut(AgeAtVisit, breaks=Args$AgeCat,
    include.lowest=FALSE, labels=NULL, right=FALSE))

  Vars <- c("HIVNegative", "HIVPositive")
  hiv[Vars] <- lapply(hiv[Vars], as.Date, origin="1970-01-01")
  if (Args$printout==TRUE) print(table(hiv$Female))
  hiv 
}
