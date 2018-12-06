#' @title readHIVData
#' 
#' @description  Reads in AHRI data from csv file
#' 
#' @param inFiles file paths from \code{\link{setArgs}}.
#' @param dropTasP drop TasP surveillance areas from the data. 
#' 
#' @return data.frame
#'
#' @import dplyr
#' @export 

readHIVData <- function(inFiles=Args$inFiles,
  dropTasP=TRUE) {
  hiv <- readr::read_csv(inFiles$hiv_dta, 
    col_types=cols_only(
      ResidencyBSIntId="i",
      IIntId="i",
      VisitDate="D",
      HIVResult="i",
      Sex="i",
      AgeAtVisit="i"))
  hiv <- filter(hiv, Sex %in% c(1,2))
  hiv <- mutate(hiv, Female=as.integer(ifelse(Sex==2, 1, 0)))
  hiv <- rename(hiv, IIntID=IIntId, BSIntID=ResidencyBSIntId) %>% 
   select(-Sex) %>% arrange(IIntID, VisitDate)
  if(dropTasP) {
    pipdat <- readPIPData(inFiles$pipfile)
    pipdat <- select(pipdat, BSIntID, PIPSA)
    hiv <- full_join(hiv, pipdat, by="BSIntID")
    hiv <- filter(hiv, !is.na(IIntID))
    hiv <- filter(hiv, PIPSA %in% c("S", NA)) %>% select(-PIPSA)
    comment(hiv) <- "This dataset drops HIV tests from TasP areas in 2017"
    hiv
  }
  save(hiv, file=file.path(inFiles$hivfile))
  hiv
}

#' @title getHIV
#' 
#' @description  get all valid test results from HIV surveillance.
#' 
#' @param Args requires Args, see \code{\link{setArgs}}.
#'
#' @return data.frame
#'
#' @import dplyr 
#'
#' @export 

getHIV <- function(Args) {
  load(Args$inFiles$hivfile, envir=environment())
  hiv <- mutate(hiv, Year=as.integer(format(VisitDate, "%Y"))) %>%
    filter(Year %in% Args$Years)
  # Filter by age
  hiv <- setAge(hiv, Args)
  # Keep sex
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
  hiv 
}
