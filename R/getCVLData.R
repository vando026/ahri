#' @title getCVLData
#' 
#' @description  Pulls in Community Viral Load data
#' 
#' @param Args requires Args (\code{\link{setArgs}}) for filepath and setting age
#'
#' @return data.frame
#'
#' @export 

getCVLData <- function(Args) {

  set.seed(20000)
  dat <- haven::read_dta(Args$inFile$pvlfile) %>% rename(IIntID=IIntId)
  dat <- mutate(dat, Comments = as.character(as_factor(dat$Comments)))

  # We dont want for year 2012
  dat <- mutate(dat, IIntID = as.integer(IIntID),
    Year = as.integer(format(SpecimenDate, "%Y")))
  dat <- filter(dat, Year != 2012)

  ind <- getIndDat()
  dat <- left_join(dat, ind, by="IIntID")
  dat <- mutate(dat, AgeAtVisit = round(as.numeric((SpecimenDate - DateOfBirth)/365.25)))
  hiv <- getHIV(Args)
  hiv <- hiv[!duplicated(hiv$IIntID), c("IIntID", "Female")]
  dat <- left_join(dat, hiv, by="IIntID")
  dat <- setAge(dat, Args) 

  # Drop all invalid results
  dat <- filter(dat, grepl("Valid|Below", Comments))
  dat <- filter(dat, ViralLoad<2e7 | is.na(ViralLoad))

  # Now replace undetectable with 0--1550
  Random = round(runif(nrow(dat), 1, 1549))
  cvl <- mutate(dat, ViralLoad = ifelse(is.na(ViralLoad), Random, ViralLoad),
      log10VL = log10(ViralLoad),
      VLDetect = ifelse(ViralLoad>=1550 & !is.na(ViralLoad), 1, 0),
      VLSuppress = ifelse(ViralLoad<1550 & !is.na(ViralLoad), 1, 0),
      AgeCat = cut(AgeAtVisit, breaks=Args$AgeCat, 
        include.lowest=TRUE, labels=NULL, right=FALSE))
  cvl <- select(cvl, IIntID, matches("Year"), Female, Age=AgeAtVisit, AgeCat,
    DBSTestDate=SpecimenDate, ViralLoad, log10VL, VLDetect, VLSuppress)
  cvl$HIVResult <- 1
  cvl
}
