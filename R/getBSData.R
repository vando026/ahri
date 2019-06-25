#' @title readBSData
#' 
#' @description  Read in Bounded Structures data.
#' 
#' @param inFile filepath to data.
#' 
#' @return data.frame
#'
#' @export 
#'
#' @examples
#' readBSData() 

readBSData <- function(inFile=getFiles()$bsifile) {
  dat <- haven::read_dta(inFile)
  dat <- mutate(dat, 
    BSIntId = as.integer(BSIntId),
    LocalArea = as.character(haven::as_factor(dat$LocalArea)),
    Isigodi = as.character(haven::as_factor(dat$Isigodi)),
    IsUrbanOrRural = as.character(haven::as_factor(dat$IsUrbanOrRural)))
  dat <- rename(dat, BSIntID=BSIntId)
  dat
}


#' @title readPIPData
#' 
#' @description  Read in PIP data to identify ACIDS from TASP area
#' 
#' @param inFile
#' 
#' @return data.frame
#'
#' @export 
readPIPData <- function(inFile=getFiles()$pipfile) {
  dat <- readxl::read_excel(inFile)
  names(dat)[names(dat)=="BSIntId"] = "BSIntID"
  dat <- mutate(dat, BSIntID=as.integer(BSIntID))
  dat
}

#' @title getBSMax
#' 
#' @description Gets the BSIntID that IIntID spent most time in a surveillance year. 
#' 
#' @param inFile file path to Episodes dataset (\code{getFiles()$epifile}).
#' @param outFile file path to write dataset.
#' @param minDays Value of 1:366 min days spent in DSA to qualify as being a resident in
#' that year. 
#' @param dropNonResident Drop all non-residents, default is TRUE.
#'
#' @return data.frame
#'
#' @export 
#'
#' @examples 
#' hiv <- getBSMax()

getBSMax <- function(
  inFile=getFiles()$epifile,
  outFile="MaxBSIntID.Rdata",
  minDays=0, dropNonResident=TRUE) {

  load(inFile)
  if (dropNonResident)
    dat <- filter(dat, Resident==1)

  # Identify max expdays per episode
  dat <- group_by(dat, IIntID, Year) %>% mutate(
    MaxDays = max(ExpDays, na.rm=TRUE))
  dat <- ungroup(dat)
  
  dat <- filter(dat, MaxDays==ExpDays)

  dat <- group_by(dat, IIntID, Year) %>% 
    filter(row_number()==1)
  dat <- ungroup(dat)

  dat <- filter(dat, MaxDays >= minDays) %>% 
    select(IIntID, Year, BSIntID )

  # save(dat, file=file.path(Sys.getenv("HOME"), 
    # "Documents/AC_Data/Derived/Other", outFile))
  return(dat)
}

#' @title addMigrVars
#' 
#' @description Gets variables on Migration.
#' 
#' @param inFile File path to import Demography dataset.
#' 
#' @return data.frame
#' @export

addMigrVars <- function(dat, dem=NULL, inFile=getFiles()$demfile) {

  if (is.null(dem))
    dem <- readEpisodes(Vars="^Resident$|Migration")

  # Make resident % rule
  # Total <- group_by(dem, IIntID, Year) %>% 
  #   summarize(Total=sum(ExpDays))
  # TotalIn <- filter(dem, Resident==1) %>%
  #   group_by(IIntID, Year) %>% 
  #   summarize(TotalIn=sum(ExpDays))
  # # Now get prop of days in and out
  # TimeIn <- left_join(Total, TotalIn, by=c("IIntID", "Year"))
  # TimeIn <- mutate(TimeIn, 
  #   TotalIn=ifelse(is.na(TotalIn), 0, TotalIn),
  #   TimeInProp=TotalIn/Total) %>%
  #   select(IIntID, Year, TimeInProp)
  # Count time outside
  TimeOut <- filter(dem, Resident==0) %>% 
    group_by(IIntID, Year) %>% 
    summarize(DaysOut=sum(ExpDays))

  # Count external migr events
  Migr <- group_by(dem, IIntID) %>% summarize(
    In=sum(InMigration), Out=sum(OutMigration)) %>%
    ungroup()
  Migr <- mutate(Migr, MigrCount=In+Out) %>% 
    select(IIntID, MigrCount)

  # Now bring all data together
  # dat <- left_join(dat, TimeIn, by=c("IIntID", "Year"))
  dat <- left_join(dat, TimeOut, by=c("IIntID", "Year"))
  dat <- left_join(dat, Migr, by="IIntID")
  dat <- mutate(dat, 
    DaysOut=ifelse(is.na(DaysOut), 0, DaysOut),
    TimeOut=round((DaysOut/365.25)*100,2),
    DayYear=365.25)
  dat <- group_by(dat, IIntID) %>%
    mutate(CumDays=cumsum(DaysOut), 
      CumDayYear=cumsum(DayYear),
      CumTimeOut=round((CumDays/CumDayYear)*100, 2))
  dat <- select(dat, -c(CumDays, DaysOut, DayYear, TimeOut, CumDayYear))
  dat <- arrange(dat, IIntID, Year)
  dat
}

#' @title getBSCord
#' 
#' @description  get the GPS coordinates for BSIntID 
#' 
#' @param inFile path to the BSIntID coordinates csv file
#' 
#' @return data.frame
#'
#' @export

getBSCord <- function(inFile=getFiles()$bscfile) {
  dat <-  read_csv(inFile)
  mutate(dat, BSIntID=as.integer(BSIntID))
}

#' @title addBSVars
#' 
#' @description Add variables from Bounded Structures to existing dataset.
#' 
#' @param dat An existing dataset.
#' @param Vars Select variables.
#' @param dropNonResident Drop all non residents.
#' @param dropMissBS Drop any missing BS.
#' 
#' @return 
#'
#' @export 

addBSVars <- function(dat, Vars="IsUrbanOrRural", 
  dropNonResident=TRUE, dropMissBS=TRUE) {
  maxBS <- getBSMax(dropNonResident=dropNonResident)
  dat <- left_join(dat, maxBS, by=c("IIntID", "Year"))
  bdat <- readBSData()
  bdat <- select(bdat, BSIntID, matches(Vars))
  dat <- left_join(dat, bdat, by="BSIntID")
  dat <- rename(dat, Area=IsUrbanOrRural)
  dat$Area[dat$Area=="Peri-Urban"] <- "PeriUrban"
  dat$Area[is.na(dat$Area)] <- 
    sample(sort(unique(dat$Area)),
    size=sum(is.na(dat$Area)),
    replace=TRUE,
    prob=prop.table(table(dat$Area)))
  if (dropMissBS)
    dat <- filter(dat, !is.na(BSIntID))
  dat
}

#' @title readHSEData
#' 
#' @description Read HSE data.
#' 
#' @param inFile. File path from \code{\link{getFiles}}.
#' 
#' @return 
#'
#' @export 
readHSEData <- function(inFile=getFiles()$hsefile) {
  dat <- haven::read_dta(inFile)
  dat <- rename(dat, Year=ExpYear, AIQ=AssetIndexQuintile) %>% 
    arrange(BSIntID, Year) 
  dat[] <- lapply(dat[], as.integer)
  dat
}

#' @title addAIQVar
#' 
#' @description Add variables from Bounded Structures to existing dataset.
#' 
#' @param dat An existing dataset.
#' @param Vars Select variables.
#' 
#' @return 
#'
#' @export 

addAIQVar <- function(dat) {
  hdat <- readHSEData() %>% select(BSIntID, Year, AIQ)
  hdat <- distinct(hdat, BSIntID, Year, .keep_all=TRUE) 
  dat <- left_join(dat, hdat, by=c("BSIntID", "Year"))
  dat <- mutate(dat, 
    AIQ = zoo::na.locf(AIQ, na.rm=FALSE), 
    AIQ = zoo::na.locf(AIQ, na.rm=FALSE, fromLast=TRUE))
  dat <- mutate(dat, AIQ3 =
    ifelse(AIQ %in% c(1:2), "lower",
    ifelse(AIQ %in% c(4:5), "upper", "middle")))
  dat <- mutate(dat, AIQ3 = as.factor(AIQ3))
  dat
}
