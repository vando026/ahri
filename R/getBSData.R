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
  dat <- haven::read_dta(inFile) %>% 
    select(BSIntID=BSIntId, LocalArea, IsUrbanOrRural)
  mutate(dat, 
    BSIntID = as.integer(BSIntID),
    LocalArea = as.character(haven::as_factor(dat$LocalArea)),
    IsUrbanOrRural = as.character(haven::as_factor(dat$IsUrbanOrRural)))
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
  minDays=0) {

  load(inFile)
  dat <- filter(dat, Resident==1)

  # Identify max expdays per episode
  dat <- group_by(dat, IIntID, Year) %>% mutate(
    MaxDays = max(ExpDays, na.rm=TRUE))
  dat <- ungroup(dat)
  
  dat <- filter(dat, MaxDays==ExpDays)

  dat <- group_by(dat, IIntID, Year) %>% 
    filter(row_number()==1)
  dat <- ungroup(dat)

  maxBS <- filter(dat, MaxDays >= minDays) %>% 
    select(IIntID, Year, BSIntID )

  save(maxBS, file=file.path(Sys.getenv("HOME"), 
    "Documents/AC_Data/Derived/Other", outFile))
  return(maxBS)
}

#' @title addMigrVars
#' 
#' @description Gets variables on Migration.
#' 
#' @param dat Dataset to merge variables to. 
#' @param dem Dataset from \code{\link{readEpisodes}}.
#' @param keepYear Years to keep. 
#' 
#' @return data.frame
#' @export

addMigrVars <- function(dat, dem=NULL, keepYear=Args$Years) {

  if (is.null(dem))
    dem <- readEpisodes(Vars="^Resident$|Migration")

  adat <- distinct(dem, IIntID, Year)
  mdat <- filter(dem, Resident==1)
  mdat <- group_by(mdat, IIntID, Year) %>% 
    summarize(DaysIn=sum(ExpDays))
  adat <- left_join(adat, mdat, by=c("IIntID", "Year"))
  adat <- filter(adat, Year %in% keepYear)
  adat$DaysIn[is.na(adat$DaysIn)] <- 0
  adat <- mutate(adat, 
    DaysOut = 366 - DaysIn, DayFull = 366)
  adat <- group_by(adat, IIntID) %>% 
    mutate(CumDaysOut = cumsum(DaysOut),
    CumDays = cumsum(DayFull),
    CumTimeOut = round(CumDaysOut/CumDays, 2))
  adat <- ungroup(adat) %>%
    select(IIntID, Year, CumTimeOut)

  # Count external migr events
  Migr <- group_by(dem, IIntID, Year) %>% summarize(
    InMigr=sum(InMigration), OutMigr=sum(OutMigration)) %>%
    ungroup()
  Migr <- mutate(Migr, MigrCount=InMigr+OutMigr) %>% 
    select(IIntID, Year, InMigr, OutMigr, MigrCount)

  # Now bring all data together
  dat <- left_join(dat, adat, by=c("IIntID", "Year"))
  dat <- left_join(dat, Migr, by=c("IIntID", "Year"))
  dat <- arrange(dat, IIntID, Year)
  dat <- mutate(dat, 
    MigrCount = zoo::na.locf(MigrCount, na.rm=FALSE), 
    MigrCount = zoo::na.locf(MigrCount, na.rm=FALSE, fromLast=TRUE),
    CumTimeOut = zoo::na.locf(CumTimeOut, na.rm=FALSE), 
    CumTimeOut = zoo::na.locf(CumTimeOut, na.rm=FALSE, fromLast=TRUE),
    CumTimeOut = round(CumTimeOut*100))
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
#' @param dropMissBS Drop any missing BS.
#' 
#' @return 
#'
#' @export 

addBSVars <- function(dat, Vars="IsUrbanOrRural", 
  dropMissBS=TRUE) {
  load(getFiles()$bsmfile)
  dat <- left_join(dat, maxBS, by=c("IIntID", "Year"))
  bdat <- readBSData()
  bdat <- select(bdat, BSIntID, matches(Vars))
  dat <- left_join(dat, bdat, by="BSIntID")
  dat <- rename(dat, Area=IsUrbanOrRural)
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

#' @title addHIVPrevBS
#' 
#' @description  Calculates the HIV prevalence of area surrounding BS
#' 
#' @param dat A dataset to add HIV prevalence variables to. 
#' @param Args requires Args, see \code{\link{setArgs}}
#' @param Type Males, Females or All for ART coverage. 
#' 
#' @return 
#'
#' @export 

addHIVPrevBS <- function(dat, Args, Type="All") {

  prev <- tbl_df(read.csv(Args$inFiles$prvfile))
  prev[] <- lapply(prev[], function(x) as.numeric(as.character(x)))

  # Reshape to long
  prev <- select(prev, BSIntID, starts_with(Type))
  long <- tidyr::gather(prev, Year, HIVPrev, starts_with(Type)) 
  long <- suppressWarnings(mutate(long, 
    Year=as.integer(gsub("[^[:digit:]]", "", Year)),
    HIVPrev = as.numeric(HIVPrev)*100))

  dat <- left_join(dat, long, by=c("BSIntID", "Year"))
  dat <- arrange(dat, BSIntID, Year) 
  dat <- group_by(dat, BSIntID) %>% mutate(
      HIVPrev=zoo::na.locf(HIVPrev, na.rm=FALSE),
      HIVPrev=zoo::na.locf(HIVPrev, na.rm=FALSE, fromLast=TRUE))
  dat$HIVPrev[is.na(dat$HIVPrev)]  <- 
    runif(sum(is.na(dat$HIVPrev)), 0, 50)
  dat
}

