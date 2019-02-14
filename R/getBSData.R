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
#' @param inFile file path to Demography dataset.
#' 
#' @param outFile file path to write dataset.
#'
#' @return data.frame
#'
#' @export 
#'
#' @examples 
#' hiv <- getBSMax(Args$inFiles$demfile)

getBSMax <- function(
  inFile=getFiles()$demfile,
  outFile="MaxBSIntID.Rdata") {

  dem <- getEpisodes() %>% 
    select(BSIntID, IIntID, Year, ExpDays) 
    
  # Identify max expdays per episode
  maxBS <- group_by(dem, IIntID, Year) %>% mutate(
    MaxDays = max(ExpDays, na.rm=TRUE)) %>%
    ungroup(maxBS)
  
  maxBS <- filter(maxBS, MaxDays==ExpDays) %>%
    select(IIntID, Year, BSIntID )
    
  maxBS <- group_by(maxBS, IIntID, Year) %>% 
    filter(row_number()==1) %>%
    ungroup(maxBS)

  save(maxBS, file=file.path(Sys.getenv("HOME"), 
    "Documents/AC_Data/Derived/Other", outFile))
  return(maxBS)
}

#' @title mkBSData
#' 
#' @description Makes a specific file for the IntCens project
#' 
#' @param inFile file path to import Demography dataset.
#' 
#' @param outFile file path to write dataset.
#'
#' @return data.frame


mkBSData <- function(inFiles) {

  dem <- read_tsv(inFiles$dem, 
    col_types=cols_only(
      BSIntID="i",
      IIntID="i",
      ObservationStart="T",
      ObservationEnd="T",
      ExpYear="i",
      ExpDays="i",
      Area="i",
      InMigrEx="i",
      OutMigrEx="i"))

  dem <- rename(dem, Year=ExpYear) %>%
    arrange(IIntID, ObservationStart) %>%
    mutate(InDSA = ifelse(!is.na(BSIntID), 1, 0))

  # Make resident % rule
  Total <- group_by(dem, IIntID, Year) %>% 
    summarize(Total=sum(ExpDays))
  TotalIn <- filter(dem, InDSA==1) %>%
    group_by(IIntID, Year) %>% 
    summarize(TotalIn=sum(ExpDays))
  # Now get prop of days in and out
  Rule1 <- left_join(Total, TotalIn, by=c("IIntID", "Year"))
  Rule1 <- mutate(Rule1, 
    TotalIn=ifelse(is.na(TotalIn), 0, TotalIn),
    Prop=TotalIn/Total)
  Rule1 <- filter(Rule1, Prop>=Args$ResRule) %>% 
    select(IIntID, Year, Prop)
  
  # Get Area of BS
  Area <- select(dem, BSIntID, Area) %>%
    filter(!is.na(BSIntID)) %>%
    filter(!duplicated(BSIntID))  

  # Count external migr events
  Migr <- group_by(dem, IIntID) %>% 
    summarize(In=sum(InMigrEx),
    Out=sum(OutMigrEx)) %>% 
    mutate(MigrCount=In+Out) %>% 
    select(IIntID, MigrCount)

  # Count time outside
  TimeOut <- filter(dem, InDSA==0) %>% 
    group_by(IIntID, Year) %>% 
    summarize(DaysOut=sum(ExpDays))

  bsmax <- read_csv(inFiles$bsmfile) %>% 
    rename(BSIntID=MaxBSIntID)

  # Now bring all data together
  # dem1 <- filter(dat, IIntID %in% Rule1$IIntID)
  dem1 <- left_join(dem1, bsmax, by=c("IIntID", "Year"))
  dem1 <- arrange(dem1, IIntID, Year) %>% 
    group_by(IIntID) %>% mutate(
      BSIntID = zoo::na.locf(BSIntID, na.rm=FALSE),
      BSIntID = zoo::na.locf(BSIntID, na.rm=FALSE, fromLast=TRUE))
  dem1 <- filter(dem1, !is.na(BSIntID)) #rm miss BS after CF
  dem1 <- left_join(dem1, Area, by="BSIntID") 
  dem1 <- left_join(dem1, Migr, by="IIntID")
  dem1 <- left_join(dem1, TimeOut, by=c("IIntID", "Year"))
  dem1 <- mutate(dem1, 
    DaysOut=ifelse(is.na(DaysOut), 0, DaysOut),
    TimeOut=round((DaysOut/365.25)*100,2),
    DayYear=365.25)
  dem1 <- group_by(dem1, IIntID) %>%
    mutate(CumDays=cumsum(DaysOut), 
      CumDayYear=cumsum(DayYear),
      CumTimeOut=round((CumDays/CumDayYear)*100, 2))
  dem1 <- select(dem1, -c(CumDays, DaysOut, DayYear, CumDayYear)) %>%
    ungroup(dem1)

  bdat <- mutate(dem1, 
    Rural=ifelse(Area==0, 1, 0),
    Peri=ifelse(Area==1, 1, 0),
    Urban=ifelse(Area==2, 1, 0)) 
    
  bdat
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

getBSCord <- function(inFile=Args$inFile$bscfile) {
  dat <-  read_csv(inFile)
  mutate(dat, BSIntID=as.integer(BSIntID))
}

#' @title addBSVars
#' 
#' @description Add variables from Bounded Structures to existing dataset.
#' 
#' @param dat An existing dataset.
#' @param Vars Select variables.
#' 
#' @return 
#'
#' @export 

addBSVars <- function(dat, Vars="IsUrbanOrRural") {
  load(getFiles()$bsmfile)
  dat <- left_join(dat, maxBS, by=c("IIntID", "Year"))
  bdat <- readBSData()
  bdat <- select(bdat, BSIntID, contains(Vars))
  dat <- left_join(dat, bdat, by="BSIntID")
  dat
}

