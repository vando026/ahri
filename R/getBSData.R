#' @title getBSData
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
#' getBSData() 
getBSData <- function(inFile=NULL) {
  if (is.null(inFile)) {
    check_getFiles()
    inFile=getFiles()$bsifile
  }
  dat <- haven::read_dta(inFile) %>% 
    rename(BSIntID=BSIntId)
  dat <- mutate(dat, BSIntID = as.integer(.data$BSIntID))
  return(dat)
}

#' @title dropTasPData
#' 
#' @description  Function to drop obervations from TasP areas.
#' 
#' @param dat A dataset, which will be merged with the Bounded Structures
#' dataset, to determine if observations come from the TasP (northern) areas. 
#' If an observation cannot be linked to an area, it is kept.
#' 
#' @return data.frame
#'
#' @export 

dropTasPData <- function(dat) {
  bsdat <- getBSData() %>% select(BSIntID, PIPSA)
  bsdat$PIPSA <- as.character(haven::as_factor(bsdat$PIPSA))
  dat <- left_join(dat, bsdat, by="BSIntID")
  dat <- filter(dat, PIPSA %in% c("Southern PIPSA", NA)) 
  return(dat)
}

#' @title getBSMax
#' 
#' @description Gets the BSIntID that IIntID spent most time in a surveillance year. 
#' 
#' @param inFile file path to Episodes dataset (\code{getFiles()$epi_rda}).
#' @param minDays Value of 1:366 min days spent in DSA to qualify as being a resident in
#' that year. 
#'
#' @return data.frame
#'
#' @export 
#'
#' @examples 
#' getBSMax()

getBSMax <- function(
  inFile=getFiles()$epi_rda,
  minDays=0) {

  dat <- readRDS(inFile)
  dat <- filter(dat, .data$Resident==1)

  # Identify max expdays per episode
  dat <- group_by(dat, .data$IIntID, .data$Year) %>% mutate(
    MaxDays = max(.data$ExpDays, na.rm=TRUE))
  dat <- ungroup(dat)
  
  dat <- filter(dat, MaxDays==.data$ExpDays)

  dat <- group_by(dat, .data$IIntID, .data$Year) %>% 
    filter(row_number()==1)
  dat <- ungroup(dat)

  maxBS <- filter(dat, MaxDays >= .data$minDays) %>% 
    select(.data$IIntID, .data$Year, .data$BSIntID )

  maxBS
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
#' @keywords internal
#' @export 

addMigrVars <- function(dat, dem=NULL, keepYear=Args$Years) {
  if (is.null(dem)) dem <- getEpisodes()
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

#' @title addBSVars
#' 
#' @description Add variables from Bounded Structures to existing dataset.
#' 
#' @param dat An existing dataset.
#' @param Vars Select variables.
#' @param dropMissBS Drop any missing BS.
#' 
#' @return data.frame
#' @keywords internal
#' @export 

addBSVars <- function(dat, Vars="Area", 
  dropMissBS=TRUE) {
  maxBS <- readRDS(getFiles()$bsm_rda)
  dat <- left_join(dat, maxBS, by=c("IIntID", "Year"))
  bdat <- readRDS(getFiles()$bsc_rda)
  bdat <- select(bdat, BSIntID, matches(Vars))
  dat <- left_join(dat, bdat, by="BSIntID")
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
#' @param inFile File path from \code{\link{setFiles}}.
#' 
#' @return data.frame
#'
#' @export 
readHSEData <- function(inFile=getFiles()$hsefile) {
  dat <- haven::read_dta(inFile)
  dat <- select(dat, BSIntID, Year=ExpYear, AIQ=AssetIndexQuintile) %>% 
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
#' @keywords internal

##cut(ee1$ModerntAssetIdx, breaks=quantile(ee1$ModerntAssetIdx, probs = seq(0, 1, 1/5), na.rm=TRUE), labels=FALSE, include.lowest=TRUE, right=FALSE)

addAIQVar <- function(dat) {
  hdat <- getEpisodes() 
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
#' @return data.frame
#' @keywords internal
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

