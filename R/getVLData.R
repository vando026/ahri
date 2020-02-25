#' @title getVLData
#' 
#' @description Gets viral load data for 2011, 2013, 2014.
#' 
#' @param Args takes list from \code{\link{setArgs}}.
#'
#' @return data.frame
#'
#' @import dplyr

getVLData <- function(Args) {

  cvl_all <- tbl_df(read.dta(Args$inFiles$pvlfile))
  ind <- read_csv(Args$inFiles$indfile, 
    col_types=cols_only(
    IIntID="i",
    Sex="i",
    DateOfBirth="D"))
  ind$Female <- ifelse(ind$Sex==2, 1, 0)
  names(cvl_all)[names(cvl_all)=="IIntId"] <- "IIntID"

  # We dont want for year 2012
  cvl_all <- mutate(cvl_all, Year = as.integer(format(SpecimenDate, "%Y")))
  cvl_all <- filter(cvl_all, Year != 2012)
  cvl_all <- filter(cvl_all, !(grepl("^A test|^Excluded|^Specimen not|^Other", Comments)))

  cvl_all <- left_join(cvl_all, ind, by="IIntID")
  cvl_all <- mutate(cvl_all, Age = round(as.numeric((SpecimenDate - DateOfBirth)/365.25)))

  cvl_all <- filter(cvl_all, !(Female==0 & Age < Args$Age[["Mal"]][1]) &
    !(Female==0 & Age > Args$Age[["Mal"]][2]))
  cvl_all <- filter(cvl_all, !(Female==1 & Age < Args$Age[["Fem"]][1]) &
    !(Female==1 & Age > Args$Age[["Fem"]][2]))

  # Drop all invalid results
  cvl_valid <- filter(cvl_all, grepl("Valid|Below", Comments))
  cvl_valid <- filter(cvl_valid, ViralLoad<2e7 | is.na(ViralLoad))

  # Now replace undetectable with 0--1550
  old <- .Random.seed
  set.seed(20000)
  cvl <- mutate(cvl_valid, Random = round(runif(nrow(cvl_valid), 1, 1549)))
  cvl <- mutate(cvl, ViralLoad = ifelse(is.na(ViralLoad), Random, ViralLoad)) 

  cvl <- mutate(cvl, 
    log10VL = log10(ViralLoad),
    VLDetect = ifelse(ViralLoad>=1550 & !is.na(ViralLoad), 1, 0),
    VLSuppress = ifelse(ViralLoad<1550 & !is.na(ViralLoad), 1, 0),
    AgeCat = cut(Age, breaks=Args$AgeCat, 
      labels=NULL, right=FALSE),
    HIVResult=1)

  .Random.seed <<- old
  cvl[, c("IIntID", "Year", "Female", "Age", "AgeCat", "HIVResult",
    "ViralLoad", "log10VL", "VLDetect", "VLSuppress")]
}

#' @title getPVLData
#' 
#' @description Makes the PVL version of viral load data for 2011, 2013, 2014.
#'
#' @param cvl dataset from \code{\link{getVLData}}.
#' 
#' @param Args takes list from \code{\link{setArgs}}.
#'
#' @return data.frame
#'
#' @import dplyr

getPVLData <- function(cvl, Args) {

  hiv <- getHIV(Args)
  ind <- read_csv(Args$inFiles$indfile, 
    col_types=cols_only(IIntID="i", DateOfBirth="D"))

  # We need to ensure that the HIV- added is proprtional to HIV+ by age and sex for each year
  Vars <- c("Year", "Female", "AgeCat")
  Pr <- calcTrend(hiv, Formula="HIVResult ~ Year + Female + AgeCat", 
    calcBy=Vars, fmt=FALSE)

  MergeName <- data.frame(do.call("rbind", strsplit(rownames(Pr), "[.]")))
  colnames(MergeName) <- Vars
  Pr <- cbind(MergeName, Pr)

  # This gets cvl pos count
  Pos <- group_by(cvl, Year, Female, AgeCat) %>%
    summarize(HIVPos=sum(HIVResult))
  Pos <- merge(Pr, Pos, by=Vars) 

  # This calculates how many HIV- we need to sample
  calcNeg <- function(pos, prop)  round((pos/prop) - pos)
  SampN <- mutate(Pos, Y = calcNeg(HIVPos, crude.rate))
  SampN <- SampN[, c("Year", "Female", "AgeCat", "Y")]

  # We sample from the HIV- in surveillance
  hiv_neg0 <- filter(hiv, HIVResult==0)

  # Indiv cant be in both pos and neg datasets
  old <- .Random.seed
  set.seed(12399)
  hiv_neg <- setDiffData(hiv_neg0, cvl, SampN, Args) 

  # Get time in years
  hiv_neg <- mutate(hiv_neg,
    Year = as.numeric(as.character(Year)),
    HIVResult = 0, VLSuppress = 1,
    VLDetect = 0, log10VL = 0,
    ViralLoad = 1)

  # Ages for pvl bit trickier since we added HIV-
  hiv_neg <- left_join(hiv_neg, ind, by="IIntID")
  hiv_neg <- mutate(hiv_neg, Age = 
    Year - as.numeric(format(DateOfBirth, "%Y")))
  hiv_neg <- hiv_neg[, !(names(hiv_neg) %in% c("DateOfBirth"))]

  pvl <- rbind(cvl, hiv_neg)
  pvl <- arrange(pvl, IIntID, Year)

  # Do tests make sure HIV- not after HIV+
  pvl <- group_by(pvl, IIntID) %>% 
    mutate(Count=n(), 
    Female=as.integer(Female))
  # Must have max three measures
  stopifnot(max(pvl$Count)<=3)
  .Random.seed <<- old
  pvl
}


setDiffData <- function(hdat, cdat, SampN, Args) {
  hiv_neg1 <- data.frame(IIntID=integer(), Year=integer(),
     Female=numeric(), AgeCat=factor(), HIVResult=numeric())
  # dont include anyone from cvl in HIV negs
  hdat <- hdat[!(hdat$IIntID %in% cdat$IIntID), ]
  agelab <- attributes(hdat$AgeCat)$levels
  for (yr in c(2011, 2013, 2014)) {
    for (fem in c(0,1)) {
      for (age in agelab) {
        femlabel <- ifelse(age==1, "Females", "Males")
        hdat0 <- subset(hdat, Year==yr & AgeCat==age & Female==fem)
        cdat0 <- subset(cdat, Year==yr & AgeCat==age & Female==fem)
        Samp0 <- subset(SampN, Year==yr & AgeCat==age & Female==fem)
        hdat0 <- hdat0[!duplicated(hdat0$IIntID), ]
        nids <- length(unique(hdat0$IIntID))
        ndat <- mkDat(Samp0)
        if (nids > Samp0$Y) {
          ID <- sample(hdat0$IIntID, Samp0$Y)
          out <- cbind(IIntID=ID, ndat)
        } else {
          newIDs <- sample(4e5:9e5, Samp0$Y - nids)
          newIDs <- c(hdat0$IIntID, newIDs)
          out <- cbind(IIntID=newIDs, ndat)
        }
        if (Args$printout==TRUE) {
          cat(rep("-", 20), "\n")
          cat(paste(yr, ":  sample was", length(unique(hdat0$IIntID)), femlabel, age,  "\n"))
          cat(paste(yr, ": weight HIV-", nrow(out), femlabel, age, "\n"))
        }
        hiv_neg1 <- rbind(hiv_neg1, out)
      }
    }
  }
  return(tbl_df(hiv_neg1))
 } 

mkDat <- function(dat) {
  list2env(dat, envir=environment())
  mat <-  data.frame(matrix(nrow=Y, ncol=3))
  mat[, 1] <- Year
  mat[, 2] <- Female
  mat[, 3] <- AgeCat
  mat[, 4] <- 0
  colnames(mat) <- c("Year", "Female", "AgeCat", "HIVResult")
  mat
}
