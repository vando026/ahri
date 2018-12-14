#' @title getHIVPop
#' 
#' @description Gets the total number of HIV- and HIV+ participants.
#' 
#' @param  Args
#' 
#' @return list 
#'
#' @export 

getHIVPop <- function(Args) {
  hiv <- getHIV(Args)
  hiv <- dplyr::filter(hiv, Year %in% Args$Years)
  neg <- dplyr::filter(hiv, HIVResult==0) %>%
    group_by(Year) %>% summarize(n=n())
  pos <- dplyr::filter(hiv, HIVResult==1) %>%
    group_by(Year) %>% summarize(n=n())
  list(hiv_neg=neg, hiv_pos=pos)
}


#' @title getMissedHIV
#' 
#' @description  Calculates missed HIV test dates by year.
#' 
#' @param Args 
#' 
#' @return data.frame
#'
#' @export 

getHIVMiss <- function(Args) {
  hdat <- readr::read_csv(Args$inFiles$hivfile, 
    col_types=cols_only(
      ResidencyBSIntId="i",
      IIntId="i",
      VisitDate="D",
      HIVRefused="i",
      HIVResult="i",
      Sex="i",
      AgeAtVisit="i"))
  hdat <- dplyr::filter(hdat, Sex %in% c(1,2))
  hdat <- dplyr::mutate(hdat, Female=as.integer(ifelse(Sex==2, 1, 0)))
  hdat <- dplyr::rename(hdat, IIntID=IIntId, BSIntID=ResidencyBSIntId) %>% 
     select(-Sex) 
  hdat <- dropTasPData(hdat, Args$inFiles$pipfile)
  # Get the bounds for obs period
  hdat <- dplyr::mutate(hdat, Year=as.integer(format(VisitDate, "%Y"))) %>%
    filter(Year %in% Args$Years)
  hdat <- dplyr::arrange(hdat, IIntID, VisitDate)
  hdat <- setAge(hdat, Args)
  # Keep sex
  hdat <- dplyr::filter(hdat, Female %in% Args$FemCode)
  # Only legible for testing
  hdat <- dplyr::filter(hdat, HIVRefused %in% c(1, 2)) 

  getRefused <- function(dat) {
    out <- round(prop.table(table(dat$HIVRefused))*100, 2)
    # HIVRefused 1=yes 2=no
    cbind(Refused=out[1], NotRefused=out[2])
  }
  sdat <- split(hdat, hdat$Year)
  out <- t(sapply(sdat,  getRefused))
  colnames(out) <- c("Refused", "Not_Refused")
  
  # Get perc ever tested
  cdat <- mutate(hdat, HIVTest = as.numeric(HIVRefused==2))
  cdat <- group_by(cdat, IIntID) %>%
    mutate(EverTest = as.numeric(cumsum(HIVTest)>=1))
  out2 <- group_by(cdat, Year) %>%
    summarize(Test=sum(EverTest), Tot=n(),
    PercEverTest = round(Test/Tot * 100, 2))
  out2
  cbind(out, out2[, "PercEverTest"])
}

# hdat1 <- group_by(yydat, IIntID) %>%
#   mutate(R1=as.numeric(all(HIVRefused==1)))
# hdat1 <- group_by(hdat1, IIntID) %>%
#   summarize(TotR = n())
# # hdat1 <- filter(hdat1, TotR>=2)
# # hdat1 <- group_by(hdat1, IIntID) %>% slice(1)
# # tab <- table(hdat1$R1)
# # prop.table(tab)

