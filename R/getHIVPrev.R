#' @title addHIVPrevBS
#' 
#' @description  Calculates the HIV prevalence of area surrounding BS
#' 
#' @param dat A dataset to add HIV prevalence variables to. 
#' @param Args requires Args, see \code{\link{setArgs}}
#' @param oppSex Make opposite-sex HIV prevalence. Default is FALSE.
#' 
#' @return 
#'
#' @export 

addHIVPrevBS <- function(dat, 
  Args=eval.parent(quote(Args)),
  oppSex=FALSE) {

  prev <- tbl_df(read.csv(Args$inFiles$prvfile))
  prev[] <- lapply(prev[], function(x) as.numeric(as.character(x)))

  # We need opposite sex prev else all
  if (oppSex) {
    Sex <- ifelse(Args$Sex=="Fem", "Males",
           ifelse(Args$Sex=="Mal", "Females", "All"))
  } else {
    Sex <- Args$Sex 
  }

  # Reshape to long
  prev <- select(prev, BSIntID, starts_with(Sex))
  long <- tidyr::gather(prev, Year, HIVPrev, starts_with(Sex)) 
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

