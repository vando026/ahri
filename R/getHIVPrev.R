#' @title addHIVPrevBS
#' 
#' @description  Calculates the HIV prevalence of area surrounding BS
#' 
#' @param dat
#' 
#' @return 
#'
#' @export 

addHIVPrevBS <- function(dat, 
  Args=eval.parent(quote(Args))) {

  prev <- tbl_df(read.csv(Args$inFiles$prvfile))
  prev[] <- lapply(prev[], function(x) as.numeric(as.character(x)))

  # We need opposite sex prev else all
  Sex <- ifelse(Args$Sex=="Fem", "Males",
         ifelse(Args$Sex=="Mal", "Females", "All"))

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
  dat <- mutate(dat, HIVPrevCat = 
    cut(HIVPrev, breaks=c(0, 10, 15, 20, 100),
    include.lowest=TRUE, right=FALSE))
  dat
}

