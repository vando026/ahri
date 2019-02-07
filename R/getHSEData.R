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
  hdat <- hdat[!duplicated(hdat[, c("BSIntID", "Year")]), ]
  dat <- left_join(dat, hdat, by=c("BSIntID", "Year"))
  dat <- mutate(dat, 
    AIQ = zoo::na.locf(AIQ, na.rm=FALSE), 
    AIQ = zoo::na.locf(AIQ, na.rm=FALSE, fromLast=TRUE))
}
