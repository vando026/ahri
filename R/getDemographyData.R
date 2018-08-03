#' @title getDemographyData
#' 
#' @description  get AHRI demography dataset
#' 
#' @param inFile path to data which is typically set using \code{\link{inFiles}}
#'
#' @return data.frame
#'
#' @export
#' 
#' @importFrom readr read_tsv cols_only
#'
#' @importFrom dplyr tbl_df

getDemographyData <- function(
  inFile=Args$inFiles$demfile) {
  dem <- read_tsv(inFile, 
    col_types=cols_only(
      BSIntID="i",
      IIntID="i",
      ObservationStart="D",
      ObservationEnd="D",
      Sex="i",
      ExpYear="i",
      ExpDays="i",
      Area="i",
      InMigrEx="i",
      OutMigrEx="i",
      HIVPositive="i",
      Died="i"))
  names(dem)[names(dem)=="ExpYear"] <- "Year"
  dem$InDSA <- ifelse(!is.na(dem$BSIntID), 1, 0)
  dem <- dem[dem$Sex %in% c(1, 2), ]
  dem$Female <- ifelse(dem$Sex==2, 1, 0)
  dem <- dem[with(dem, order(IIntID, ObservationStart)), 
    !(names(dem) %in% "Sex")]
  tbl_df(dem)
}


