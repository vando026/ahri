#' @title getDemographyData
#' 
#' @description  get AHRI demography dataset (old dataset). 
#' 
#' @param inFile path to data, see \code{\link{getFiles}}.
#'
#' @return data.frame
#'
#' @export

getDemographyData <- function(
  inFile=getFiles()$demfile) {
  dem <- readr::read_tsv(inFile, 
    col_types=readr::cols_only(
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


#' @title setDemographyData
#' 
#' @description  Sets the data according to Args.
#' 
#' @param Args requires Args, see \code{\link{setArgs}}
#'
#' @return data.frame
#'
#' @export 
#' @examples
#' setDemographyData(Args)

setDemographyData <- function(Args) {
  dat <- getDemographyData(Args$inFiles$demfile)
  setData(dat)
}

#' @title getAgeYear
#' 
#' @description  Calculate Age distribution by year of resident.
#' 
#' @param Args.
#' 
#' @return 
#'
#' @export 
getAgeYear <- function(Args) {
  mn_age <- getAgeWeights(Args)
  data.frame(Age = mn_age, tscale=1,
    Year = factor(Args$Year, levels = Args$Year, 
    labels = levels(as.factor(Args$Year))))
}
