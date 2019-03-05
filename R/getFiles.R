#' @title getFiles
#' 
#' @description  get/set file paths to AHRI data 
#' 
#' @param root file path to derived files
#'
#' @return list
#'
#' @export
#'
#' @examples 
#' inFiles <- getFiles()

getFiles <- function(
  root=setRoot(),
  demfile="Derived/Demography/2016/RD02-01_ACDIS_Demography.csv",
  indfile="Derived/Individuals/2016/RD01-01_ACDIS_Individuals.csv",
  hivfile="Derived/HIVSurveillance/2017/RD05-99_ACDIS_HIV_All.csv",
  hsefile="Derived/HSE/HSE2009_2012.dta",
  wghfile="Derived/WGH_MGH/2018/RD03-99_ACDIS_WGH_ALL.dta",
  mghfile="Derived/WGH_MGH/2018/RD04-99_ACDIS_MGH_ALL.dta",
  bsifile="Derived/BoundedStructure/2017/RD01-03_ACDIS_BoundedStructures.dta",
  bsmfile="Derived/Other/MaxBSIntID.Rdata",
  epi_dta="Source/Episodes/2017/SurveillanceEpisodesBasicAgeYrHIV.dta",
  epifile="Derived/Episodes/2017/SurveillanceEpisodes.Rdata",
  bsafile="Derived/Other/BS_Area.Rdata",
  parfile="Derived/Other/PartnerDat.Rdata",
  prvfile="Derived/Other/HIV_Prev_Aug21.csv",
  pipfile="Derived/BoundedStructure/PIPBoundedStructures.xlsx",
  bscfile="Derived/BoundedStructure/BSIntID_Coords.csv",
  pvlfile="Source/CVL_2011_2014/Community Viral Load 2011-2014.dta") {
  flist <- as.list(environment())
  flist <- lapply(flist, function(x) file.path(root, x))
  return(flist[setdiff(names(flist), "root")])
}


#' @title setRoot
#' 
#' @description Sets the root path to datasets depending on the platform.
#' 
#' @return 
#'
#' @export 

setRoot <- function() {
  if (Sys.getenv("R_PLATFORM")==  "x86_64-redhat-linux-gnu") {
    root    <- file.path("/data/AlainData/AC_Data")
  } else {
    root=file.path(Sys.getenv("HOME"), "Documents/AC_Data")
  }
  root
}

