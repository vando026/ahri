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
  root=file.path(Sys.getenv("HOME"), "Documents/AC_Data/Derived"),
  demfile="Demography/2016/RD02-01_ACDIS_Demography.csv",
  indfile="Individuals/2016/RD01-01_ACDIS_Individuals.csv",
  hivfile="HIVSurveillance/2016/RD05-99_ACDIS_HIV_All.csv",
  hsefile="HSE/HSE2009_2012.dta",
  wghfile="WGH_MGH/2016/RD03-99_ACDIS_WGH_ALL.csv",
  mghfile="WGH_MGH/2016/RD04-99_ACDIS_MGH_ALL.csv",
  artemis="ARTemis/ARTemisAll2013A.dta",
  bsmfile="Other/MaxBSIntID.csv",
  bsafile="Other/BS_Area.Rdata",
  parfile="Other/PartnerDat.Rdata",
  prvfile="Other/HIV_Prev_Aug21.csv",
  pvlfile="Source/CVL_2011_2014/Community Viral Load 2011-2014.dta") {
  flist <- as.list(environment())
  flist <- lapply(flist, function(x) file.path(root, x))
  return(flist[setdiff(names(flist), "root")])
}







