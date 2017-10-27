## Description: Set File paths for AHRI
## Project: ahri
## Author: AV / Created: 27Oct2017 

#' @title getFiles
#' 
#' @description  get/set file paths to AHRI data 
#' 
#' @param root file path to derived files
#'
#' @return list
#'
#' @examples 
#' inFiles <- getFiles()

getFiles <- function(root=Sys.getenv("USERPROFILE")) {
  file_ls <- list(
    demfile="Demography/2016/RD02-01_ACDIS_Demography.csv",
    indfile="Individuals/2016/RD01-01_ACDIS_Individuals.csv",
    hivfile="HIVSurveillance/2016/RD05-99_ACDIS_HIV_All.csv",
    hsefile="HSE/HSE2009_2012.dta",
    wghfile="WGH_MGH/2016/RD03-99_ACDIS_WGH_ALL.csv",
    mghfile="WGH_MGH/2016/RD04-99_ACDIS_MGH_ALL.csv",
    artemis="ARTemis/ARTemisAll2013A.dta",
    bsmfile="Other/MaxBSIntID.csv",
    bsafile="BS_Area.Rdata",
    parfile="PartnerDat.Rdata",
    prvfile="HIV_Prev_Aug21.csv",
    artfile="Results_ART_Aug18.xlsx"
  )
  file_ls <- lapply(file_ls, function(x) file.path(root, x))
  return(file_ls)
}







