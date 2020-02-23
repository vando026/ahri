#' @title setFiles
#' 
#' @description  Set file paths to the default AHRI datasets, which must be placed into a
#' single folder. The default datasets are called:
#' 
#'  - \code{RD05-99 ACDIS HIV All.dta}
#' 
#'  - \code{SurveillanceEpisodesBasicAgeYrHIV.dta}
#' 
#'  - \code{RD03-99 ACDIS WGH ALL.dta}
#' 
#'  - \code{RD04-99 ACDIS MGH ALL.dta}
#' 
#'  - \code{RD01-03 ACDIS BoundedStructures.dta}
#' 
#'  - \code{RD06-99 ACDIS HSE-H All.dta}
#' 
#' You should not need to change these names. If you do, see the
#' example below on how to do this.  
#' The function also sets the names for the .Rda datasets used in other functions. 
#' You should not need to change these names.  
#'
#' !!! You must assign the \code{setFiles} function to a variable called \code{getFiles},
#' as in the example below. !!!
#'
#' @param folder The path (as a string) to  the folder of default .dta files. If
#' \code{folder=""}, then The default calls a dialogue box is called to set the folder path.
#'
#' @return function
#'
#' @export
#'
#' @examples 
#' # You must assign the setfiles function to the getFiles name
#' getFiles <- setFiles(folder="Path/to/my/datafolder")
#'
#' # Show an example of how to change the name of the HIV Surveillance dataset
#' getFiles <- setFiles(folder="Path/to/my/datafolder", hivfile="RD09-01 PIP HIV All.dta")
#'
#' # print out the file paths and names
#' getFiles() 

setFiles <- function(
  folder="",
  hivfile="RD05-99 ACDIS HIV All.dta",
  epi_dta="SurveillanceEpisodesBasicAgeYrHIV.dta",
  wghfile="RD03-99 ACDIS WGH ALL.dta",
  mghfile="RD04-99 ACDIS MGH ALL.dta",
  bsifile="RD01-03 ACDIS BoundedStructures.dta",
  hsefile="RD06-99 ACDIS HSE-H All.dta",

  hiv_rda="ACDIS_HIV_All.Rda",
  epi_rda="SurveillanceEpisodesBasicAgeYrHIV.Rda",
  wgh_rda="ACDIS_WGH_ALL.Rda",
  mgh_rda="ACDIS_MGH_ALL.Rda",
  bsm_rda="MaxBSIntID.Rda",
  bsc_rda="ACDIS_BoundedStructures.Rda") {

  # parfile="/Analytics/PartnerDat.Rdata",
  # ind_rda="/Analytics/Individuals.Rda",
  # prvfile="/Analytics/HIV_Prev_Aug21.csv",
  # pipfile="/Analytics/PIPBoundedStructures2018.dta",
  # bscfile="BoundedStructure/BSIntID_Coords.csv",
  # fem_art="/ARTemis/2017/ART_All.csv",
  # mal_art="/ARTemis/2017/ART_All.csv",
  # pvlfile="CVL_2011_2014/Community Viral Load 2011-2014.dta") 
  if (folder=="") 
    folder <- choose.dir(caption="Select a folder which contains all the AHRI .dta files") 
  flist <- as.list(environment())
  flist <- lapply(flist, function(x) file.path(folder, x))
  function() 
    return(flist[setdiff(names(flist), "folder")])
}


#' @title setRoot
#' 
#' @description Sets the root path to datasets depending on the platform.
#' 
#' @return 
#'
#' @export 
setRoot <- function() {
  if (Sys.getenv("R_PLATFORM")=="x86_64-redhat-linux-gnu") {
    root    <- file.path("/data/AlainData/AC_Data")
  } else if (Sys.getenv("R_PLATFORM")=="x86_64-pc-linux-gnu") { #docker path
    root = file.path("/home/AC_Data") 
  } else {
    root=file.path(Sys.getenv("HOME"), "Documents/AC_Data/AHRI_Data")
  }
  root
}

#' @title setHomePath
#' 
#' @description  Set paths for PC, Linux, and Docker
#' 
#' @param pc PC path
#' @param linux Path to Linux Redhat
#' @param docker Path to docker
#' 
#' @return 
#' @export 

setHomePath <- function(pc=getwd(), ssh=getwd(), docker = "/home") {
  krisp <- "x86_64-redhat-linux-gnu"
  dock <- "x86_64-pc-linux-gnu"
  if (Sys.getenv("R_PLATFORM")==krisp) {
    home <- ssh
  } else if (Sys.getenv("R_PLATFORM")==dock) {
    home <- docker
  } else {
    home <- pc
  }
  return(home)
}


