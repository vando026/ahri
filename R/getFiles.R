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
#' @param hivfile Default is RD05-99 ACDIS HIV All.dta
#' @param epifile Default is SurveillanceEpisodesBasicAgeYrHIV.dta
#' @param wghfile Default is RD03-99 ACDIS WGH ALL.dta
#' @param mghfile Default is RD04-99 ACDIS MGH ALL.dta
#' @param bsifile Default is RD01-03 ACDIS BoundedStructures.dta
#' @param hsefile Default is RD06-99 ACDIS HSE-H All.dta
#' @param hiv_rda Default is ACDIS_HIV_All.Rda
#' @param epi_rda Default is SurveillanceEpisodesBasicAgeYrHIV.Rda
#' @param wgh_rda Default is ACDIS_WGH_ALL.Rda
#' @param mgh_rda Default is ACDIS_MGH_ALL.Rda
#' @param bsc_rda Default is ACDIS_BoundedStructures.Rda
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
  folder="C:/Users/alainv/AHRI_Data",
  hivfile="RD05-99 ACDIS HIV All.dta",
  epifile="SurveillanceEpisodesBasicAgeYrHIV.dta",
  wghfile="RD03-99 ACDIS WGH ALL.dta",
  mghfile="RD04-99 ACDIS MGH ALL.dta",
  bsifile="RD01-03 ACDIS BoundedStructures.dta",
  hsefile="RD06-99 ACDIS HSE-H All.dta",

  hiv_rda="ACDIS_HIV_All.Rda",
  epi_rda="SurveillanceEpisodesBasicAgeYrHIV.Rda",
  wgh_rda="ACDIS_WGH_ALL.Rda",
  mgh_rda="ACDIS_MGH_ALL.Rda",
  bsc_rda="ACDIS_BoundedStructures.Rda") {

  if (folder=="") 
    folder <- choose.dir(caption="Select a folder which contains all the AHRI .dta files") 
  flist <- as.list(environment())
  flist <- lapply(flist, function(x) file.path(folder, x))
  function() 
    return(flist[setdiff(names(flist), "folder")])
}


