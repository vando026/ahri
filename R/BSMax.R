## Description: Get the BS that ID spent most time in for the year
## Project: All
## Author: AV / Created: 12Aug2017 

BSMax <- function() {

  filepath <- file.path(Sys.getenv("USERPROFILE"), 
    "Documents/AC_Data/Derived/Demography/2016")
    
  dem <- tbl_df(read_tsv(file.path(filepath, "RD02-01_ACDIS_Demography.csv"))) %>% 
    select(BSIntID, IIntID, Year=ExpYear, Episode, ExpDays) %>% 
    arrange(IIntID, Episode)

  # Identify max expdays per episode
  maxBS <- group_by(dem, IIntID, Year) %>% 
    mutate(MaxDays = max(ExpDays, na.rm=TRUE))
    
  # Identify the BSIntID
  maxBS <- group_by(maxBS, IIntID, Year) %>%
    mutate(MaxBSIntID=ifelse(MaxDays==ExpDays, BSIntID, NA)) %>%
    ungroup(maxBS)
    
  # Deal with same time in 2+ episodes, just take the first BS
  maxBS <- filter(maxBS, !is.na(MaxBSIntID)) %>%
    select(IIntID, Year, MaxBSIntID ) %>%
    arrange(IIntID, Year) 

  maxBS <- group_by(maxBS, IIntID, Year) %>% 
    filter(row_number()==1)

  write_csv(maxBS, file.path(filepath, "MaxBSIntID.csv"))
}
