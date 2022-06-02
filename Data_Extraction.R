### Reliability Generalization HEXACO ###

## 02/06/2022




###################################################################################################
# This script is used purely for data cleaning, initial manipulation and extraction to single     #
#  files per scale                                                                                #
# Raw data won't be made public, as long as no agreement from authors is obtained.                #
###################################################################################################


# library loading and installing as necessary



packages <- c("tidyverse", "here", "data.table", "haven", "readxl")

# check, whether library already installed or not - install and load as needed:
apply(as.matrix(packages), MARGIN = 1, FUN = function(x) {
  
  pkg_avail <- nzchar(system.file(package = x))   # check if library is installed on system
  
  if(pkg_avail){
    require(x, character.only = TRUE)             # load the library, if already installed
    
  }else{
    install.packages(x)                           # install the library, if missing
    require(x, character.only = TRUE)             # load after installation
  }
})



# Extraction Scripts are saved in directory "Extraction_Scripts". These contain R-code, which
#  cleans and extracts data for each individual scale.
Extraction_Scripts <- list.files(here("Extraction_Scripts"), full.names = TRUE)


# BEWARE: This may take a while! Additionally, this will wipe your environment.
# extract data from all original data files - this results in a single csv for each scale.
sapply(Extraction_Scripts, source) # (in folder "Data/Extracted (Project) Data")

