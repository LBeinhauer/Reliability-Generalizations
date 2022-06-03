### Reliability Generalization  ###

## 02/06/2022




###################################################################################################
# This script is used purely for data cleaning, initial manipulation and extraction to single     #
#  files per scale                                                                                #
# Raw data won't be made public, as long as no agreement from authors is obtained.                #
###################################################################################################


# library loading and installing as necessary

packages <- c("tidyverse", "here", "metafor")

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

source(here("RG_function-library.R"))




Reliability_estimates_paths <- list.files(here("Data/Reliability Estimates"), full.names =  TRUE)

Alpha_estimates_paths <- Reliability_estimates_paths[grep("_Alpha.csv$", Reliability_estimates_paths)]


Alpha_estimates.list <- lapply(Alpha_estimates_paths, read.csv)

Alpha_rma.list <- lapply(Alpha_estimates.list, FUN = function(x){
  metafor::rma(measure = "GEN", method = "REML", yi = x$Reliability, sei = x$StandardError)
})



Bonett.Alpha_estimates_paths <- Reliability_estimates_paths[grep("_Bonett-Alpha.csv$", Reliability_estimates_paths)]

Bonett.Alpha_estimates.list <- lapply(Bonett.Alpha_estimates_paths, read.csv)

Bonett.Alpha_rma.list <- lapply(Bonett.Alpha_estimates.list[-7], FUN = function(x){
  metafor::rma(measure = "GEN", method = "REML", yi = x$Reliability, sei = x$StandardError)
})




