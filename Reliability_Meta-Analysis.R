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


Alpha_rma.list <- lapply(Alpha_estimates_paths, FUN = function(x){
  d <- read.csv(x)
  
  metafor::rma(measure = "GEN", method = "REML", yi = d$Reliability, sei = d$StandardError)
})

names(Alpha_rma.list) <- substr(Alpha_estimates_paths,
                                (regexpr("Reliability Estimates/", Alpha_estimates_paths) + 22),
                                (nchar(Alpha_estimates_paths)-10))



Omega_estimates_paths <- Reliability_estimates_paths[grep("_Omega.csv$", Reliability_estimates_paths)]

Omega_rma.list <- lapply(Omega_estimates_paths, FUN = function(x){
  d <- read.csv(x)
  
  metafor::rma(measure = "GEN", method = "REML", yi = d$Reliability, sei = d$StandardError)
})

names(Omega_rma.list) <- substr(Omega_estimates_paths,
                                (regexpr("Reliability Estimates/", Omega_estimates_paths) + 22),
                                (nchar(Omega_estimates_paths)-10))



Bonett.Alpha_estimates_paths <- Reliability_estimates_paths[grep("_Bonett-Alpha.csv$", Reliability_estimates_paths)]

Bonett.Alpha_rma.list <- lapply(Bonett.Alpha_estimates_paths[-7], FUN = function(x){
  d <- read.csv(x)
  
  metafor::rma(measure = "GEN", method = "REML", yi = d$Reliability, sei = d$StandardError)
})

names(Bonett.Alpha_rma.list) <- substr(Bonett.Alpha_estimates_paths[-7],
                                       (regexpr("Reliability Estimates/", Bonett.Alpha_estimates_paths[-7]) + 22),
                                       (nchar(Bonett.Alpha_estimates_paths[-7])-17))




Bonett.Omega_estimates_paths <- Reliability_estimates_paths[grep("_Bonett-Omega.csv$", Reliability_estimates_paths)]

Bonett.Omega_rma.list <- lapply(Bonett.Omega_estimates_paths, FUN = function(x){
  d <- read.csv(x)
  
  metafor::rma(measure = "GEN", method = "REML", yi = d$Reliability, sei = d$StandardError)
})

names(Bonett.Omega_rma.list) <- substr(Bonett.Omega_estimates_paths,
                                       (regexpr("Reliability Estimates/", Bonett.Omega_estimates_paths) + 22),
                                       (nchar(Bonett.Omega_estimates_paths)-17))

saveRDS(Alpha_rma.list, file = here("Data/Shiny Data/Alpha_rma.list.RData"))
saveRDS(Omega_rma.list, file = here("Data/Shiny Data/Omega_rma.list.RData"))
saveRDS(Bonett.Alpha_rma.list, file = here("Data/Shiny Data/Bonett.Alpha_rma.list.RData"))
saveRDS(Bonett.Omega_rma.list, file = here("Data/Shiny Data/Bonett.Omega_rma.list.RData"))
