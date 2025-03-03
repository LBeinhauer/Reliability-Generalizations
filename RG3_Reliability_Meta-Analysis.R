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


Bonett.Alpha_estimates_paths <- Reliability_estimates_paths[grep("_Bonett-Alpha.csv$", Reliability_estimates_paths)]

Bonett.Alpha_rma.list <- lapply(Bonett.Alpha_estimates_paths[-7], FUN = function(x){
  d <- read.csv(x)
  
  metafor::rma(measure = "GEN", method = "REML", yi = d$Reliability, sei = d$StandardError)
})

names(Bonett.Alpha_rma.list) <- substr(Bonett.Alpha_estimates_paths[-7],
                                       (regexpr("Reliability Estimates/", Bonett.Alpha_estimates_paths[-7]) + 22),
                                       (nchar(Bonett.Alpha_estimates_paths[-7])-17))


saveRDS(Bonett.Alpha_rma.list, file = here("Data/Shiny Data/Bonett.Alpha_rma.list.RData"))


# function to estimate heterogeneity in Cronbach's Alpha, if transformation was used
var_Bonnett_backtransformed <- function(rma_obj){
  (((-exp(rma_obj$b[1]))^2) * rma_obj$tau2) + (.5*((-exp(rma_obj$b[1]))^2)*(rma_obj$tau2^2)) + ((-exp(rma_obj$b[1])) * (-exp(rma_obj$b[1])) * (rma_obj$tau2^2))
}

# function to estimate mean value of Cronbach's Alpha, if transformation was used
mean_Bonnett_backtransformed <- function(rma_obj){
  1 - exp(rma_obj$b[1]) + ((-exp(rma_obj$b[1])) / 2) * rma_obj$tau2
}






RG_ma_df <- data.frame(mu_alpha = sapply(Bonett.Alpha_rma.list, mean_Bonnett_backtransformed),
                       tau_alpha = sqrt(sapply(Bonett.Alpha_rma.list, var_Bonnett_backtransformed)),
                       QEp = sapply(Bonett.Alpha_rma.list, FUN = function(x){x$QEp}),
                       I2 = sapply(Bonett.Alpha_rma.list, FUN = function(x){x$I2}),
                       H2 = sapply(Bonett.Alpha_rma.list, FUN = function(x){x$H2}))

write.csv(RG_ma_df, file = here("Data/Shiny Data/RG-MA_results.csv"))

