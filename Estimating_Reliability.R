### Reliability Generalization  ###

## 02/06/2022




###################################################################################################
# This script is used purely for data cleaning, initial manipulation and extraction to single     #
#  files per scale                                                                                #
# Raw data won't be made public, as long as no agreement from authors is obtained.                #
###################################################################################################


# library loading and installing as necessary



packages <- c("tidyverse", "here", "psych", "coefficientalpha")

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



path_data <- list.files(here("Data/Extracted (Project) Data"), full.names = TRUE)


data.list <- sapply(path_data, read.csv)


alpha_estimates.list <- lapply(seq_along(data.list), FUN = function(x){
  estimate_alpha(data.list[[x]], csv = TRUE, 
                 project.title = substr(names(data.list), 
                                        (regexpr("Project) Data/", names(data.list)) + 14), 
                                        (nchar(names(data.list))-4))[x])
                   
})


omega_estimates.list <- lapply(seq_along(data.list), FUN = function(x){
  estimate_omega(data.list[[x]], csv = TRUE, 
                 project.title = substr(names(data.list),
                                        (regexpr("Project) Data/", names(data.list)) + 14),
                                        (nchar(names(data.list))-4))[x])

})


Bonett.alpha_estimates.list <- lapply(seq_along(data.list), FUN = function(x){
  estimate_Bonett_alpha(data.list[[x]], csv = TRUE, 
                        project.title = substr(names(data.list), 
                                               (regexpr("Project) Data/", names(data.list)) + 14), 
                                               (nchar(names(data.list))-4))[x])
})






