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


# load library of functions created for this project
source(here("RG_function-library.R"))


# identify all extracted project data files
path_data <- list.files(here("Data/Extracted (Project) Data"), full.names = TRUE)


# load all data-files into a single list
data.list <- sapply(path_data, read.csv)



# the data-files in the list-object have a standardised format.
#  therefore, Bonett-transformed Cronbach's Alpha can be estimated in a simple loop across all 
#  list-elements
Bonett.alpha_estimates.list <- lapply(seq_along(data.list), FUN = function(x){
  
  # the transformation may not work, as some estimates of score reliability may be negative. In
  #  that case, it is not defined and the function throws an error. Therefore, this is nested
  #  in a tryCatch()-
  tryCatch({
    
    # csv = TRUE indicates that generated estimates are stored in .csv-files, in the Data/Reliability Estimates
    #  sub-directory. This is required, if the scripts are run for the first time, as no data was uploaded
    #  to the repository.
    estimate_Bonett_alpha(data.list[[x]], csv = TRUE, 
                          project.title = substr(names(data.list), 
                                                 (regexpr("Project) Data/", names(data.list)) + 14), 
                                                 (nchar(names(data.list))-4))[x])
  },
  
  # If there is an issue with a scale, as some replications led to negative estimates of score reliability, 
  #  this code will print the name of the respective project/scale
  error = function(e)(cat("ERROR: ", conditionMessage(e), " - ", 
                          substr(names(data.list), 
                                 (regexpr("Project) Data/", names(data.list)) + 14), 
                                 (nchar(names(data.list))-4))[x], 
                          " - ", x, "\n"))
  )
}) # the resulting file is another list, containing data.frames for each assessed scale.
#  each data.frame contains an estimate of Bonett-transformed reliability using Cronbach's 
#  Alpha, its standard error and in which lab/source the data, from which the estimate was 
#  derived, was collected



