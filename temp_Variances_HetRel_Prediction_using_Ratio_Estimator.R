

packages <- c("tidyverse", "here")

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


var_ratio <- function(var_x, var_y, mu_x, mu_y, cov_xy){
  var_x/mu_y^2 + (mu_x^2 * var_y)/mu_y^4 - (2 * mu_x * cov_xy)/mu_y^3
}



varT_rma.list <- readRDS(file = here("Notes/bootstrapped_varT_rma.RData"))

varE_rma.list <- readRDS(file = here("Notes/bootstrapped_varE_rma.RData"))

varX_rma.list <- readRDS(file = here("Notes/bootstrapped_varX_rma.RData"))

data.list <- list.files(here("Data/Extracted (Project) Data"), full.names = TRUE) %>%
  sapply(., read.csv)



Reliability_estimates_paths <- list.files(here("Data/Reliability Estimates"), full.names =  TRUE)

Alpha_estimates.list <- Reliability_estimates_paths[grep("_Alpha.csv$", Reliability_estimates_paths)] %>%
  lapply(., read.csv)

names(Alpha_estimates.list) <- substr(Reliability_estimates_paths[grep("_Alpha.csv$", Reliability_estimates_paths)],
                                      (regexpr("Reliability Estimates/", Reliability_estimates_paths[grep("_Alpha.csv$", Reliability_estimates_paths)]) + 22),
                                      (nchar(Reliability_estimates_paths[grep("_Alpha.csv$", Reliability_estimates_paths)])-10))

Alpha_rma.list <- readRDS(here("Data/Shiny Data/Alpha_rma.list.RData"))




var_ratio_rel.tau2_est <- sapply(seq_along(varE_rma.list), FUN = function(x){
  tryCatch(
    var_ratio(var_x = varT_rma.list[[x]]$tau2, 
              var_y = varX_rma.list[[x]]$tau2, 
              mu_x = varT_rma.list[[x]]$b[1], 
              mu_y = varX_rma.list[[x]]$b[1],
              cov_xy = cov(as.numeric(varT_rma.list[[x]]$yi), as.numeric(varX_rma.list[[x]]$yi)))
    ,
    error = function(e)(cat("ERROR: ", conditionMessage(e), " - ",
                            substr(names(data.list), 
                                   (regexpr("Project) Data/", names(data.list)) + 14), 
                                   (nchar(names(data.list))-4))[x], 
                            " - ", x, "\n")))
  
  
})


plot(
  unlist(var_ratio_rel.tau2_est),
  unlist(lapply(seq_along(Alpha_rma.list), FUN = function(x){
    Alpha_rma.list[[x]]$tau2
  }))
  
)

sapply(seq_along(varE_rma.list), FUN = function(x){
  tryCatch(
    
              cor(as.numeric(varT_rma.list[[x]]$yi), as.numeric(varX_rma.list[[x]]$yi))
              ,
              error = function(e)(cat("ERROR: ", conditionMessage(e), " - ",
                            substr(names(data.list), 
                                   (regexpr("Project) Data/", names(data.list)) + 14), 
                                   (nchar(names(data.list))-4))[x], 
                            " - ", x, "\n")))
  
  
})
