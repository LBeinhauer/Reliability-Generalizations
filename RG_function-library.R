### Reliability Generalization HEXACO ###

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




estimate_alpha <- function(data, csv = FALSE, project.title = NULL){
  
  k <- length(unique(data$source))
  s.idx <- grep("source", names(data))
  
  est <- apply(as.matrix(1:k), MARGIN = 1, FUN = function(x){
    d <- data[which(data$source == unique(data$source)[x]), -s.idx]
    
    suppressWarnings(psych::alpha(d))
  })  
  
  df <- data.frame(Reliability = sapply(est, FUN = function(x){x$total$raw_alpha}),
                   StandardError = sapply(est, FUN = function(x){x$total$ase}),
                   source = unique(data$source))
  
  if(csv){
    write.csv(df, file = here(paste0("Data/Reliability Estimates/", project.title, "_Alpha.csv")), row.names = FALSE)
  }
  
  return(df)
}




estimate_omega <- function(data, csv = FALSE, project.title = NULL){
  
  k <- length(unique(data$source))
  s.idx <- grep("source", names(data))
  
  est <- sapply(1:k, FUN = function(x){
    d <- data[which(data$source == unique(data$source)[x]), -s.idx]
    
    fit <- coefficientalpha::omega(d, se = TRUE, test = FALSE, silent = TRUE)
    
    return(c(fit$omega, fit$se))
  })  
  
  df <- data.frame(Reliability = t(est)[,1],
                   StandardError = t(est)[,2],
                   source = unique(data$source))
  
  if(csv){
    write.csv(df, file = here(paste0("Data/Reliability Estimates/", project.title, "_Omega.csv")), row.names = FALSE)
  }
  
  return(df)
}


estimate_Bonett_alpha <- function(data, csv = FALSE, project.title = NULL){
  
  k <- length(unique(data$source))
  s.idx <- grep("source", names(data))
  j <- k-1
  n <- data %>%
    group_by(source) %>%
    summarise(n = n())
  n <- n$n
  
  est <- apply(as.matrix(1:k), MARGIN = 1, FUN = function(x){
    d <- data[which(data$source == unique(data$source)[x]), -s.idx]
    
    suppressWarnings(psych::alpha(d))
  })  
  
  Alpha <- sapply(est, FUN = function(x){x$total$raw_alpha})
  B.Alpha <- log(1 - abs(Alpha))
  SE_B.Alpha <- sqrt((2 * j)/((j - 1) * (n - 2)))                
                  
  df <- data.frame(Reliability = B.Alpha,
                   StandardError = SE_B.Alpha,
                   source = unique(data$source))
  
  if(csv){
    write.csv(df, file = here(paste0("Data/Reliability Estimates/", project.title, "_Bonett-Alpha.csv")), row.names = FALSE)
  }
  
  return(df)
}
