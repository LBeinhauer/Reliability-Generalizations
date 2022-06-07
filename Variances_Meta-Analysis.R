### Reliability Generalization - Meta-Analysis of Variance Components ###

## 07/06/2022




###################################################################################################
# This script is used purely for breaking down sample variance into components containing either  #
#  True or Error Variance (according to CTT) using estimates of reliability. Using Bootstrapping, #
#  we can generate standard error estimates for these components, and run a random-effects meta-  #
#  analysis.                                                                                      #
###################################################################################################


# library loading and installing as necessary

packages <- c("tidyverse", "here", "psych", "coefficientalpha", "boot")

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








set.seed(070622)

long_test_T <- lapply(seq_along(data.list), FUN = function(x){
  tryCatch(apply_Bootstrap_SE_Project.specific(data.list[[x]], var.component = "TRUE"),
           error = function(e)(cat("ERROR: ", conditionMessage(e), " - ",
                                   substr(names(data.list), 
                                          (regexpr("Project) Data/", names(data.list)) + 14), 
                                          (nchar(names(data.list))-4))[x], 
                                   " - ", x, "\n")))
})

save(long_test_T, file = here("Notes/bootstrapped_varT.RData"))

long_test_E <- lapply(seq_along(data.list), FUN = function(x){
  tryCatch(apply_Bootstrap_SE_Project.specific(data.list[[x]], var.component = "ERROR"),
           error = function(e)(cat("ERROR: ", conditionMessage(e), " - ",
                                   substr(names(data.list), 
                                          (regexpr("Project) Data/", names(data.list)) + 14), 
                                          (nchar(names(data.list))-4))[x], 
                                   " - ", x, "\n")))
})






varT_rma.list <- lapply(seq_along(long_test_T), FUN = function(x){
  tryCatch(metafor::rma(measure = "GEN", method = "REML", 
                        yi = long_test_T[[x]]$boot.mean, 
                        sei = long_test_T[[x]]$SE),
           error = function(e)(cat("ERROR: ", conditionMessage(e), " - ",
                                   substr(names(data.list), 
                                          (regexpr("Project) Data/", names(data.list)) + 14), 
                                          (nchar(names(data.list))-4))[x], 
                                   " - ", x, "\n")))
  
})


varE_rma.list <- lapply(seq_along(long_test_E), FUN = function(x){
  tryCatch(metafor::rma(measure = "GEN", method = "REML", 
                        yi = long_test_E[[x]]$boot.mean, 
                        sei = long_test_E[[x]]$SE),
           error = function(e)(cat("ERROR: ", conditionMessage(e), " - ",
                                   substr(names(data.list), 
                                          (regexpr("Project) Data/", names(data.list)) + 14), 
                                          (nchar(names(data.list))-4))[x], 
                                   " - ", x, "\n")))
  
})





library(gridExtra)
library(grid)

pdf(here("Graphics/ForestPlots_varET_test.pdf"), width = 9, height = 6)

laymat <- matrix(c(1, 1, 1, 1, 1, 3, 3,
                   1, 1, 1, 1, 1, 3, 3,
                   1, 1, 1, 1, 1, 3, 3,
                   1, 1, 1, 1, 1, 3, 3,
                   2, 2, 2, 2, 2, 4, 4, 
                   2, 2, 2, 2, 2, 4, 4,
                   2, 2, 2, 2, 2, 4, 4,
                   2, 2, 2, 2, 2, 4, 4), byrow = T, ncol = 7)

lapply(seq_along(data.list), FUN = function(x){
  
  tryCatch(
    {
      lT <- long_test_T[[x]]
      vT <- varT_rma.list[[x]]
      
      lE <- long_test_E[[x]]
      vE <- varE_rma.list[[x]]
      
      
      pT <- my_forest_plot(rma.fit = vT, rma.data = lT,
                           main.title = paste0("Forest Plot - ", substr(names(data.list),
                                                                        (regexpr("Project) Data/", names(data.list)) + 14),
                                                                        (nchar(names(data.list))-4))[x]),
                           x.lab = "True Variance (estimated)", ci.lvl = .975, CI.display = TRUE)
      
      pE <- my_forest_plot(rma.fit = vE, rma.data = lE,
                           main.title = paste0("Forest Plot - ", substr(names(data.list),
                                                                        (regexpr("Project) Data/", names(data.list)) + 14),
                                                                        (nchar(names(data.list))-4))[x]),
                           x.lab = "Error Variance (estimated)", ci.lvl = .975, CI.display = TRUE)
      
      tT <- textGrob(
        paste0("MA-Est.: ", round(vT$b[1], 3), 
               "   [",(round(vT$b[1] - qnorm(.975)*sqrt(vT$tau2), 2)), ";",
               (round(vT$b[1] + qnorm(.975)*sqrt(vT$tau2), 2)), "]","\n \n", 
               "tau: ", round(sqrt(vT$tau2), 4), 
               "    I2: ", round(vT$I2, 2), "\n \n",
               "p(QE) = ", if(vT$QEp < .0001){"<.0001"}else{round(vT$QEp, 4)},
               if(vT$QEp < .05){" *"}else{""})
      )
      
      tE <- textGrob(
        paste0("MA-Est.: ", round(vE$b[1], 3), 
               "   [",(round(vE$b[1] - qnorm(.975)*sqrt(vE$tau2), 2)), ";",
               (round(vE$b[1] + qnorm(.975)*sqrt(vE$tau2), 2)), "]","\n \n", 
               "tau: ", round(sqrt(vE$tau2), 4), 
               "    I2: ", round(vE$I2, 2), "\n \n",
               "p(QE) = ", if(vE$QEp < .0001){"<.0001"}else{round(vE$QEp, 4)},
               if(vE$QEp < .05){" *"}else{""})
      )
      
      
      grid.arrange(pT, pE, tT, tE, layout_matrix = laymat)
    },
    
    error = function(e)(cat("ERROR: ", conditionMessage(e), " - ",
                            substr(names(data.list), 
                                   (regexpr("Project) Data/", names(data.list)) + 14), 
                                   (nchar(names(data.list))-4))[x], 
                            " - ", x, "\n"))
    
  )
  
  
  
})

dev.off()


