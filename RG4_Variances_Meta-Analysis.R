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

# long_test_T <- lapply(seq_along(data.list), FUN = function(x){
#   tryCatch(apply_Bootstrap_SE_Project.specific(data.list[[x]], var.component = "TRUE"),
#            error = function(e)(cat("ERROR: ", conditionMessage(e), " - ",
#                                    substr(names(data.list), 
#                                           (regexpr("Project) Data/", names(data.list)) + 14), 
#                                           (nchar(names(data.list))-4))[x], 
#                                    " - ", x, "\n")))
# })
# 
# saveRDS(long_test_T, file = here("Notes/bootstrapped_varT.RData"))

long_test_lnE <- lapply(seq_along(data.list), FUN = function(x){
  tryCatch(apply_Bootstrap_SE_Project.specific(data.list[[x]], var.component = "ERROR"),
           
           error = function(e)(cat("ERROR: ", conditionMessage(e), " - ",
                                   substr(names(data.list), 
                                          (regexpr("Project) Data/", names(data.list)) + 14), 
                                          (nchar(names(data.list))-4))[x], 
                                   " - ", x, "\n")))
})

saveRDS(long_test_lnE, file = here("Data/Shiny Data/bootstrapped_lnvarE.RData"))


varE_rma.list <- lapply(seq_along(long_test_lnE), FUN = function(x){
  tryCatch(metafor::rma(measure = "GEN", method = "REML", 
                        yi = long_test_lnE[[x]]$var.est, 
                        sei = long_test_lnE[[x]]$SE),
           error = function(e)(cat("ERROR: ", conditionMessage(e), " - ",
                                   substr(names(data.list), 
                                          (regexpr("Project) Data/", names(data.list)) + 14), 
                                          (nchar(names(data.list))-4))[x], 
                                   " - ", x, "\n")))
  
})

names(varE_rma.list) <- names(data.list)

saveRDS(varE_rma.list, file = here("Data/Shiny Data/bootstrapped_varE_rma.RData"))


varE_rma <- data.frame(mu_varE = sapply(varE_rma.list, bt_var_m),
                       tau_varE = sqrt(sapply(varE_rma.list, bt_var_v)),
                       QEp = sapply(varE_rma.list, FUN = function(x){x$QEp}),
                       H2 = sapply(varE_rma.list, FUN = function(x){x$H2}),
                       I2 = sapply(varE_rma.list, FUN = function(x){x$I2}),
                       MASC = str_sub(names(data.list), start = 96, end = nchar(names(data.list)) - 4),
                       row.names = NULL)




long_test_lnX <- lapply(seq_along(data.list), FUN = function(x){
  tryCatch(apply_Bootstrap_SE_Project.specific(data.list[[x]], var.component = "TOTAL"),
           
           error = function(e)(cat("ERROR: ", conditionMessage(e), " - ",
                                   substr(names(data.list), 
                                          (regexpr("Project) Data/", names(data.list)) + 14), 
                                          (nchar(names(data.list))-4))[x], 
                                   " - ", x, "\n")))
})

saveRDS(long_test_lnX, file = here("Data/Shiny Data/bootstrapped_varX.RData"))




varX_rma.list <- lapply(seq_along(long_test_lnX), FUN = function(x){
  tryCatch(metafor::rma(measure = "GEN", method = "REML", 
                        yi = long_test_lnX[[x]]$var.est, 
                        sei = long_test_lnX[[x]]$SE),
           error = function(e)(cat("ERROR: ", conditionMessage(e), " - ",
                                   substr(names(data.list), 
                                          (regexpr("Project) Data/", names(data.list)) + 14), 
                                          (nchar(names(data.list))-4))[x], 
                                   " - ", x, "\n")))
  
})

names(varX_rma.list) <- names(data.list)

saveRDS(varX_rma.list, file = here("Data/Shiny Data/bootstrapped_varx_rma.RData"))

varX_rma <- data.frame(mu_varX = sapply(varX_rma.list, bt_var_m),
                       tau_varX = sqrt(sapply(varX_rma.list, bt_var_v)),
                       QEp = sapply(varX_rma.list, FUN = function(x){x$QEp}),
                       H2 = sapply(varX_rma.list, FUN = function(x){x$H2}),
                       I2 = sapply(varX_rma.list, FUN = function(x){x$I2}),
                       MASC = str_sub(names(data.list), start = 96, end = nchar(names(data.list)) - 4),
                       row.names = NULL)




my_forest_plot(varX_rma.list[[2]], long_test_lnX[[2]], transformation = "ln")



library(gridExtra)
library(grid)


laymat <- matrix(c(1, 1, 1, 1, 1, 3, 3,
                   1, 1, 1, 1, 1, 3, 3,
                   1, 1, 1, 1, 1, 3, 3,
                   1, 1, 1, 1, 1, 3, 3,
                   2, 2, 2, 2, 2, 4, 4, 
                   2, 2, 2, 2, 2, 4, 4,
                   2, 2, 2, 2, 2, 4, 4,
                   2, 2, 2, 2, 2, 4, 4), byrow = T, ncol = 7)


pdf(here("Graphics/ForestPlots_varET_test.pdf"), width = 9, height = 6)

lapply(seq_along(data.list), FUN = function(x){
  
  tryCatch({
    
    lX <- long_test_lnX[[x]]
    vX <- varX_rma.list[[x]]
    
    lE <- long_test_lnE[[x]]
    vE <- varE_rma.list[[x]]
    
    pX <- my_forest_plot(rma.fit = vX,
                         rma.data = lX,
                         transformation = "ln",
                         x.lab = "Total Score Variance",
                         main.title = "")
    
    pE <- my_forest_plot(rma.fit = vE,
                         rma.data = lE,
                         transformation = "ln",
                         x.lab = "Error Score Variance",
                         main.title = "")
    
    tX <- textGrob(
      paste0("MA-Est.: ", round(bt_var_m(vX), 3), 
             "   [",(round(exp(vX$b[1] - qnorm(.975)*sqrt(vX$tau2)), 2)), ";",
             (round(exp(vX$b[1] + qnorm(.975)*sqrt(vX$tau2)), 2)), "]","\n \n", 
             "tau: ", round(sqrt(bt_var_v(vX)), 3), 
             "    I2: ", round(vX$I2, 2), "\n \n",
             "p(QE) = ", if(vX$QEp < .001){"<.001"}else{round(vX$QEp, 3)},
             if(vX$QEp < .05){" *"}else{""})
    )
    
    tE <- textGrob(
      paste0("MA-Est.: ", round(bt_var_m(vE), 3), 
             "   [",(round(exp(vE$b[1] - qnorm(.975)*sqrt(vE$tau2)), 2)), ";",
             (round(exp(vE$b[1] + qnorm(.975)*sqrt(vE$tau2)), 2)), "]","\n \n", 
             "tau: ", round(sqrt(bt_var_v(vE)), 3), 
             "    I2: ", round(vE$I2, 2), "\n \n",
             "p(QE) = ", if(vE$QEp < .001){"<.001"}else{round(vE$QEp, 3)},
             if(vE$QEp < .05){" *"}else{""})
    )
    
    grid.arrange(pX, pE, tX, tE, layout_matrix = laymat, 
                 top = substr(names(data.list)[x], 
                              start = gregexpr(pattern ='Extracted', names(data.list)[1])[[1]][1] + 25, 
                              stop = (nchar(names(data.list)) - 4)[x]))
    
  
  },
  
  error = function(e)(cat("ERROR: ", conditionMessage(e), " - ",
                          substr(names(data.list), 
                                 (regexpr("Project) Data/", names(data.list)) + 14), 
                                 (nchar(names(data.list))-4))[x], 
                          " - ", x, "\n")))
  
  
  
  
})


dev.off()




