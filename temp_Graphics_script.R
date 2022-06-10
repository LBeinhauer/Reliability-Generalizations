### Reliability Generalization - temp graphics script ###

## 10/06/2022




###################################################################################################
# This script is used purely for data visualizations. It is a temporary file, that will likely be #
#  deleted in the future
###################################################################################################


# library loading and installing as necessary

packages <- c("tidyverse", "here", "metafor", "grid", "gridExtra", "svglite")

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


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


pdf(here("Graphics/ForestPlots_Alpha_test.pdf"), width = 9, height = 6)

laymat <- matrix(c(1, 1, 1, 1, 2, 2, 2,
                   1, 1, 1, 1, 2, 2, 2,
                   1, 1, 1, 1, 2, 2, 2,
                   1, 1, 1, 1, 3, 3, 3, 
                   1, 1, 1, 1, 3, 3, 3), byrow = T, ncol = 7)


lapply(seq_along(Alpha_estimates.list)[-7], FUN = function(x){
  
  AE <- Alpha_estimates.list[[x]]
  AR <- Alpha_rma.list[[x]]
  

  p <- my_forest_plot(rma.data = AE, rma.fit = AR,
                      main.title = paste0("Forest Plot - ", substr(names(data.list),
                                                                   (regexpr("Project) Data/", names(data.list)) + 14),
                                                                   (nchar(names(data.list))-4))[x]),
                      x.lab = "Cronbach's Alpha", ci.lvl = .975, CI.display = TRUE)
  
  
  bw_FD <- (2 * IQR(AE$Reliability))/length(AE$Reliability)^(1/3)
  
  h <- ggplot(AE) +
    geom_histogram(aes(x = Reliability), 
                   binwidth = bw_FD,
                   colour = "black", fill = gg_color_hue(2)[1]) +
    labs(x = "Cronbach's Alpha", y = "Frequency")
  
  
  
  t <- textGrob(
    paste0("Meta-Analytic Estimate: ", round(AR$b[1], 3), 
           "   [",(round(AR$b[1] - qnorm(.975)*sqrt(AR$tau2), 2)), ";",
           (round(AR$b[1] + qnorm(.975)*sqrt(AR$tau2), 2)), "]","\n \n", 
           "Heterogeneity -> tau: ", round(sqrt(AR$tau2), 4), 
           "    I^2: ", round(AR$I2, 2))
  )
  
  
  
  grid.arrange(p, h, t, layout_matrix = laymat)
  
  
  
  
})

dev.off()







pdf(here("Graphics/ForestPlots_Bonett_Alpha_test.pdf"), width = 9, height = 6)

laymat <- matrix(c(1, 1, 1, 1, 2, 2, 2,
                   1, 1, 1, 1, 2, 2, 2,
                   1, 1, 1, 1, 2, 2, 2,
                   1, 1, 1, 1, 3, 3, 3, 
                   1, 1, 1, 1, 3, 3, 3), byrow = T, ncol = 7)


lapply(seq_along(Bonett.Alpha_estimates.list[-7]), FUN = function(x){
  
  AE <- Bonett.Alpha_estimates.list[-7][[x]]
  AR <- Bonett.Alpha_rma.list[[x]]
  
  
  p <- my_forest_plot(rma.data = AE, rma.fit = AR,
                      main.title = paste0("Forest Plot - ", substr(names(data.list),
                                                                   (regexpr("Project) Data/", names(data.list)) + 14),
                                                                   (nchar(names(data.list))-4))[x]),
                      x.lab = "Bonett ln(1 - Alpha)", ci.lvl = .975, CI.display = TRUE)
  
  
  bw_FD <- (2 * IQR(AE$Reliability))/length(AE$Reliability)^(1/3)
  
  h <- ggplot(AE) +
    geom_histogram(aes(x = Reliability), 
                   binwidth = bw_FD,
                   colour = "black", fill = gg_color_hue(2)[2]) +
    labs(x = "Bonett ln(1 - Alpha)", y = "Frequency")
  
  
  
  t <- textGrob(
    paste0("Meta-Analytic Estimate: ", round(AR$b[1], 3), 
           "   [",(round(AR$b[1] - qnorm(.975)*sqrt(AR$tau2), 2)), ";",
           (round(AR$b[1] + qnorm(.975)*sqrt(AR$tau2), 2)), "]","\n \n", 
           "Heterogeneity -> tau: ", round(sqrt(AR$tau2), 4), 
           "    I^2: ", round(AR$I2, 2))
  )
  
  
  
  grid.arrange(p, h, t, layout_matrix = laymat)
  
  
  
  
})

dev.off()













pdf(here("Graphics/ForestPlots_Back-Transformed_Bonett_Alpha_test.pdf"), width = 9, height = 6)

laymat <- matrix(c(1, 1, 1, 1, 2, 2, 2,
                   1, 1, 1, 1, 2, 2, 2,
                   1, 1, 1, 1, 2, 2, 2,
                   1, 1, 1, 1, 3, 3, 3, 
                   1, 1, 1, 1, 3, 3, 3), byrow = T, ncol = 7)


lapply(seq_along(Bonett.Alpha_estimates.list[-7]), FUN = function(x){
  
  AEB <- Bonett.Alpha_estimates.list[-7][[x]]
  AE <- data.frame(Reliability = 1-exp(AEB$Reliability),
                   StandardError = 1-exp(AEB$StandardError),
                   source = AEB$source)
  AR <- Bonett.Alpha_rma.list[[x]]
  ARB <- AR 
  ARB$yi <- 1-exp(ARB$yi)
  ARB$vi <- 1-exp(ARB$vi)
  ARB$se <- 1-exp(ARB$se)
  ARB$b <- 1-exp(ARB$b)
  
  
  p <- my_forest_plot(rma.data = AE, rma.fit = ARB,
                      main.title = paste0("Forest Plot - ", substr(names(data.list),
                                                                   (regexpr("Project) Data/", names(data.list)) + 14),
                                                                   (nchar(names(data.list))-4))[x]),
                      x.lab = "BTb Cronbach's Alpha", ci.lvl = .975, CI.display = TRUE)
  
  
  bw_FD <- (2 * IQR(AE$Reliability))/length(AE$Reliability)^(1/3)
  
  h <- ggplot(AE) +
    geom_histogram(aes(x = Reliability), 
                   binwidth = bw_FD,
                   colour = "black", fill = "orange") +
    labs(x = "BTb Cronbach's Alpha", y = "Frequency")
  
  
  
  t <- textGrob(
    paste0("Meta-Analytic Estimate: ", round(1-exp(AR$b[1]), 3), 
           "   [",(round(1-exp(AR$b[1] - qnorm(.975)*sqrt(AR$tau2)), 2)), ";",
           (round(1-exp(AR$b[1] + qnorm(.975)*sqrt(AR$tau2)), 2)), "]","\n \n", 
           "Heterogeneity -> tau: ", round(1-exp(sqrt(AR$tau2)), 4), 
           "    I^2: ", round(AR$I2, 2))
  )
  
  
  
  grid.arrange(p, h, t, layout_matrix = laymat)
  
  
  
  
})

dev.off()





alpha_tau <- sapply(Alpha_rma.list, FUN = function(x){
  sqrt(x$tau2)
})


alpha_I2 <- sapply(Alpha_rma.list, FUN = function(x){
  x$I2
})

alpha_het.sig <- sapply(Alpha_rma.list, FUN = function(x){
  x$QEp <= .05
})

sum(!alpha_het.sig, na.rm = T)

B.alpha_tau <- sapply(Bonett.Alpha_rma.list, FUN = function(x){
  sqrt(x$tau2)
})

# 1-exp(B.alpha_tau)

B.alpha_I2 <- sapply(Bonett.Alpha_rma.list, FUN = function(x){
  x$I2
})

B.alpha_het.sig <- sapply(Bonett.Alpha_rma.list, FUN = function(x){
  x$QEp <= .05
})

sum(!B.alpha_het.sig, na.rm = T)

violin_df <- data.frame(tau = c(alpha_tau[-7], B.alpha_tau),
                        I2 = c(alpha_I2[-7], B.alpha_I2),
                        sig = c(alpha_het.sig[-7], B.alpha_het.sig),
                        stat = as.factor(c(rep(1, length(alpha_tau[-7])), rep(2, length(alpha_tau[-7])))))



violin_I2 <- violin_df %>%
  ggplot() + 
  geom_violin(aes(x = stat, y = I2), fill = "transparent") +
  geom_boxplot(aes(x = stat, y = I2), width = .1, fill = "transparent") +
  geom_point(aes(x = stat, y = I2, colour = stat, shape = sig), 
             position = position_jitter(w = 0.1, h = 0), size = 3, alpha = .7) +
  scale_x_discrete(labels = c("Untransformed", "Bonett-Transformed")) +
  scale_shape_manual(values = c(21, 16)) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "transparent",
                                        colour = NA_character_), 
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "transparent"),
        axis.line.x = element_line(colour = "transparent"),
        axis.line.y = element_line(colour = "black"),
        axis.ticks.x = element_line(colour = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  labs(x = "", y = "I2", title = "Violin Plot - I2 estimates of Cronbach's Alpha")

ggsave(
  plot = violin_I2,
  filename = here("Graphics/temp_ViolinPlot_I2_Alpha.svg"),
  bg = "transparent",
  width = 6,
  height = 4,
  units = "in"
)




v1 <- violin_df[which(violin_df$stat == 1),] %>%
  ggplot() + 
  geom_violin(aes(x = 0, y = tau), fill = "transparent") +
  geom_boxplot(aes(y = tau), width = .1, fill = "transparent") +
  geom_point(aes(x = 0, y = tau, shape = sig), colour = gg_color_hue(2)[1], 
             position = position_jitter(w = 0.1, h = 0), size = 3, alpha = .7) +
  scale_x_discrete(labels = c("Untransformed")) +
  scale_shape_manual(values = c(21, 16)) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "transparent",
                                        colour = NA_character_), 
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "transparent"),
        axis.line.x = element_line(colour = "transparent"),
        axis.line.y = element_line(colour = "black"),
        axis.ticks.x = element_line(colour = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  labs(x = "", y = "tau") +
  scale_x_continuous(breaks = 0, labels = "Untransformed")

v2 <- violin_df[which(violin_df$stat == 2),] %>%
  ggplot() + 
  geom_violin(aes(x = 0, y = tau), fill = "transparent") +
  geom_boxplot(aes(y = tau), width = .1, fill = "transparent") +
  geom_point(aes(x = 0, y = tau, shape = sig), colour = gg_color_hue(2)[2], 
             position = position_jitter(w = 0.1, h = 0), size = 3, alpha = .7) +
  scale_x_discrete(labels = c("Bonett-transformed")) +
  scale_shape_manual(values = c(21, 16)) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "transparent",
                                        colour = NA_character_), 
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "transparent"),
        axis.line.x = element_line(colour = "transparent"),
        axis.line.y = element_line(colour = "black"),
        axis.ticks.x = element_line(colour = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  labs(x = "", y = "tau") +
  scale_x_continuous(breaks = 0, labels = "Bonett-Transformed")


violin_tau <- gridExtra::grid.arrange(v1, v2, ncol = 2, top = "Violin Plot - tau estimates for Cronbach's Alpha")

ggsave(
  plot = violin_tau,
  filename = here("Graphics/temp_ViolinPlot_tau_Alpha.svg"),
  bg = "transparent",
  width = 6,
  height = 4,
  units = "in"
)

mean(B.alpha_I2)
mean(alpha_I2[-7])
mean(alpha_tau)
summary(alpha_tau)






laymat <- matrix(c(1, 1, 1, 1, 2, 2, 2,
                   1, 1, 1, 1, 2, 2, 2,
                   1, 1, 1, 1, 2, 2, 2,
                   1, 1, 1, 1, 2, 2, 2, 
                   1, 1, 1, 1, 2, 2, 2), byrow = T, ncol = 7)



AE <- Alpha_estimates.list[[27]]
AR <- Alpha_rma.list[[27]]


p <- my_forest_plot(rma.data = AE, rma.fit = AR,
                    main.title = paste0("Forest Plot - ", substr(names(data.list),
                                                                 (regexpr("Project) Data/", names(data.list)) + 14),
                                                                 (nchar(names(data.list))-4))[27]),
                    x.lab = "Cronbach's Alpha", ci.lvl = .975, CI.display = FALSE)


bw_FD <- (2 * IQR(AE$Reliability))/length(AE$Reliability)^(1/3)

h <- ggplot(AE) +
  geom_histogram(aes(x = Reliability), 
                 binwidth = bw_FD,
                 colour = "black", fill = gg_color_hue(2)[1]) +
  theme(panel.background = element_rect(fill = "white"), 
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"),
        axis.line.x = element_line(colour = "white"),
        axis.line.y = element_line(colour = "white"),
        axis.ticks.x = element_line(colour = "grey"),
        axis.ticks.y = element_line(colour = "grey")) +
  labs(x = "Cronbach's Alpha", y = "Frequency", title = paste0("Histogram - ", substr(names(data.list),
                                                                                      (regexpr("Project) Data/", names(data.list)) + 14),
                                                                                      (nchar(names(data.list))-4))[27]))




gridExtra::grid.arrange(p, h, layout_matrix = laymat)




