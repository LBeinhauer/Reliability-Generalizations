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


Alpha_rma.list <- readRDS(file = here("Data/Shiny Data/Alpha_rma.list.RData"))
Bonett.Alpha_rma.list <- readRDS(file = here("Data/Shiny Data/Bonett.Alpha_rma.list.RData"))

Reliability_estimates_paths <- list.files(here("Data/Reliability Estimates"), full.names =  TRUE)

Alpha_estimates.list <- Reliability_estimates_paths[grep("_Alpha.csv$", Reliability_estimates_paths)][-7] %>%
  lapply(., read.csv)

Bonett.Alpha_estimates.list <- Reliability_estimates_paths[grep("_Bonett-Alpha.csv$", Reliability_estimates_paths)][-7] %>%
  lapply(., read.csv)


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}





#################################################################
# Generating PDF - Forest Plot, Histogram, Text of Meta-Analyis #
#################################################################

#######
# Cronbach's Alpha Untransformed
######


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




######
# Cronbach's Alpha Bonett-Transformed
######


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









######
# Cronbach's Alpha Back-Transformed
######



# DOES NOT WORK!


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







###################################################################
# Collecting estimates, tau, I2, sig. for each scale respectively #
###################################################################


alpha_tau <- sapply(Alpha_rma.list, FUN = function(x){
  sqrt(x$tau2)
})


alpha_I2 <- sapply(Alpha_rma.list, FUN = function(x){
  x$I2
})

alpha_het.sig <- sapply(Alpha_rma.list, FUN = function(x){
  x$QEp <= .05
})

alpha_est <- sapply(Alpha_rma.list, FUN = function(x){
  x$b[1]
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

B.alpha_est <- sapply(Bonett.Alpha_rma.list, FUN = function(x){
  x$b[1]
})

sum(!B.alpha_het.sig, na.rm = T)

violin_df <- data.frame(tau = c(alpha_tau[-7], B.alpha_tau),
                        I2 = c(alpha_I2[-7], B.alpha_I2),
                        sig = c(alpha_het.sig[-7], B.alpha_het.sig),
                        stat = as.factor(c(rep(1, length(alpha_tau[-7])), rep(2, length(alpha_tau[-7])))),
                        est = c(alpha_est[-7], B.alpha_est))




###########################################
# Violin Plots for I2 and tau over scales #
###########################################



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




# Plots without Bonett-Transformed estimates:

violin_I2_lonely <- violin_df[which(violin_df$stat == 1),] %>%
  ggplot() + 
  geom_violin(aes(x = stat, y = I2), fill = "transparent") +
  geom_boxplot(aes(x = stat, y = I2), width = .1, fill = "transparent") +
  geom_point(aes(x = stat, y = I2, shape = sig), colour =  "#f66963",
             position = position_jitter(w = 0.1, h = 0), size = 4, alpha = .7) +
  scale_x_discrete(labels = c("")) +
  scale_shape_manual(values = c(21, 16)) +
  theme(legend.position = "none",
        text = element_text(size = 17),
        panel.background = element_rect(fill = "transparent",
                                        colour = NA_character_), 
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "transparent"),
        panel.grid.minor.x = element_line(colour = "transparent"),
        panel.grid.minor.y = element_line(colour = "transparent"),
        axis.line.x = element_line(colour = "transparent"),
        axis.line.y = element_line(colour = "transparent"),
        axis.ticks.x = element_line(colour = "transparent"),
        axis.ticks.y = element_line(colour = "grey"),
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  labs(x = "", y = expression(~I^2), title = "")


v1 <- violin_df[which(violin_df$stat == 1),] %>%
  ggplot() + 
  geom_violin(aes(x = 0, y = tau), fill = "transparent") +
  geom_boxplot(aes(y = tau), width = .1, fill = "transparent") +
  geom_point(aes(x = 0, y = tau, shape = sig),  colour =  "#f66963",
             position = position_jitter(w = 0.1, h = 0), size = 4, alpha = .7) +
  scale_x_discrete(labels = c("")) +
  scale_shape_manual(values = c(21, 16)) +
  theme(legend.position = "none",
        text = element_text(size = 17),
        panel.background = element_rect(fill = "transparent",
                                        colour = NA_character_), 
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "transparent"),
        panel.grid.minor.x = element_line(colour = "transparent"),
        panel.grid.minor.y = element_line(colour = "transparent"),
        axis.line.x = element_line(colour = "transparent"),
        axis.line.y = element_line(colour = "transparent"),
        axis.ticks.x = element_line(colour = "transparent"),
        axis.ticks.y = element_line(colour = "grey"),
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  labs(x = "", y = expression(~tau), title = "") 


grid_lonely <- grid.arrange(violin_I2_lonely, v1, ncol = 2,
                            top=textGrob("Heterogeneity in Cronbach's Alpha", gp=gpar(fontsize=22)))

# ggsave(
#   plot = grid_lonely,
#   filename = here("Graphics/temp_ViolinPlots_lonely_Alpha.svg"),
#   bg = "transparent",
#   width = 6,
#   height = 6,
#   units = "in"
# )


svg(here("Graphics/temp_ViolinPlots_lonely_Alpha.svg"), bg = "transparent",
        width = 6, height = 6)

grid.arrange(violin_I2_lonely, v1, ncol = 2,
             top=textGrob("Heterogeneity in Cronbach's Alpha", gp=gpar(fontsize=22)))

dev.off()







########################################################
# Violin Plots for meta-analytic estimates over scales #
########################################################



v3 <- violin_df[which(violin_df$stat == 1),] %>%
  ggplot() + 
  geom_violin(aes(x = 0, y = est), fill = "transparent") +
  geom_boxplot(aes(y = est), width = .1, fill = "transparent") +
  geom_point(aes(x = 0, y = est), colour = gg_color_hue(2)[1], 
             position = position_jitter(w = 0.1, h = 0), size = 3, alpha = .7) +
  scale_x_discrete(labels = c("Untransformed")) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "transparent",
                                        colour = NA_character_), 
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "transparent"),
        axis.line.x = element_line(colour = "transparent"),
        axis.line.y = element_line(colour = "black"),
        axis.ticks.x = element_line(colour = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  labs(x = "", y = "") +
  scale_x_continuous(breaks = 0, labels = "Untransformed")

v4 <- violin_df[which(violin_df$stat == 2),] %>%
  ggplot() + 
  geom_violin(aes(x = 0, y = est), fill = "transparent") +
  geom_boxplot(aes(y = est), width = .1, fill = "transparent") +
  geom_point(aes(x = 0, y = est), colour = gg_color_hue(2)[2], 
             position = position_jitter(w = 0.1, h = 0), size = 3, alpha = .7) +
  scale_x_discrete(labels = c("Bonett-transformed")) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "transparent",
                                        colour = NA_character_), 
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "transparent"),
        axis.line.x = element_line(colour = "transparent"),
        axis.line.y = element_line(colour = "black"),
        axis.ticks.x = element_line(colour = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  labs(x = "", y = "") +
  scale_x_continuous(breaks = 0, labels = "Bonett-Transformed")


violin_estimates <- gridExtra::grid.arrange(v3, v4, ncol = 2, top = "Violin Plot - Reliability Estimates")



# plots without Bonett-transformed estimates


v3_lonely <- violin_df[which(violin_df$stat == 1),] %>%
  ggplot() + 
  geom_violin(aes(x = 0, y = est), fill = "transparent") +
  geom_boxplot(aes(y = est), width = .1, fill = "transparent") +
  geom_point(aes(x = 0, y = est), colour = gg_color_hue(2)[1], 
             position = position_jitter(w = 0.1, h = 0), size = 4, alpha = .7) +
  scale_x_discrete(labels = c("")) +
  theme(legend.position = "none",
        text = element_text(size = 17),
        panel.background = element_rect(fill = "transparent",
                                        colour = NA_character_), 
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "transparent"),
        panel.grid.minor.y = element_line(colour = "transparent"),
        panel.grid.minor.x = element_line(colour = "transparent"),
        axis.line.x = element_line(colour = "transparent"),
        axis.line.y = element_line(colour = "transparent"),
        axis.ticks.x = element_line(colour = "transparent"),
        axis.ticks.y = element_line(colour = "grey"),
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  labs(x = "", y = "Cronnbach's Alpha") 


violin_estimates_lonely <- gridExtra::grid.arrange(v3_lonely, top = textGrob("Reliability Estimates", gp=gpar(fontsize=22)))

svg(here("Graphics/temp_ViolinPlots_estimates_lonely_Alpha.svg"), bg = "transparent",
        width = 4, height = 4)

gridExtra::grid.arrange(v3_lonely, top = textGrob("Reliability Estimates", gp=gpar(fontsize=22)))

dev.off()

# ggsave(
#   plot = violin_estimates_lonely,
#   filename = here("Graphics/temp_ViolinPlots_estimates_lonely_Alpha.svg"),
#   bg = "transparent",
#   width = 4,
#   height = 4,
#   units = "in"
# )


####################################################################
# Scatterplots for Reliability estimates (X) and Heterogeneity (Y) #
####################################################################


violin_df[which(violin_df$stat == 1),] %>%
  ggplot(data = .) +
  geom_point(aes(x = est, y = tau, shape = sig), colour = gg_color_hue(2)[1],
             size = 3, alpha = .7) +
  scale_shape_manual(values = c(21, 16)) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "transparent",
                                        colour = NA_character_), 
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        axis.ticks.x = element_line(colour = "grey"),
        axis.ticks.y = element_line(colour = "grey"),
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  labs(x = "RMA-Est. Cronbach's Alpha", y = "Heterogeneity (tau)")



violin_df[which(violin_df$stat == 1),] %>%
  ggplot(data = .) +
  geom_point(aes(x = est, y = I2, shape = sig), colour = gg_color_hue(2)[1],
             size = 3, alpha = .7) +
  scale_shape_manual(values = c(21, 16)) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "transparent",
                                        colour = NA_character_), 
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        axis.ticks.x = element_line(colour = "grey"),
        axis.ticks.y = element_line(colour = "grey"),
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  labs(x = "RMA-Est. Cronbach's Alpha", y = "Heterogeneity (I2)")



violin_df[which(violin_df$stat == 2),] %>%
  ggplot(data = .) +
  geom_point(aes(x = est, y = tau, shape = sig), colour = gg_color_hue(2)[2],
             size = 3, alpha = .7) +
  scale_shape_manual(values = c(21, 16)) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "transparent",
                                        colour = NA_character_), 
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        axis.ticks.x = element_line(colour = "grey"),
        axis.ticks.y = element_line(colour = "grey"),
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  labs(x = "RMA-Est. Cronbach's Alpha (Bonett-Transformed)", y = "Heterogeneity (tau)")



violin_df[which(violin_df$stat == 2),] %>%
  ggplot(data = .) +
  geom_point(aes(x = est, y = I2, shape = sig), colour = gg_color_hue(2)[2],
             size = 3, alpha = .7) +
  scale_shape_manual(values = c(21, 16)) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "transparent",
                                        colour = NA_character_), 
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        axis.ticks.x = element_line(colour = "grey"),
        axis.ticks.y = element_line(colour = "grey"),
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  labs(x = "RMA-Est. Cronbach's Alpha (Bonett-Transformed)", y = "Heterogeneity (I2)")












##############################################################################################
# Combination of Forest Plot and Histogram for Reliability estimates, HEXACO HH specifically #
##############################################################################################


my_forest_plot <- function(rma.fit, rma.data, main.title = "Forest Plot", 
                           x.lab = "Estimate", ci.lvl = .975, CI.display = FALSE){
  
  # Calculate lower and upper limits of confidence levels, for each replication's estimate
  cil <- rma.fit$yi[1:length(rma.fit$yi)] - qnorm(ci.lvl)*sqrt(rma.fit$vi[1:length(rma.fit$vi)])
  ciu <- rma.fit$yi[1:length(rma.fit$yi)] + qnorm(ci.lvl)*sqrt(rma.fit$vi[1:length(rma.fit$vi)])
  
  
  # weights <- 1/sqrt(rma.fit$vi+rma.fit$tau2)
  # weights.scaled <- weights/mean(weights)
  # weights.rescaled <- weights.scaled/mean(weights.scaled)
  
  p <- ggplot() + # initialize ggplot
    
    # plot point estimates
    geom_point(aes(x = rma.fit$yi, y = c(5:(length(rma.fit$yi)+4))
                   #                , size = weights.rescaled
    ), shape = 15) + 
    
    # vertical line at x = 0
    # geom_vline(xintercept = 0, linetype = "dashed") +
    
    # add horizontal line for CI of each replication's estimate
    geom_segment(aes(x = cil, y = c(5:(length(rma.fit$yi)+4)), xend = ciu, yend = c(5:(length(rma.fit$yi)+4)))) +
    
    # ggplot theme
    theme_minimal() +
    
    # plot meta analytic point estimate
    geom_point(aes(x = rma.fit$b[1], y = 1), shape = 18) +
    
    #add CI-line for meta-analytic point estimate
    geom_segment(aes(x = rma.fit$b[1] - qnorm(ci.lvl)*rma.fit$se, y = 1, 
                     xend = rma.fit$b[1] + qnorm(ci.lvl)*rma.fit$se, yend = 1)) +
    
    # add vertical upper & lower-limit "fence"-lines, for each replication's estimate
    geom_segment(aes(x = cil, xend = cil, y = (c(5:(length(rma.fit$yi)+4))+.3), yend = (c(5:(length(rma.fit$yi)+4))-.3) )) +
    geom_segment(aes(x = ciu, xend = ciu, y = (c(5:(length(rma.fit$yi)+4))+.3), yend = (c(5:(length(rma.fit$yi)+4))-.3) )) +
    
    # add vertical upper- & lower-limit "fence lines, for meta-analytic point estimate
    geom_segment(aes(x = rma.fit$b[1] - qnorm(ci.lvl)*rma.fit$se, y = (1+.3), 
                     xend = rma.fit$b[1] - qnorm(ci.lvl)*rma.fit$se, yend = (1-.3))) +
    geom_segment(aes(x = rma.fit$b[1] + qnorm(ci.lvl)*rma.fit$se, y = (1+.3), 
                     xend = rma.fit$b[1] + qnorm(ci.lvl)*rma.fit$se, yend = (1-.3))) +
    
    
    
    
    # labs & titles
    xlab(x.lab) +
    ylab("Lab") # +
    # ggtitle(main.title)
  
  if(CI.display){
    p <- p + 
      scale_y_continuous(breaks = c(1, (5:(length(rma.fit$yi)+4))), 
                         labels = c("RE Model", unique(as.character(rma.data$source))),
                         
                         sec.axis = dup_axis(breaks = c(1, (5:(length(rma.fit$yi)+4))),
                                             labels = c(paste0("[", round(rma.fit$b[1] - qnorm(ci.lvl)*rma.fit$se, 2), ";", round(rma.fit$b[1] + qnorm(ci.lvl)*rma.fit$se, 2), "]"), 
                                                        paste0("[", round(cil, 2), ";", round(ciu, 2), "]")),
                                             name = ""))
    # p <- p + geom_text(aes(y = c(5:(length(rma.fit$yi)+4)), x = (max(ciu) + abs(max(ciu))*.05),
    #               label = paste0("[", round(cil, 2), ";", round(ciu, 2), "]")))
  }else{
    p <- p +     # adjust labels on y-axis, to display lab-abreviations
      scale_y_continuous(breaks = c(1, (5:(length(rma.fit$yi)+4))), labels = c("RE Model", unique(as.character(rma.data$source)))                         ) 
  }
  
  p
}




laymat <- matrix(c(1, 1, 1, 1, 2, 2, 2,
                   1, 1, 1, 1, 2, 2, 2,
                   1, 1, 1, 1, 2, 2, 2,
                   1, 1, 1, 1, 2, 2, 2, 
                   1, 1, 1, 1, 2, 2, 2), byrow = T, ncol = 7)



AE <- Alpha_estimates.list[[27]]
AR <- Alpha_rma.list[[27]]


p <- my_forest_plot(rma.data = AE, rma.fit = AR, main.title = NULL,
                    x.lab = "Cronbach's Alpha", ci.lvl = .975, CI.display = FALSE)

p <- p + theme(panel.background = element_rect(fill = "transparent",
                                               colour = NA_character_), 
               text = element_text(size = 17),
               panel.grid.major.y = element_line(colour = "grey"),
               panel.grid.major.x = element_line(colour = "grey"),
               panel.grid.minor.y = element_line(colour = "transparent"),
               panel.grid.minor.x = element_line(colour = "transparent"),
               axis.line.x = element_line(colour = "transparent"),
               axis.line.y = element_line(colour = "transparent"),
               axis.ticks.x = element_line(colour = "transparent"),
               axis.ticks.y = element_line(colour = "grey"),
               plot.background = element_rect(fill = "transparent", colour = NA),
               axis.text.y = element_text(size = 8)) 
  


bw_FD <- (2 * IQR(AE$Reliability))/length(AE$Reliability)^(1/3)

h <- ggplot(AE) +
  geom_histogram(aes(x = Reliability), 
                 binwidth = bw_FD,
                 colour = "black", fill = "#f66963") +
  theme(panel.background = element_rect(fill = "transparent",
                                        colour = NA_character_), 
        text = element_text(size = 17),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "transparent"),
        panel.grid.minor.y = element_line(colour = "transparent"),
        panel.grid.minor.x = element_line(colour = "transparent"),
        axis.line.x = element_line(colour = "transparent"),
        axis.line.y = element_line(colour = "transparent"),
        axis.ticks.x = element_line(colour = "transparent"),
        axis.ticks.y = element_line(colour = "grey"),
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  labs(x = "Cronbach's Alpha", y = "Frequency")




Forest_Histogram_HH <- gridExtra::grid.arrange(p, h, layout_matrix = laymat, 
                                               top = textGrob("Reliability Estimates - HEXACO HH", gp=gpar(fontsize=22)))


ggsave(
  plot = Forest_Histogram_HH,
  filename = here("Graphics/temp_Forest_Histogram.svg"),
  bg = "transparent",
  width = 8.5,
  height = 6.5,
  units = "in"
)



## All Scales


substr(names(data.list),
       (regexpr("Project) Data/", names(data.list)) + 14),
       (nchar(names(data.list))-4))

unique(substr(scales_meta$Scale, 1, regexpr(" ", scales_meta$Scale) - 1))
length(unique(substr(scales_meta$Scale, 1, regexpr(" ", scales_meta$Scale) - 1)))

scales_meta <- read.csv(here("Notes/Tutzing_Vis_Scales.csv"))
sum(scales_meta$Psychometrics)


