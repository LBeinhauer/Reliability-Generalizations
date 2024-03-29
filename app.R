#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# library loading and installing as necessary
# relevant R packages
packages <- c("metafor", "tidyverse", "here", "data.table", "lavaan", "gridExtra", "shiny", "grid")

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


Alpha_rma.list <- readRDS(here("Data/Shiny Data/Alpha_rma.list.RData"))[-7]

Bonett.Alpha_rma.list <- readRDS(here("Data/Shiny Data/Bonett.Alpha_rma.list.RData"))

Omega_rma.list <- readRDS(here("Data/Shiny Data/Omega_rma.list.RData"))

Bonett.Omega_rma.list <- readRDS(here("Data/Shiny Data/Bonett.Omega_rma.list.RData"))


Reliability_estimates_paths <- list.files(here("Data/Reliability Estimates"), full.names =  TRUE)

Alpha_estimates.list <- Reliability_estimates_paths[grep("_Alpha.csv$", Reliability_estimates_paths)][-7] %>%
    lapply(., read.csv)

names(Alpha_estimates.list) <- substr(Reliability_estimates_paths[grep("_Alpha.csv$", Reliability_estimates_paths)][-7],
                                      (regexpr("Reliability Estimates/", Reliability_estimates_paths[grep("_Alpha.csv$", Reliability_estimates_paths)][-7]) + 22),
                                      (nchar(Reliability_estimates_paths[grep("_Alpha.csv$", Reliability_estimates_paths)][-7])-10))


Omega_estimates.list <- Reliability_estimates_paths[grep("_Omega.csv$", Reliability_estimates_paths)] %>%
    lapply(., read.csv)

names(Omega_estimates.list) <- substr(Reliability_estimates_paths[grep("_Omega.csv$", Reliability_estimates_paths)],
                                      (regexpr("Reliability Estimates/", Reliability_estimates_paths[grep("_Omega.csv$", Reliability_estimates_paths)]) + 22),
                                      (nchar(Reliability_estimates_paths[grep("_Omega.csv$", Reliability_estimates_paths)])-10))


Bonett.Alpha_estimates.list <- Reliability_estimates_paths[grep("_Bonett-Alpha.csv$", Reliability_estimates_paths)][-7] %>%
    lapply(., read.csv)

names(Bonett.Alpha_estimates.list) <- substr(Reliability_estimates_paths[grep("_Bonett-Alpha.csv$", Reliability_estimates_paths)][-7],
                                             (regexpr("Reliability Estimates/", Reliability_estimates_paths[grep("_Bonett-Alpha.csv$", Reliability_estimates_paths)][-7]) + 22),
                                             (nchar(Reliability_estimates_paths[grep("_Bonett-Alpha.csv$", Reliability_estimates_paths)][-7])-17))


Bonett.Omega_estimates.list <- Reliability_estimates_paths[grep("_Bonett-Omega.csv$", Reliability_estimates_paths)][-7] %>%
    lapply(., read.csv)

names(Bonett.Omega_estimates.list) <- substr(Reliability_estimates_paths[grep("_Bonett-Omega.csv$", Reliability_estimates_paths)][-7],
                                             (regexpr("Reliability Estimates/", Reliability_estimates_paths[grep("_Bonett-Omega.csv$", Reliability_estimates_paths)][-7]) + 22),
                                             (nchar(Reliability_estimates_paths[grep("_Bonett-Omega.csv$", Reliability_estimates_paths)][-7])-17))



data.list <- list.files(here("Data/Extracted (Project) Data"), full.names = TRUE)[-7] %>%
    sapply(., read.csv)

names(data.list) <- substr(list.files(here("Data/Extracted (Project) Data"), full.names = FALSE)[-7],
                           1, (nchar(list.files(here("Data/Extracted (Project) Data"), full.names = FALSE)[-7])-4))


proj.names <- names(data.list)

varT_rma.list <- readRDS(file = here("Notes/bootstrapped_varT_rma.RData"))[-7]

varE_rma.list <- readRDS(file = here("Notes/bootstrapped_varE_rma.RData"))[-7]


scales_meta <- read.csv(here("Notes/Tutzing_Vis_Scales.csv"))[-7,]


# Define UI for application that allows for different visualisations of reliability generalization
#  studies
ui <- navbarPage(
    
    "Reliability Generalization",
    
    tabPanel(
        "Histograms",
        
        sidebarPanel(
            
            selectInput(inputId = "ScaleHG",
                        label = "Scale of Interest",
                        choices = proj.names),
            
            selectInput(inputId = "RelMeasHG",
                        label = "Reliability Measure",
                        choices = c("Cronbach's Alpha",
                                    "McDonald's Omega",
                                    "Bonett-transf. Alpha",
                                    "Bonett-transf. Omega"))
        ),
        
        mainPanel(
            h4("Histograms for Selected Scale"),
            plotOutput(outputId = "histogram")
        )
    ),
    
    tabPanel(
        
        "Individual Forest Plots",
        
        sidebarPanel(
            
            selectInput(inputId = "ScaleFP",
                        label = "Scale of Interest",
                        choices = proj.names),
            
            selectInput(inputId = "RelMeasFP",
                        label = "Reliability Measure",
                        choices = c("Cronbach's Alpha",
                                    "McDonald's Omega",
                                    "Bonett-transf. Alpha",
                                    "Bonett-transf. Omega")),
            
            checkboxInput(inputId = "CI.bool", 
                          label = "Display Confidence-Interval",
                          value = FALSE)
            
        ),
        
        mainPanel(
            h4("Forest Plots for Selected Scale of Interest & Reliability Measure"),
            plotOutput(outputId = "forsplot")
        )
        
    ),
    
    tabPanel(
        "Random-Effects MA-Model",
        
        sidebarPanel(
            
            selectInput(inputId = "ScaleRMA",
                        label = "Scale of Interest",
                        choices = proj.names),
            
            selectInput(inputId = "RelMeasRMA",
                        label = "Reliability Measure",
                        choices = c("Cronbach's Alpha", 
                                    "McDonald's Omega",
                                    "Bonett-transf. Alpha",
                                    "Bonett-transf. Omega"))
        ),
        
        mainPanel(
            h4("Random Effects Meta-Analytic Models"),
            plotOutput(outputId = "REMA")
        )
    ),
    
    tabPanel(
        "Violin Plots Heterogeneity",
        
        sidebarPanel(
            selectInput(inputId = "HetEst",
                        label = "Heterogeneity Estimate",
                        choices = c("I2",
                                    "tau")),
            
            checkboxInput(inputId = "Sig.bool",
                          label = "Indicate Significance",
                          value = FALSE)
        ),
        
        mainPanel(
            h4("Violin Plots for Heterogeneity"),
            plotOutput(outputId = "violplot", height = "600px", width = "600px")
        )
        
        
    ), 
    
    tabPanel(
        "Forest Plots - Variance deconstruction",
        
        sidebarPanel(
            selectInput(inputId = "ScaleVD",
                        label = "Scale of Interest",
                        choices = proj.names),
            
            checkboxInput(inputId = "CI.boolVD", 
                          label = "Display Confidence-Interval",
                          value = FALSE)
        ),
        
        mainPanel(
            h4("Forest Plots - Variance Decomposition"),
            plotOutput(outputId = "VDforsplot", height = "600px", width = "600px")
        )
    ),
    
    tabPanel(
        "Violin Plots - Split",
        
        sidebarPanel(
            
            selectInput(inputId = "Stat_AP",
                        label = "Statistic of Interest",
                        choices = c("Reliability estimates",
                                    "Heterogeneity of reliability")),
            
            selectInput(inputId = "Split_AP",
                        label = "Split by",
                        choices = c("Psychometric vs ad hoc",
                                    "Scale use")),
            
            uiOutput("UI_ad_hoc"),
            
            uiOutput("UI_ScaleUse")
            
        ),
        
        mainPanel(
            h4("Violin Plots comparing ad hoc and psychometrically assessed"),
            plotOutput(outputId = "violplotAP", height = "600px", width = "600px")
        )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {

    output$forsplot <- renderPlot({
        
        if(input$RelMeasFP == "Cronbach's Alpha"){
            data <- Alpha_rma.list[[which(names(Alpha_rma.list) %in% input$ScaleFP)]]
        }
        if(input$RelMeasFP == "McDonald's Omega"){
            data <- Omega_rma.list[[which(names(Omega_rma.list) %in% input$ScaleFP)]]
        }
        if(input$RelMeasFP == "Bonett-transf. Alpha"){
            data <- Bonett.Alpha_rma.list[[which(names(Bonett.Alpha_rma.list) %in% input$ScaleFP)]]
        }
        if(input$RelMeasFP == "Bonett-transf. Omega"){
            data <- Bonett.Omega_rma.list[[which(names(Bonett.Omega_rma.list) %in% input$ScaleFP)]]
        }
       
        
        
        forsplot <- my_forest_plot(rma.fit = data, rma.data = data.list[[which(names(data.list) %in% input$ScaleFP)]], 
                                   main.title = paste0("Forest Plot - ", input$ScaleFP),
                                   x.lab = input$RelMeasFP, CI.display = input$CI.bool)
        
        print(forsplot)
        
    })
    
    
    
    output$histogram <- renderPlot({
        
        if(input$RelMeasHG == "Cronbach's Alpha"){
            data <- Alpha_estimates.list[[which(names(Alpha_estimates.list) %in% input$ScaleHG)]]
        }
        if(input$RelMeasHG == "McDonald's Omega"){
            data <- Omega_estimates.list[[which(names(Omega_estimates.list) %in% input$ScaleHG)]]
        }
        if(input$RelMeasHG == "Bonett-transf. Alpha"){
            data <- Bonett.Alpha_estimates.list[[which(names(Bonett.Alpha_estimates.list) %in% input$ScaleHG)]]
        }
        if(input$RelMeasHG == "Bonett-transf. Omega"){
            data <- Bonett.Omega_estimates.list[[which(names(Bonett.Omega_estimates.list) %in% input$ScaleHG)]]
        }
        
        
        bw_FD <- (2 * IQR(data$Reliability))/length(data$Reliability)^(1/3)
        
        bplot <- ggplot(data = data, aes(x = Reliability)) + 
            geom_histogram(binwidth = bw_FD, fill = "Orange", color = "black") +
            labs(title = paste0("Histogram ", input$ScaleHG), y = "Frequency", 
                 x = input$RelMeasHG)
        
        bplot
        
    })
    
    
    output$REMA <- renderPlot({
        
        if(input$RelMeasRMA == "Cronbach's Alpha"){
            data <- Alpha_rma.list[[which(names(Alpha_rma.list) %in% input$ScaleRMA)]]
        }
        if(input$RelMeasRMA == "McDonald's Omega"){
            data <- Omega_rma.list[[which(names(Omega_rma.list) %in% input$ScaleRMA)]]
        }
        if(input$RelMeasRMA == "Bonett-transf. Alpha"){
            data <- Bonett.Alpha_rma.list[[which(names(Bonett.Alpha_rma.list) %in% input$ScaleRMA)]]
        }
        if(input$RelMeasRMA == "Bonett-transf. Omega"){
            data <- Bonett.Omega_rma.list[[which(names(Bonett.Omega_rma.list) %in% input$ScaleRMA)]]
        }
        
        
        t1 <- textGrob(paste0(input$ScaleRMA, " - ", input$RelMeasRMA, "\n \n",
                             "Meta-Analytic Estimate: ", round(data$b[1], 3), "\n \n",
                             "Heterogeneity: ", "\n",
                             "tau: ", round(sqrt(data$tau2), 3), "\n",
                             "I2: ", round(data$I2, 2), "\n",
                             "Q_E(df = ", data$k-1 ,") = ", round(data$QE), ", ", 
                             if(data$QEp < .0001){"<.0001"}else{round(data$QEp, 4)},
                             if(data$QEp < .05){" *"}else{""}, "\n \n" ,
                             "Prediction Interval: [", round(data$b[1] - 1.96*(sqrt(data$tau2 + data$se^2)), 2), " ; ",
                             round(data$b[1] + 1.96*(sqrt(data$tau2 + data$se^2)), 2), "]"),
                       gp = gpar(col = "black", fontsize = 20)
                       )
        
        grid.arrange(t1)
        
        
        
    })
    
    
    output$violplot <- renderPlot({
        
        set.seed(210622)
        
        alpha_tau <- sapply(Alpha_rma.list, FUN = function(x){
            sqrt(x$tau2)
        })
        
        
        alpha_I2 <- sapply(Alpha_rma.list, FUN = function(x){
            x$I2
        })
        
        alpha_het.sig <- sapply(Alpha_rma.list, FUN = function(x){
            x$QEp <= .05
        })
        

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
        
        
        
        
        omega_tau <- sapply(Omega_rma.list, FUN = function(x){
            sqrt(x$tau2)
        })
        
        
        omega_I2 <- sapply(Omega_rma.list, FUN = function(x){
            x$I2
        })
        
        omega_het.sig <- sapply(Omega_rma.list, FUN = function(x){
            x$QEp <= .05
        })
        
        
        B.omega_tau <- sapply(Bonett.Omega_rma.list, FUN = function(x){
            sqrt(x$tau2)
        })
        
        # 1-exp(B.omega_tau)
        
        B.omega_I2 <- sapply(Bonett.Omega_rma.list, FUN = function(x){
            x$I2
        })
        
        B.omega_het.sig <- sapply(Bonett.Omega_rma.list, FUN = function(x){
            x$QEp <= .05
        })
        

        violin_df <- data.frame(tau = c(alpha_tau, B.alpha_tau, omega_tau, B.omega_tau),
                                I2 = c(alpha_I2, B.alpha_I2, omega_I2, B.omega_I2),
                                sig = c(alpha_het.sig, B.alpha_het.sig, omega_het.sig, B.omega_het.sig),
                                stat = as.factor(c(rep(0, length(alpha_tau)), rep(0, length(B.alpha_tau)),
                                                   rep(1, length(omega_tau)), rep(1, length(B.omega_tau)))),
                                transf = as.factor(c(rep(0, length(alpha_tau)), rep(1, length(B.alpha_tau)),
                                                     rep(0, length(omega_tau)), rep(1, length(B.omega_tau)))))
        
        
        
        
        if(input$HetEst == "I2"){
            
            row.labs <- c("untransformed", "Bonett-transf.")
            names(row.labs) <- c("0", "1")
            col.labs <- c("Cronbach's Alpha", "McDonald's Omega")
            names(col.labs) <- c("0", "1")
            
            vplot <- violin_df %>%
                ggplot() + 
                facet_grid(rows = vars(transf), cols = vars(stat), scales = "free",
                           labeller = labeller(transf = row.labs, stat = col.labs)) +
                geom_violin(aes(x = 1, y = I2), fill = "transparent") +
                geom_boxplot(aes(x = 1, y = I2), width = .1, fill = "transparent") +
                scale_x_discrete(labels = c("Untransformed", "Bonett-Transformed")) +
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
            
            if(input$Sig.bool){
                vplot <- vplot + 
                    geom_point(aes(x = 1, y = I2, colour = stat, shape = sig), 
                               position = position_jitter(w = 0.1, h = 0), size = 3, alpha = .7) +
                    scale_shape_manual(values = c(21, 16)) 
            }else{
                vplot <- vplot + 
                    geom_point(aes(x = 1, y = I2, colour = stat), shape = 16,
                               position = position_jitter(w = 0.1, h = 0), size = 3, alpha = .7)
            }
            
            
        }
        
        
            
            
        
        if(input$HetEst == "tau"){
            
            row.labs <- c("untransformed", "Bonett-transf.")
            names(row.labs) <- c("0", "1")
            col.labs <- c("Cronbach's Alpha", "McDonald's Omega")
            names(col.labs) <- c("0", "1")
            
            vplot <- violin_df %>%
                ggplot() + 
                facet_grid(rows = vars(transf), cols = vars(stat), scales = "free",
                           labeller = labeller(transf = row.labs, stat = col.labs)) +
                geom_violin(aes(x = 0, y = tau), fill = "transparent") +
                geom_boxplot(aes(y = tau), width = .1, fill = "transparent") +
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
                labs(x = "", y = "tau") +
                scale_x_continuous(breaks = 0, labels = "Untransformed")
            
            
            
            if(input$Sig.bool){
                vplot <- vplot  + 
                    geom_point(aes(x = 0, y = tau, shape = sig, colour = stat), 
                                      position = position_jitter(w = 0.1, h = 0), size = 3, alpha = .7) +
                    scale_shape_manual(values = c(21, 16)) 
            }else{
                vplot <- vplot + 
                    geom_point(aes(x = 0, y = tau, colour = stat), 
                               position = position_jitter(w = 0.1, h = 0), size = 3, alpha = .7) 
            }
            
            
        }
        
        vplot
    })
    
    output$VDforsplot <- renderPlot({
        
        laymat <- matrix(c(1, 1, 1, 1, 1, 3, 3,
                           1, 1, 1, 1, 1, 3, 3,
                           1, 1, 1, 1, 1, 3, 3,
                           1, 1, 1, 1, 1, 3, 3,
                           2, 2, 2, 2, 2, 4, 4, 
                           2, 2, 2, 2, 2, 4, 4,
                           2, 2, 2, 2, 2, 4, 4,
                           2, 2, 2, 2, 2, 4, 4), byrow = T, ncol = 7)
        
        dl.index <- which(proj.names %in% input$ScaleVD)
        
        
        dl <- data.list[[dl.index]]
        vT <- varT_rma.list[[dl.index]]
        
        vE <- varE_rma.list[[dl.index]]
        
        
        pT <- my_forest_plot(rma.fit = vT, rma.data = dl,
                             main.title = paste0("Forest Plot - ", input$ScaleVD),
                             x.lab = "True Variance (estimated)", ci.lvl = .975, CI.display = input$CI.boolVD)
        
        pE <- my_forest_plot(rma.fit = vE, rma.data = dl,
                             main.title = paste0("Forest Plot - ", input$ScaleVD),
                             x.lab = "Error Variance (estimated)", ci.lvl = .975, CI.display = input$CI.boolVD)
        
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
        
        
        VDforsplot <- grid.arrange(pT, pE, tT, tE, layout_matrix = laymat)
    
    
        
        VDforsplot
        
    })
    
    
    output$UI_ad_hoc <- renderUI({
        if(input$Stat_AP == "Reliability estimates"){
            
        }
        
        if(input$Stat_AP == "Heterogeneity of reliability"){
            sidebarPanel(
                selectInput(inputId = "HetEst_AP",
                            label = "Heterogeneity Estimate",
                            choices = c("I2",
                                        "tau")),
                
                checkboxInput(inputId = "Sig.bool_AP",
                              label = "Indicate Significance",
                              value = FALSE),
                
                width = 8
            )
            
            
        }
    })
    
    
    output$UI_ScaleUse <- renderUI({
        if(input$Split_AP == "Psychometric vs ad hoc"){
            
        }
        
        if(input$Split_AP == "Scale use"){
            sidebarPanel(
                selectInput(inputId = "Merge_AP",
                            label = "Merge Auxiliary, Filler & Manip Check?",
                            choices = c("Yes",
                                        "No")),
                
                width = 8
            )
            
            
        }
    })
    
    
   
    
    
    output$violplotAP <- renderPlot({
        
        set.seed(210622)
        
        alpha_tau <- sapply(Alpha_rma.list, FUN = function(x){
            sqrt(x$tau2)
        })
        
        
        alpha_I2 <- sapply(Alpha_rma.list, FUN = function(x){
            x$I2
        })
        
        alpha_het.sig <- sapply(Alpha_rma.list, FUN = function(x){
            x$QEp <= .05
        })
        
        
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
        
        alpha_est <- sapply(Alpha_rma.list, FUN = function(x){
            x$b[1]
        })
        
        B.alpha_est <- 1 - exp(sapply(Bonett.Alpha_rma.list, FUN = function(x){
            x$b[1]
        }))
        
        
        # Inbar is not available for Bonett-estimates
        scales_meta2 <- rbind(scales_meta, scales_meta[-grep("Inbar", scales_meta$Scale),])
        
        violin_df2 <- data.frame(estimate = c(alpha_tau, B.alpha_tau, alpha_I2, B.alpha_I2),
                                 stat = as.factor(rep(c(rep(0, length(alpha_tau)), rep(1, length(B.alpha_tau))), 2)),
                                 sig = rep(c(alpha_het.sig, B.alpha_het.sig), 2),
                                 Psychometrics = as.factor(rep(scales_meta2$Psychometrics, 2)),
                                 tauI2 = c(rep(0, length(alpha_tau) + length(B.alpha_tau)), rep(1, length(alpha_I2) + length(B.alpha_I2))),
                                 Use = rep(scales_meta2$Use, 2),
                                 scale = rep(scales_meta2$Scale, 2))
        
        
        if(input$Split_AP == "Psychometric vs ad hoc"){
            violin_df2$SplitVar <- violin_df2$Psychometrics
        }
        if(input$Split_AP == "Scale use"){
            if(input$Merge_AP == "No"){
                violin_df2$SplitVar <- violin_df2$Use
            }
            
            if(input$Merge_AP == "Yes"){
                violin_df2$SplitVar <- violin_df2$Use
                
                violin_df2$SplitVar[violin_df2$SplitVar %in% c("IV", "MC", "Fil")] <- "other"
                
                violin_df2$SplitVar <- as.factor(violin_df2$SplitVar)
            }
            
        }
        
        
        if(input$Stat_AP == "Heterogeneity of reliability"){
            if(input$HetEst_AP == "I2"){
                
                row.labs <- c("Untransformed", "Bonett-transformed")
                names(row.labs) <- c("0", "1")
                
                if(input$Split_AP == "Psychometric vs ad hoc"){
                    col.labs <- c("ad hoc constructed", "Psychometrically dev.")
                    names(col.labs) <- c("0", "1")
                }
                if(input$Split_AP == "Scale use"){
                    
                    if(input$Merge_AP == "No"){
                        col.labs <- c("Dependent Variable", "Independent Variable", "Auxiliary Variable",
                                      "Manipulation Check", "Filler")
                        names(col.labs) <- c("DV", "IV", "Aux", "MC", "Fil")
                    }
                    
                    if(input$Merge_AP == "Yes"){
                        col.labs <- c("Dependent Variable", "Auxiliary Variable",
                                      "Other")
                        names(col.labs) <- c("DV", "Aux", "other")
                    }
                    
                }
                
                
                
                vplot <- violin_df2[which(violin_df2$tauI2 == 1),] %>%
                    ggplot(., aes(x = 0, y = estimate)) + 
                    geom_violin() +
                    geom_boxplot(width = .1) +
                    facet_grid(rows = vars(stat), cols = vars(SplitVar), 
                               labeller = labeller(stat = row.labs, SplitVar = col.labs)) +
                    theme(legend.position = "none",
                          panel.background = element_rect(fill = "transparent",
                                                          colour = NA_character_), 
                          panel.grid.major.y = element_line(colour = "grey"),
                          panel.grid.major.x = element_line(colour = "transparent"),
                          axis.line.x = element_line(colour = "transparent"),
                          axis.line.y = element_line(colour = "black"),
                          axis.ticks.x = element_line(colour = "transparent"),
                          plot.background = element_rect(fill = "transparent", colour = NA),
                          axis.text.x = element_blank()) +
                    labs(y = expression("I"^2), x = "")
                
                
                if(input$Sig.bool_AP){
                    vplot <- vplot + 
                        geom_point(aes(x = 0, y = estimate, colour = stat, shape = sig), 
                                   position = position_jitter(w = 0.1, h = 0), size = 3, alpha = .7) +
                        scale_shape_manual(values = c(21, 16)) 
                    
                }else{
                    vplot <- vplot + 
                        geom_point(aes(x = 0, y = estimate, colour = stat), shape = 16,
                                   position = position_jitter(w = 0.1, h = 0), size = 3, alpha = .7)
                }
                
                print(vplot) 
            }
            
            
            if(input$HetEst_AP == "tau"){
                
                row.labs <- c("Untransformed", "Bonett-transformed")
                names(row.labs) <- c("0", "1")
                
                if(input$Split_AP == "Psychometric vs ad hoc"){
                    col.labs <- c("ad hoc constructed", "Psychometrically dev.")
                    names(col.labs) <- c("0", "1")
                }
                if(input$Split_AP == "Scale use"){
                    
                    if(input$Merge_AP == "No"){
                        col.labs <- c("Dependent Variable", "Independent Variable", "Auxiliary Variable",
                                      "Manipulation Check", "Filler")
                        names(col.labs) <- c("DV", "IV", "Aux", "MC", "Fil")
                    }
                    
                    if(input$Merge_AP == "Yes"){
                        col.labs <- c("Dependent Variable", "Auxiliary Variable",
                                      "Other")
                        names(col.labs) <- c("DV", "Aux", "other")
                    }
                    
                }
                
                vplot <- violin_df2[which(violin_df2$tauI2 == 0),] %>%
                    ggplot(., aes(x = 0, y = estimate)) + 
                    geom_violin() +
                    geom_boxplot(width = .1) +
                    facet_grid(rows = vars(stat), cols = vars(SplitVar), scales = "free", 
                               labeller = labeller(stat = row.labs, SplitVar = col.labs)) +
                    theme(legend.position = "none",
                          panel.background = element_rect(fill = "transparent",
                                                          colour = NA_character_), 
                          panel.grid.major.y = element_line(colour = "grey"),
                          panel.grid.major.x = element_line(colour = "transparent"),
                          axis.line.x = element_line(colour = "transparent"),
                          axis.line.y = element_line(colour = "black"),
                          axis.ticks.x = element_line(colour = "transparent"),
                          plot.background = element_rect(fill = "transparent", colour = NA),
                          axis.text.x = element_blank()) +
                    labs(y = expression("tau"), x = "")
                
                
                if(input$Sig.bool_AP){
                    vplot <- vplot + 
                        geom_point(aes(x = 0, y = estimate, colour = stat, shape = sig), 
                                   position = position_jitter(w = 0.1, h = 0), size = 3, alpha = .7) +
                        scale_shape_manual(values = c(21, 16)) 
                    
                }else{
                    vplot <- vplot + 
                        geom_point(aes(x = 0, y = estimate, colour = stat), shape = 16,
                                   position = position_jitter(w = 0.1, h = 0), size = 3, alpha = .7)
                }
                
                print(vplot)
            }
             
        }
        
        if(input$Stat_AP == "Reliability estimates"){
            violin_df2$reliability <- rep(c(alpha_est, B.alpha_est), 2)
            
            row.labs <- c("Untransformed", "Bonett-transformed")
            names(row.labs) <- c("0", "1")
            
            if(input$Split_AP == "Psychometric vs ad hoc"){
                col.labs <- c("ad hoc constructed", "Psychometrically dev.")
                names(col.labs) <- c("0", "1")
            }
            if(input$Split_AP == "Scale use"){
                
                if(input$Merge_AP == "No"){
                    col.labs <- c("Dependent Variable", "Independent Variable", "Auxiliary Variable",
                                  "Manipulation Check", "Filler")
                    names(col.labs) <- c("DV", "IV", "Aux", "MC", "Fil")
                }
                
                if(input$Merge_AP == "Yes"){
                    col.labs <- c("Dependent Variable", "Auxiliary Variable",
                                  "Other")
                    names(col.labs) <- c("DV", "Aux", "other")
                }
                
            }
            
            vplot <- violin_df2[which(violin_df2$tauI2 == 0),] %>%
                ggplot(., aes(x = 0, y = reliability)) + 
                geom_violin() +
                geom_boxplot(width = .1) +
                facet_grid(rows = vars(stat), cols = vars(SplitVar), scales = "free", 
                           labeller = labeller(stat = row.labs, SplitVar = col.labs)) +
                theme(legend.position = "none",
                      panel.background = element_rect(fill = "transparent",
                                                      colour = NA_character_), 
                      panel.grid.major.y = element_line(colour = "grey"),
                      panel.grid.major.x = element_line(colour = "transparent"),
                      axis.line.x = element_line(colour = "transparent"),
                      axis.line.y = element_line(colour = "black"),
                      axis.ticks.x = element_line(colour = "transparent"),
                      plot.background = element_rect(fill = "transparent", colour = NA),
                      axis.text.x = element_blank()) +
                labs(y = expression("Reliability"), x = "") +
                geom_point(aes(x = 0, y = reliability, colour = stat),
                           position = position_jitter(w = .1, h = 0))
            
            print(vplot)
        }
        
        
        
    })
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
