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
packages <- c("metafor", "tidyverse", "here", "data.table", "lavaan", "gridExtra", "shiny")

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

Reliability_estimates_paths <- list.files(here("Data/Reliability Estimates"), full.names =  TRUE)

Alpha_estimates.list <- Reliability_estimates_paths[grep("_Alpha.csv$", Reliability_estimates_paths)][-7] %>%
    lapply(., read.csv)

Bonett.Alpha_estimates.list <- Reliability_estimates_paths[grep("_Bonett-Alpha.csv$", Reliability_estimates_paths)][-7] %>%
    lapply(., read.csv)

data.list <- list.files(here("Data/Extracted (Project) Data"), full.names = TRUE)[-7] %>%
    sapply(., read.csv)

proj.names <- substr(names(data.list),
                     (regexpr("Project) Data/", names(data.list)) + 14),
                     (nchar(names(data.list))-4))

varT_rma.list <- readRDS(file = here("Notes/bootstrapped_varT_rma.RData"))

varE_rma.list <- readRDS(file = here("Notes/bootstrapped_varE_rma.RData"))




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
                                    "Bonett-transf. Alpha"))
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
                                    "Bonett-transf. Alpha")),
            
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
                                    "Bonett-transf. Alpha"))
        ),
        
        mainPanel(
            h4("Random Effects Meta-Analytic Models"),
            verbatimTextOutput(outputId = "REMA")
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
            plotOutput(outputId = "violplot")
        )
        
        
    ), 
    
    tabPanel(
        "Forest Plots - Variance deconstruction",
        
        sidebarPanel(
            selectInput(inputId = "ScaleVD",
                        label = "Scale of Interest",
                        choices = proj.names)
        )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {

    output$forsplot <- renderPlot({
        
        dl.index <- which(proj.names %in% input$ScaleFP)
        
        if(input$RelMeasFP == "Cronbach's Alpha"){
            data <- Alpha_rma.list[[dl.index]]
        }
        if(input$RelMeasFP == "Bonett-transf. Alpha"){
            data <- Bonett.Alpha_rma.list[[dl.index]]
        }
        
        
        forsplot <- my_forest_plot(rma.fit = data, rma.data = data.list[[dl.index]], 
                                   main.title = paste0("Forest Plot - ", input$ScaleFP),
                                   x.lab = input$RelMeasFP, CI.display = input$CI.bool)
        
        print(forsplot)
        
    })
    
    
    
    output$histogram <- renderPlot({
        
        dl.index <- which(proj.names %in% input$ScaleHG)
        
        if(input$RelMeasHG == "Cronbach's Alpha"){
            data <- Alpha_estimates.list[[dl.index]]
        }
        if(input$RelMeasHG == "Bonett-transf. Alpha"){
            data <- Bonett.Alpha_estimates.list[[dl.index]]
        }
        
        bw_FD <- (2 * IQR(data$Reliability))/length(data$Reliability)^(1/3)
        
        bplot <- ggplot(data = data, aes(x = Reliability)) + 
            geom_histogram(binwidth = bw_FD, fill = "Orange", color = "black") +
            labs(title = paste0("Histogram ", input$ScaleHG), y = "Frequency", 
                 x = input$RelMeasHG)
        
        bplot
        
    })
    
    
    output$REMA <- renderText({
        dl.index <- which(proj.names %>% input$ScaleHG)
        
        if(input$RelMeasRMA == "Cronbach's Alpha"){
            data <- Alpha_rma.list[[dl.index]]
        }
        if(input$RelMeasRMA == "Bonett-transf. Alpha"){
            data <- Bonett.Alpha_rma.list[[dl.index]]
        }
        
        
        data
        
        
        
    })
    
    
    output$violplot <- renderPlot({
        
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
        

        violin_df <- data.frame(tau = c(alpha_tau, B.alpha_tau),
                                I2 = c(alpha_I2, B.alpha_I2),
                                sig = c(alpha_het.sig, B.alpha_het.sig),
                                stat = as.factor(c(rep(1, length(alpha_tau)), rep(2, length(alpha_tau)))))
        
        
        
        
        if(input$HetEst == "I2"){
            
            vplot <- violin_df %>%
                ggplot() + 
                geom_violin(aes(x = stat, y = I2), fill = "transparent") +
                geom_boxplot(aes(x = stat, y = I2), width = .1, fill = "transparent") +
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
                    geom_point(aes(x = stat, y = I2, colour = stat, shape = sig), 
                               position = position_jitter(w = 0.1, h = 0), size = 3, alpha = .7) +
                    scale_shape_manual(values = c(21, 16)) 
            }else{
                vplot <- vplot + 
                    geom_point(aes(x = stat, y = I2, colour = stat), 
                               position = position_jitter(w = 0.1, h = 0), size = 3, alpha = .7)
            }
            
            
        }
        
        
            
            
        
        if(input$HetEst == "tau"){
            v1 <- violin_df[which(violin_df$stat == 1),] %>%
                ggplot() + 
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
            
            v2 <- violin_df[which(violin_df$stat == 2),] %>%
                ggplot() + 
                geom_violin(aes(x = 0, y = tau), fill = "transparent") +
                geom_boxplot(aes(y = tau), width = .1, fill = "transparent") +
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
                labs(x = "", y = "tau") +
                scale_x_continuous(breaks = 0, labels = "Bonett-Transformed")
            
            if(input$Sig.bool){
                v1 <- v1 + 
                    geom_point(aes(x = 0, y = tau, shape = sig), colour = gg_color_hue(2)[1], 
                                      position = position_jitter(w = 0.1, h = 0), size = 3, alpha = .7) +
                    scale_shape_manual(values = c(21, 16)) 
                
                v2 <- v2 + 
                    geom_point(aes(x = 0, y = tau, shape = sig), colour = gg_color_hue(2)[2], 
                               position = position_jitter(w = 0.1, h = 0), size = 3, alpha = .7) +
                    scale_shape_manual(values = c(21, 16))
            }else{
                v1 <- v1 + 
                    geom_point(aes(x = 0, y = tau), colour = gg_color_hue(2)[1], 
                               position = position_jitter(w = 0.1, h = 0), size = 3, alpha = .7) 
                v2 <- v2 + 
                    geom_point(aes(x = 0, y = tau), colour = gg_color_hue(2)[2], 
                                      position = position_jitter(w = 0.1, h = 0), size = 3, alpha = .7)
            }
            
            
            
                
            
            vplot <- gridExtra::grid.arrange(v1, v2, ncol = 2, top = "Violin Plot - tau estimates for Cronbach's Alpha")
            
        }
        
        vplot
    }
        
    )
    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
