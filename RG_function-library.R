### Reliability Generalization HEXACO ###

## 02/06/2022




###################################################################################################
# This script is used purely for data cleaning, initial manipulation and extraction to single     #
#  files per scale                                                                                #
# Raw data won't be made public, as long as no agreement from authors is obtained.                #
###################################################################################################


# library loading and installing as necessary

packages <- c("tidyverse", "here", "psych", "coefficientalpha", "spsUtil")

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
    
    spsUtil::quiet(psych::alpha(d))
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
    
    fit <- spsUtil::quiet(coefficientalpha::omega(d, se = TRUE, test = FALSE, silent = TRUE))
    
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
  
  # compute nr. of groups
  k <- length(unique(data$source))
  # identify "source"-colum
  s.idx <- grep("source", names(data))
  # compute nr. of items
  j <- length(names(data[,-s.idx]))
  # compute group-sample size
  n <- data %>%
    group_by(source) %>%
    summarise(n = n())
  n <- n$n
  
  est <- apply(as.matrix(1:k), MARGIN = 1, FUN = function(x){
    d <- data[which(data$source == unique(data$source)[x]), -s.idx]
    
    spsUtil::quiet(psych::alpha(d))
  })  
  
  Alpha <- sapply(est, FUN = function(x){x$total$raw_alpha})
  
  if(sum(Alpha < 0) > 0){
    stop("Estimate of Coefficient Alpha is negative - variance breakdown is futile")
  }
  
  B.Alpha <- log(1 - Alpha)
  SE_B.Alpha <- sqrt((2 * j)/((j - 1) * (n - 2)))                
                  
  df <- data.frame(Reliability = B.Alpha,
                   StandardError = SE_B.Alpha,
                   source = unique(data$source))
  
  if(csv){
    write.csv(df, file = here(paste0("Data/Reliability Estimates/", project.title, "_Bonett-Alpha.csv")), row.names = FALSE)
  }
  
  return(df)
}


estimate_Bonett_omega <- function(data, csv = FALSE, project.title = NULL){
  
  k <- length(unique(data$source))
  s.idx <- grep("source", names(data))
  j <- length(names(data[,-s.idx]))
  n <- data %>%
    group_by(source) %>%
    summarise(n = n())
  n <- n$n
  
  est <- apply(as.matrix(1:k), MARGIN = 1, FUN = function(x){
    d <- data[which(data$source == unique(data$source)[x]), -s.idx]
    
    fit <- spsUtil::quiet(coefficientalpha::omega(d, se = TRUE, test = FALSE, silent = TRUE))
  })  
  
  Omega <- sapply(est, FUN = function(x){x$omega})
  
  if(sum(Omega < 0) > 0){
    stop("Estimate of Coefficient Alpha is negative - variance breakdown is futile")
  }
  
  B.Omega <- log(1 - Omega)
  SE_B.Omega <- sqrt((2 * j)/((j - 1) * (n - 2)))                
  
  df <- data.frame(Reliability = B.Omega,
                   StandardError = SE_B.Omega,
                   source = unique(data$source))
  
  if(csv){
    write.csv(df, file = here(paste0("Data/Reliability Estimates/", project.title, "_Bonett-Omega.csv")), row.names = FALSE)
  }
  
  return(df)
}



bootstrap_SE_varT <- function(data, indices, stat = "ALPHA"){
  
  d <- data[indices,]
  
  if(stat == "ALPHA"){
    # compute Cronbach's Alpha
    C <- cov(d)
    n <- dim(C)[1]
    alpha <- (1 - sum(diag(C))/sum(C)) * (n/(n - 1))
    
    # return Cronbach's Alpha as rel-object
    rel <- alpha
  }
  if(stat == "OMEGA"){
    omega_fit <- coefficientalpha::omega(d, se = F, varphi = 0, test = F)
    
    omega <- omega_fit$omega
    
    rel <- omega
  }
  
  
  var_X <- var(rowMeans(d, na.rm = T), na.rm = T)
  
  var_T <- as.numeric(rel * var_X)
  
  return(var_T)
  
}



bootstrap_SE_varE <- function(data, indices, stat = "ALPHA"){
  
  d <- data[indices,]
  
  if(stat == "ALPHA"){
    # compute Cronbach's Alpha
    C <- cov(d)
    n <- dim(C)[1]
    alpha <- (1 - sum(diag(C))/sum(C)) * (n/(n - 1))
    
    # return Cronbach's Alpha as rel-object
    rel <- alpha
  }
  if(stat == "OMEGA"){
    omega_fit <- coefficientalpha::omega(d, se = F, varphi = 0, test = F)
    
    omega <- omega_fit$omega
    
    rel <- omega
  }
  
  var_X <- var(rowMeans(d, na.rm = T), na.rm = T)
  
  var_T <- as.numeric(rel * var_X)
  
  var_E <- var_X - var_T
  
  return(var_E)
  
}



apply_Bootstrap_SE_Project.specific <- function(data, var.component = c("TRUE", "ERROR"), R = 100){
  if(length(var.component) != 1){
    stop("Set var.component as either TRUE or ERROR.")
  }
  if(var.component == "TRUE"){
    stat.f <- bootstrap_SE_varT
  }
  if(var.component == "ERROR"){
    stat.f <- bootstrap_SE_varE
  }
  suppressMessages(
  df <- apply(as.matrix(seq_along(unique(data$source))), MARGIN = 1, FUN = function(x){
    bvar <- boot(data = data[data$source == unique(data$source)[x],-grep("source", names(data))],
                 statistic = stat.f,
                 stat = "ALPHA",
                 R = R)
    
    d <- data[data$source == unique(data$source)[x],-grep("source", names(data))]
    
    D <- na.omit(d)
    
    C <- cov(D)
    n <- dim(C)[1]
    
    alpha <- (1 - sum(diag(C))/sum(C)) * (n/(n - 1))
    
    varX <- var(rowMeans(D))
    
    if(var.component == "TRUE"){
      var_est <- as.numeric(varX * alpha )
    }
    if(var.component == "ERROR"){
      var_est <- as.numeric(varX * (1-alpha))
    }
    
    
    
    return(data.frame(SE = sd(log(bvar$t)), 
                      boot.mean = mean(log(bvar$t)),
                      var.emp = log(var_est)))
  })
  )
  
  df.formatted <- data.frame(SE = sapply(df, FUN = function(x){x$SE}),
                             boot.mean = sapply(df, FUN = function(x){x$boot.mean}),
                             var.est = sapply(df, FUN = function(x){x$var.emp}),
                             source = unique(data$source))
  
}


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
    ylab("Lab") +
    ggtitle(main.title)
  
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
      scale_y_continuous(breaks = c(1, (5:(length(rma.fit$yi)+4))), labels = c("RE Model", unique(as.character(rma.data$source)))) 
  }
  
  p
}


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}