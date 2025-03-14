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
  
  data <- na.omit(data)
  
  # compute nr. of groups
  k <- length(unique(data$source))
  # identify "source"-colum
  s.idx <- grep("source", names(data))
  # compute group-sample size
  n <- data %>%
    group_by(source) %>%
    summarise(n = n())
  n <- n$n
  
  j <- sum(!names(data) %in% c("source", "group"))
  
  Alpha <- sapply(as.matrix(1:k), FUN = function(x){
    
    if(!is.null(data$group[1])){
      
      d1 <- data %>% 
        filter(source == unique(data$source)[x]) %>% 
        filter(group == 1) %>% 
        dplyr::select(-c("group", "source"))
      
      d0 <- data %>% 
        filter(source == unique(data$source)[x]) %>% 
        filter(group == 0) %>% 
        dplyr::select(-c("group", "source"))
      
      C1 <- cov(d1)
      j <- dim(C1)[1]
      alpha1 <- (1 - sum(diag(C1))/sum(C1)) * (j/(j - 1))
      
      C0 <- cov(d0)
      j <- dim(C0)[1]
      alpha0 <- (1 - sum(diag(C0))/sum(C0)) * (j/(j - 1))
      
      n1 <- nrow(d1)
      n0 <- nrow(d0)
      
      alpha <- (n1 / (n0 + n1)) * alpha1 + (n0 / (n0 + n1)) * alpha0
    }else{
      
      d <- data %>% 
        filter(source == unique(data$source)[x]) %>% 
        dplyr::select(-"source")
      
      C <- cov(d)
      j <- dim(C)[1]
      alpha <- (1 - sum(diag(C))/sum(C)) * (j/(j - 1))
      
    }
    
    return(alpha)
    
  })  
  
 
  # if(sum(Alpha < 0) > 0){
  #   stop("Estimate of Coefficient Alpha is negative - variance breakdown is futile")
  # }
  
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
  
  d <- na.omit(d)
  
  if(stat == "ALPHA"){
    if(!is.null(d$group[1])){
      
      d1 <- d %>% 
        filter(group == 1) %>% 
        dplyr::select(-c("group"))
      
      d0 <- d %>% 
        filter(group == 0) %>% 
        dplyr::select(-c("group"))
      
      C1 <- cov(d1)
      j <- dim(C1)[1]
      alpha1 <- (1 - sum(diag(C1))/sum(C1)) * (j/(j - 1))
      
      C0 <- cov(d0)
      j <- dim(C0)[1]
      alpha0 <- (1 - sum(diag(C0))/sum(C0)) * (j/(j - 1))
      
      n1 <- nrow(d1)
      n0 <- nrow(d0)
      
      alpha <- (n1 / (n0 + n1)) * alpha1 + (n0 / (n0 + n1)) * alpha0
    }else{
      
      C <- cov(d)
      j <- dim(C)[1]
      alpha <- (1 - sum(diag(C))/sum(C)) * (j/(j - 1))
      
    }
    
    rel <- alpha
  }
  
  if(stat == "OMEGA"){
    omega_fit <- coefficientalpha::omega(d, se = F, varphi = 0, test = F)
    
    omega <- omega_fit$omega
    
    rel <- omega
  }
  
  
  if(!is.null(d$group[1])){
    d1 <- d %>% filter(group == 1) %>% dplyr::select(-"group")
    d0 <- d %>% filter(group == 0) %>% dplyr::select(-"group")
    
    n1 <- nrow(d1)
    n0 <- nrow(d0)
    
    var_X1 <- var(rowMeans(d1, na.rm = T), na.rm = T)
    var_X0 <- var(rowMeans(d0, na.rm = T), na.rm = T)
    
    var_X <- ((n1-1)*var_X1 + (n0-1)*var_X0)/(n1+n0-2)
    
    var_T <- as.numeric(rel * var_X)
    
  }else{
    
    var_X <- var(rowMeans(d, na.rm = T), na.rm = T)
    
    var_T <- as.numeric(rel * var_X)
    
  }
  
  
  return(var_T)
  
}



bootstrap_SE_varE <- function(data, indices, stat = "ALPHA"){
  
  d <- data[indices,]
  
  d <- na.omit(d)
  
  if(stat == "ALPHA"){
    
    if(!is.null(d$group[1])){
      
      d1 <- d %>% 
        filter(group == 1) %>% 
        dplyr::select(-c("group"))
      
      d0 <- d %>% 
        filter(group == 0) %>% 
        dplyr::select(-c("group"))
      
      C1 <- cov(d1)
      j <- dim(C1)[1]
      alpha1 <- (1 - sum(diag(C1))/sum(C1)) * (j/(j - 1))
      
      C0 <- cov(d0)
      j <- dim(C0)[1]
      alpha0 <- (1 - sum(diag(C0))/sum(C0)) * (j/(j - 1))
      
      n1 <- nrow(d1)
      n0 <- nrow(d0)
      
      alpha <- (n1 / (n0 + n1)) * alpha1 + (n0 / (n0 + n1)) * alpha0
    }else{
      
      C <- cov(d)
      j <- dim(C)[1]
      alpha <- (1 - sum(diag(C))/sum(C)) * (j/(j - 1))
      
    }
    
    rel <- alpha
    
  }
  if(stat == "OMEGA"){
    omega_fit <- coefficientalpha::omega(d, se = F, varphi = 0, test = F)
    
    omega <- omega_fit$omega
    
    rel <- omega
  }
  
  if(!is.null(d$group[1])){
    d1 <- d %>% filter(group == 1) %>% dplyr::select(-"group")
    d0 <- d %>% filter(group == 0) %>% dplyr::select(-"group")
    
    n1 <- nrow(d1)
    n0 <- nrow(d0)
    
    var_X1 <- var(rowMeans(d1, na.rm = T), na.rm = T)
    var_X0 <- var(rowMeans(d0, na.rm = T), na.rm = T)
    
    var_X <- ((n1-1)*var_X1 + (n0-1)*var_X0)/(n1+n0-2)
    
    var_T <- as.numeric(rel * var_X)
    
  }else{
    
    var_X <- var(rowMeans(d, na.rm = T), na.rm = T)
    
    var_T <- as.numeric(rel * var_X)
    
  }
  
  var_E <- var_X - var_T
  
  return(var_E)
  
}

bootstrap_SE_varX <- function(data, indices, stat = "ALPHA"){
  
  d <- data[indices,]
  
  d <- na.omit(d)
  
  if(!is.null(d$group[1])){
    d1 <- d %>% filter(group == 1) %>% dplyr::select(-"group")
    d0 <- d %>% filter(group == 0) %>% dplyr::select(-"group")
    
    n1 <- nrow(d1)
    n0 <- nrow(d0)
    
    var_X1 <- var(rowMeans(d1, na.rm = T), na.rm = T)
    var_X0 <- var(rowMeans(d0, na.rm = T), na.rm = T)
    
    var_X <- ((n1-1)*var_X1 + (n0-1)*var_X0)/(n1+n0-2)
    
  }else{
    
    var_X <- var(rowMeans(d, na.rm = T), na.rm = T)
    
    }
  
  
  return(var_X)
  
}



apply_Bootstrap_SE_Project.specific <- function(data, var.component = c("TRUE", "ERROR", "TOTAL"), R = 100){
  if(length(var.component) != 1){
    stop("Set var.component as either TRUE or ERROR.")
  }
  if(var.component == "TRUE"){
    stat.f <- bootstrap_SE_varT
  }
  if(var.component == "ERROR"){
    stat.f <- bootstrap_SE_varE
  }
  if(var.component == "TOTAL"){
    stat.f <- bootstrap_SE_varX
  }
  suppressMessages(
  df <- apply(as.matrix(seq_along(unique(data$source))), MARGIN = 1, FUN = function(x){
    bvar <- boot(data = data[data$source == unique(data$source)[x],-grep("source", names(data))],
                 statistic = stat.f,
                 stat = "ALPHA",
                 R = R)
    
    d <- data[data$source == unique(data$source)[x],-grep("source", names(data))]
    
    d <- na.omit(d)
    
    if(!is.null(d$group[1])){
      
      d1 <- d %>% 
        filter(group == 1) %>% 
        dplyr::select(-c("group"))
      
      d0 <- d %>% 
        filter(group == 0) %>% 
        dplyr::select(-c("group"))
      
      C1 <- cov(d1)
      j <- dim(C1)[1]
      alpha1 <- (1 - sum(diag(C1))/sum(C1)) * (j/(j - 1))
      
      C0 <- cov(d0)
      j <- dim(C0)[1]
      alpha0 <- (1 - sum(diag(C0))/sum(C0)) * (j/(j - 1))
      
      n1 <- nrow(d1)
      n0 <- nrow(d0)
      
      alpha <- (n1 / (n0 + n1)) * alpha1 + (n0 / (n0 + n1)) * alpha0
    }else{
      
      C <- cov(d)
      j <- dim(C)[1]
      alpha <- (1 - sum(diag(C))/sum(C)) * (j/(j - 1))
      
    }
    
    
    
    if(!is.null(d$group[1])){
      d1 <- d %>% filter(group == 1) %>% dplyr::select(-"group")
      d0 <- d %>% filter(group == 0) %>% dplyr::select(-"group")
      
      n1 <- nrow(d1)
      n0 <- nrow(d0)
      
      var_X1 <- var(rowMeans(d1, na.rm = T), na.rm = T)
      var_X0 <- var(rowMeans(d0, na.rm = T), na.rm = T)
      
      var_X <- ((n1-1)*var_X1 + (n0-1)*var_X0)/(n1+n0-2)
      
    }else{
      
      var_X <- var(rowMeans(d, na.rm = T), na.rm = T)
    }
    
    
    if(var.component == "TRUE"){
      var_est <- as.numeric(var_X * alpha )
    }
    if(var.component == "ERROR"){
      var_est <- as.numeric(var_X * (1-alpha))
    }
    if(var.component == "TOTAL"){
      var_est <- as.numeric(var_X)
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

bt_var_m <- function(rma_obj){
  exp(rma_obj$b[1] + (.5*rma_obj$tau2))
}

bt_var_v <- function(rma_obj){
  (exp(rma_obj$tau2) - 1) * exp((2 * rma_obj$b[1]) + rma_obj$tau2)
}

# function to estimate heterogeneity in Cronbach's Alpha, if transformation was used
var_Bonnett_backtransformed <- function(rma_obj){
  (((-exp(rma_obj$b[1]))^2) * rma_obj$tau2) + (.5*((-exp(rma_obj$b[1]))^2)*(rma_obj$tau2^2)) + ((-exp(rma_obj$b[1])) * (-exp(rma_obj$b[1])) * (rma_obj$tau2^2))
}

# function to estimate mean value of Cronbach's Alpha, if transformation was used
mean_Bonnett_backtransformed <- function(rma_obj){
  1 - exp(rma_obj$b[1]) + ((-exp(rma_obj$b[1])) / 2) * rma_obj$tau2
}

my_forest_plot <- function(rma.fit, rma.data, main.title = "Forest Plot", 
                           x.lab = "Estimate", ci.lvl = .975, CI.display = FALSE,
                           transformation = c("None", "ln", "Bonett")){
  
  if(transformation == "None"){
    # Calculate lower and upper limits of confidence levels, for each replication's estimate
    cil <- rma.fit$yi[1:length(rma.fit$yi)] - qnorm(ci.lvl)*sqrt(rma.fit$vi[1:length(rma.fit$vi)])
    ciu <- rma.fit$yi[1:length(rma.fit$yi)] + qnorm(ci.lvl)*sqrt(rma.fit$vi[1:length(rma.fit$vi)])
    
    yi <- rma.fit$yi[1:length(rma.fit$yi)]
    
    b <- rma.fit$b[1]
    bll <- rma.fit$ci.lb
    bul <- rma.fit$ci.ub
    
    lab_labels <- rma.data$source[which(!is.na(rma.data$SE))]
  }
  
  if(transformation == "ln"){
    # Calculate lower and upper limits of confidence levels, for each replication's estimate
    cil <- exp(rma.fit$yi[1:length(rma.fit$yi)] - qnorm(ci.lvl)*sqrt(rma.fit$vi[1:length(rma.fit$vi)]))
    ciu <- exp(rma.fit$yi[1:length(rma.fit$yi)] + qnorm(ci.lvl)*sqrt(rma.fit$vi[1:length(rma.fit$vi)]))
    
    yi <- exp(rma.fit$yi[1:length(rma.fit$yi)])
    
    b <- bt_var_m(rma.fit)
    bll <- exp(rma.fit$ci.lb)
    bul <- exp(rma.fit$ci.ub)
    
    lab_labels <- rma.data$source[which(!is.na(rma.data$SE))]
  }
  
  if(transformation == "Bonett"){
    # Calculate lower and upper limits of confidence levels, for each replication's estimate
    cil <- 1 - exp(rma.fit$yi[1:length(rma.fit$yi)] - qnorm(ci.lvl)*sqrt(rma.fit$vi[1:length(rma.fit$vi)]))
    ciu <- 1 - exp(rma.fit$yi[1:length(rma.fit$yi)] + qnorm(ci.lvl)*sqrt(rma.fit$vi[1:length(rma.fit$vi)]))
    
    yi <- 1 - exp(rma.fit$yi[1:length(rma.fit$yi)])
    
    b <- mean_Bonnett_backtransformed(rma.fit)
    bll <- 1-exp(rma.fit$ci.lb)
    bul <- 1-exp(rma.fit$ci.ub)
    
    lab_labels <- rma.data$source[which(!is.na(rma.data$SE))]
  }
  
  # weights <- 1/sqrt(rma.fit$vi+rma.fit$tau2)
  # weights.scaled <- weights/mean(weights)
  # weights.rescaled <- weights.scaled/mean(weights.scaled)
  
  p <- ggplot() + # initialize ggplot
    
    # plot point estimates
    geom_point(aes(x = yi, y = c(3:(length(yi)+2))
    #                , size = weights.rescaled
                   ), shape = 15) + 
    
    # vertical line at x = 0
    # geom_vline(xintercept = 0, linetype = "dashed") +
    
    # add horizontal line for CI of each replication's estimate
    geom_segment(aes(x = cil, y = c(3:(length(yi)+2)), xend = ciu, yend = c(3:(length(yi)+2)))) +
    
    # ggplot theme
    theme_minimal() +
    
    # add vertical upper & lower-limit "fence"-lines, for each replication's estimate
    geom_segment(aes(x = cil, xend = cil, y = (c(3:(length(yi)+2))+.3), yend = (c(3:(length(yi)+2))-.3) )) +
    geom_segment(aes(x = ciu, xend = ciu, y = (c(3:(length(yi)+2))+.3), yend = (c(3:(length(yi)+2))-.3) )) +
    
    # add vertical upper- & lower-limit "fence lines, for meta-analytic point estimate
    geom_polygon(data = data.frame(x = c(bul, b, bll, b),
                                   y = c(1, 1-.7, 1, 1+.7)),
                 aes(x = x, y = y),
                 colour = "black", fill = "black") +
    
    geom_abline(slope = 0, intercept = 2, colour = "black") +
    
    
    # labs & titles
    xlab(x.lab) +
    ylab("Lab") +
    ggtitle(main.title)
  
  if(CI.display){
    p <- p + 
      scale_y_continuous(breaks = c(1, (3:(length(yi)+2))), 
                         labels = c("RE Model", lab_labels),
                         
                         sec.axis = dup_axis(breaks = c(1, (3:(length(yi)+2))),
                                             labels = c(paste0("[", round(bll, 2), ";", round(bul), "]"), 
                                                        paste0("[", round(cil, 2), ";", round(ciu, 2), "]")),
                                             name = ""))
    # p <- p + geom_text(aes(y = c(3:(length(rma.fit$yi)+2)), x = (max(ciu) + abs(max(ciu))*.05),
    #               label = paste0("[", round(cil, 2), ";", round(ciu, 2), "]")))
  }else{
    p <- p +     # adjust labels on y-axis, to display lab-abreviations
      scale_y_continuous(breaks = c(1, (3:(length(yi)+2))), labels = c("RE Model", lab_labels)) 
  }
  
  p
}


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}