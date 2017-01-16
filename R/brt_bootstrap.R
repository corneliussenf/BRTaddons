
### Functions to perform bootstrap analysis on a gbm model


# Base bootstrapped function ----------------------------------------

gbm.boot <- function(gbm, R){
  
  # gbm = a gbm object
  # R = number of permutations
  
  ## Check if output list is too large
  s <- format(object.size(gbm), units = "MB")
  s <- as.double(gsub(" Mb", "", s))
  
  if(s*R > memory.limit()){
    stop(paste0("Size of output is larger than maximum memory limit ",
                "(estimated size: ", R*s, "mb). ",
                "Reduce number of permutations or increase memory limit."))
  }
  
  ## Get data frame and extract predictors and response
  data <- gbm$gbm.call$dataframe
  predictors <- names(data)[gbm$gbm.call$gbm.x]
  response <- names(data)[gbm$gbm.call$gbm.y]
  
  ## Function for refitting the gbm model with boostraped data
  boot_fun <- function(x){
    
    print(paste0("Fitting model no. ", x, " out of ", R))
    
    # Resample data
    data_boot <- data[sample(1:nrow(data), nrow(data), replace = TRUE), ]
    
    # Refit gbm model with resampled data
    fit_boot <- gbm(formula = as.formula(paste0(response, "~", paste(predictors, collapse = "+"))),
                    data = data_boot,
                    distribution = gbm$distribution$name,
                    weights = gbm$weights,
                    var.monotone = gbm$var.monotone,
                    n.trees = gbm$n.trees,
                    shrinkage = gbm$shrinkage,
                    interaction.depth = gbm$interaction.depth,
                    bag.fraction = gbm$bag.fraction,
                    train.fraction = gbm$train.fraction,
                    n.minobsinnode = gbm$n.minobsinnode,
                    cv.folds = 0,
                    verbose = gbm$verbose)
    
    return(fit_boot)
  }
  
  ## Replicate fit_boot R times
  fits <- lapply(1:R, boot_fun)
  
  ## Return gbm_boot object
  return(fits)
}

# Boostrapped variable importance -----------------------------------------

gbm.boot.varimp <- function(gbm_boot){
  varimp <- lapply(gbm_boot, summary, plot = FALSE)
  varimp <- do.call(rbind, varimp)
  varimp_summary <- tapply(varimp$rel.inf, varimp$var, function(x){
    c(Mean = mean(x),
      Median = median(x),
      SD = sd(x), 
      quantile(x, c(0.01, 0.05, 0.25, 0.75, 0.95, 0.99)))})
  varimp_summary <- do.call(rbind, varimp_summary)
  return(varimp_summary)
}

# Bootstrap response curve ------------------------------------------------

gbm.boot.response.curve <- function (gbm.objects,
                                     data,
                                     var, 
                                     var.label = NULL,
                                     y.label = "fitted value", 
                                     var.range = NULL, 
                                     z.range = NULL
) 
{
  
  if (!requireNamespace("gbm")) {
    stop("you need to install the gbm package to use this function")
  }
  
  if (!requireNamespace("RColorBrewer")) {
    stop("you need to install the RColorBrewer package to use this function")
  }
  
  if (!requireNamespace("scales")) {
    stop("you need to install the scales package to use this function")
  }
  
  library(gbm)
  library(RColorBrewer)
  library(scales)
  
  data <- data[, gbm.objects[[1]]$var.names]
  
  if (is.null(var.range)) {
    y.var <- seq(min(data[, var], na.rm = T), max(data[, var], na.rm = T), length = 250)
  } else {
    y.var <- seq(var.range[1], var.range[2], length = 250)
  }
  
  plotdata <- list()
  
  a <- 0
  for(gbm.call in gbm.objects){
    a <- a + 1
    
    n.trees <- gbm.call$n.trees
    n.preds <- ncol(data)
    pred.names <- colnames(data)
    family = gbm.call$distribution$name
    y.name <- colnames(data)[var]
    if (is.null(var.label)) {
      var.label <- y.name
    }
    
    pred.frame <- data.frame(y.var)
    names(pred.frame) <- c(y.name)
    pred.rows <- nrow(pred.frame)
    
    j <- 2
    for (i in 1:n.preds) {
      if (i != var) {
        if (is.vector(data[, i])) {
          pred.frame[, j] <- mean(data[, i], na.rm = T)
        }
        if (is.factor(data[, i])) {
          temp.table <- table(data[, i])
          pred.frame[, j] <- rep(names(temp.table)[2], pred.rows)
          pred.frame[, j] <- factor(pred.frame[, j], levels = names(temp.table))
        }
        names(pred.frame)[j] <- pred.names[i]
        j <- j + 1
      }
    }
    
    prediction <- gbm::predict.gbm(gbm.call, pred.frame, n.trees = n.trees, 
                                   type = "response")
    
    plotdata[[a]] <- data.frame(x = pred.frame[, y.name], y = prediction, f = a)
  }
  
  plotdata <- do.call(rbind, plotdata)
  plotdata$f <- factor(plotdata$f)
  
  if (is.null(z.range)) {
    if (family == "bernoulli") {
      z.range <- c(0, 1)
    }
    else if (family == "poisson") {
      z.range <- c(0, max(plotdata$y_mm) * 1.1)
    }
    else {
      z.range <- c(min(plotdata$y_mm), max(plotdata$y_mm))
    }
  }
  
  if (is.null(z.range)) {
    vert.limits <- c(0, max(plotdata$y_mm) * 1.1)
  }else{
    vert.limits <- z.range
  }
  
  require(dplyr)
    
  plotdata_summary <- dplyr::summarize(dplyr::group_by(plotdata, x),
                                        y_m = mean(y),
                                        y_min = quantile(y, 0.05),
                                        y_max = quantile(y, 0.95))
  
  p <- ggplot(plotdata_summary, aes(x = x, y = y_m)) +
    geom_ribbon(aes(ymax = y_max, ymin = y_min), fill = "grey") +
    geom_line() +
    #geom_line(aes(x = x, y = y_min), linetype = "dashed") +
    #geom_line(aes(x = x, y = y_max), linetype = "dashed") +
    ylim(vert.limits) +
    xlab(var.label) +
    ylab(y.label)+ 
    theme_classic() +
    theme(panel.border=element_rect(colour="black", fill=NA), 
          panel.grid.major=element_line(colour="grey", linetype="dotted"),
          axis.text.y=element_text(angle=0, hjust=0.5, size = 9),
          strip.background = element_rect(colour="black", fill="grey"),
          axis.text.x=element_text(size = 9))
  
  if(max(plotdata_summary$x)>10000){
    scientific_10 <- function(x) {
      parse(text = gsub("e", " %*% 10^", scientific_format(digits = 2)(x)))
    }
    
    brks <- quantile(plotdata_summary$x, c(0.1, 0.5, 0.9))
    
    p <- p + scale_x_continuous(label = scientific_10, 
                                breaks = brks)
  }
  
  if(max(plotdata_summary$x)<1){
    scientific_10 <- function(x) {
      parse(text = gsub("e", " %*% 10^", scientific_format(digits = 2)(x)))
    }
    brks <- quantile(plotdata_summary$x, c(0.1, 0.5, 0.9))
    
    p <- p + scale_x_continuous(label = scientific_10, 
                                breaks = brks)
  }
  
  return(p)
}

# Bootstrap response curve conditional on second variable ----------------------------

gbm.boot.response.curve.conditional <- function (gbm.objects,
                                                 data,
                                                 var,
                                                 factor,
                                                 factor.label,
                                                 cut.breaks,
                                                 cut.labels,
                                                 var.label = NULL,
                                                 y.label = "fitted value", 
                                                 var.range = NULL, 
                                                 z.range = NULL
) 
{
  
  if (!requireNamespace("gbm")) {
    stop("you need to install the gbm package to use this function")
  }
  
  if (!requireNamespace("RColorBrewer")) {
    stop("you need to install the RColorBrewer package to use this function")
  }
  
  if (!requireNamespace("scales")) {
    stop("you need to install the scales package to use this function")
  }
  
  library(gbm)
  library(RColorBrewer)
  library(scales)
  
  data <- data[, gbm.objects[[1]]$var.names]
  
  if (is.null(var.range)) {
    y.var <- seq(min(data[, var], na.rm = T), max(data[, var], na.rm = T), length = 50)
  } else {
    y.var <- seq(var.range[1], var.range[2], length = 50)
  }
  
  plotdata <- list()
  a <- 0
  
  for(gbm.call in gbm.objects){
    a <- a + 1
    
    n.trees <- gbm.call$n.trees
    n.preds <- ncol(data)
    pred.names <- colnames(data)
    family = gbm.call$distribution$name
    y.name <- colnames(data)[var]
    x.name <- colnames(data)[factor]
    
    x.var <- sample(data[, factor], 50)
    
    pred.frame <- expand.grid(list(x.var, y.var))
    names(pred.frame) <- c(x.name, y.name)
    pred.rows <- nrow(pred.frame)
    
    j <- 3
    for (i in 1:n.preds) {
      if (i != factor & i != var) {
        if (is.vector(data[, i])) {
          pred.frame[, j] <- mean(data[, i], na.rm = T)
        }
        if (is.factor(data[, i])) {
          temp.table <- table(data[, i])
          pred.frame[, j] <- rep(names(temp.table)[2], pred.rows)
          pred.frame[, j] <- factor(pred.frame[, j], levels = names(temp.table))
        }
        names(pred.frame)[j] <- pred.names[i]
        j <- j + 1
      }
    }
    
    prediction <- gbm::predict.gbm(gbm.call, pred.frame, n.trees = n.trees, 
                                   type = "response")
    
    d <- data.frame(x = pred.frame[, y.name], y = prediction, f = pred.frame[, x.name])
    d$a <- a
    plotdata[[a]] <- d
  }
  
  plotdata <- do.call(rbind, plotdata)
  plotdata$f <- cut(plotdata$f, cut.breaks, labels = cut.labels)
  
  plotdata_summary <- dplyr::summarise(group_by(plotdata, x, f, a), 
                                       y_m = mean(y))
  
  plotdata_summary <- dplyr::summarise(group_by(plotdata_summary, f, x), 
                                      n = length(y_m),
                                      y_mm = mean(y_m),
                                      y_min = quantile(y_m, 0.05),
                                      y_max = quantile(y_m, 0.95))
  
  if (is.null(z.range)) {
    if (family == "bernoulli") {
      z.range <- c(0, 1)
    }
    else if (family == "poisson") {
      z.range <- c(0, max(plotdata$y_mm) * 1.1)
    }
    else {
      z.range <- c(min(plotdata$y_mm), max(plotdata$y_mm))
    }
  }
  
  if (is.null(z.range)) {
    vert.limits <- c(0, max(plotdata$y_mm) * 1.1)
  }else{
    vert.limits <- z.range
  }
  
  p <- ggplot(plotdata_summary, aes(x = x, y = y_mm)) +
    geom_ribbon(aes(ymax = y_max, ymin = y_min, group = f), fill = "grey") +
    geom_line(aes(linetype = f)) +
    #geom_line(aes(x = x, y = y_lower), linetype = "dashed") +
    #geom_line(aes(x = x, y = y_upper), linetype = "dashed") +
    ylim(vert.limits) +
    xlab(var.label) +
    ylab(y.label)+ 
    theme_classic() +
    theme(panel.border=element_rect(colour="black", fill=NA), 
          panel.grid.major=element_line(colour="grey", linetype="dotted"),
          axis.text.y=element_text(angle=0, hjust=0.5, size = 9),
          strip.background = element_rect(colour="black", fill="grey"),
          axis.text.x=element_text(size = 9)) +
    scale_linetype_manual(factor.label, values = 1:length(unique(plotdata$f)))
  
  if(max(plotdata$x)>10000){
    scientific_10 <- function(x) {
      parse(text = gsub("e", " %*% 10^", scientific_format(digits = 2)(x)))
    }
    
    brks <- quantile(plotdata$x, c(0.1, 0.5, 0.9))
    
    p <- p + scale_x_continuous(label = scientific_10, 
                                breaks = brks)
  }
  
  if(max(plotdata$x)<1){
    scientific_10 <- function(x) {
      parse(text = gsub("e", " %*% 10^", scientific_format(digits = 2)(x)))
    }
    brks <- quantile(plotdata$x, c(0.1, 0.5, 0.9))
    
    p <- p + scale_x_continuous(label = scientific_10, 
                                breaks = brks)
  }
  
  return(p)
}
