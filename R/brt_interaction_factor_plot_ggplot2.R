gbm.interaction.factor.ggplot <- function (gbm.object, 
                                           factor, 
                                           var, 
                                           factor.label = NULL, 
                                           var.label = NULL, 
                                           y.label = "fitted value", 
                                           var.range = NULL, 
                                           z.range = NULL, 
                                           brewer = "Set1",
                                           smooth = "yes", # can be 'yes', 'no', or 'only'
                                           recode = NULL
                                           ) 
{
  
  if (!requireNamespace("gbm")) {
    stop("you need to install the gbm package to use this function")
  }
  
  if (!requireNamespace("RColorBrewer")) {
    stop("you need to install the RColorBrewer package to use this function")
  }
  
  library(gbm)
  library(RColorBrewer)
  
  gbm.call <- gbm.object$gbm.call
  gbm.x <- gbm.call$gbm.x
  n.preds <- length(gbm.x)
  gbm.y <- gbm.call$gbm.y
  pred.names <- gbm.call$predictor.names
  family = gbm.call$family
  x.name <- gbm.call$predictor.names[factor]
  if (is.null(factor.label)) {
    factor.label <- gbm.call$predictor.names[factor]
  }
  y.name <- gbm.call$predictor.names[var]
  if (is.null(var.label)) {
    var.label <- gbm.call$predictor.names[var]
  }
  
  data <- gbm.call$dataframe[, gbm.x, drop = FALSE]
  n.trees <- gbm.call$best.trees
  x.var <- names(table(data[, factor]))
  
  if (is.null(var.range)) {
    y.var <- seq(min(data[, var], na.rm = T), max(data[, var], na.rm = T), length = 50)
  } else {
    y.var <- seq(var.range[1], var.range[2], length = 50)
  }
  
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
  prediction <- gbm::predict.gbm(gbm.object, pred.frame, n.trees = n.trees, 
                                 type = "response")
  
  max.pred <- max(prediction)
  cat("maximum value = ", round(max.pred, 2), "\n")
  
  if (is.null(z.range)) {
    if (family == "bernoulli") {
      z.range <- c(0, 1)
    }
    else if (family == "poisson") {
      z.range <- c(0, max.pred * 1.1)
    }
    else {
      z.range <- c(min(prediction), max.pred)
    }
  }
  
  factor.list <- names(table(pred.frame[, 1]))
  n <- 1
  if (is.null(z.range)) {
    vert.limits <- c(0, max.pred * 1.1)
  }else{
    vert.limits <- z.range
  }
    
  plotdata <- data.frame(x = pred.frame[, y.name], y = prediction, f = pred.frame[, x.name])
  
  if(!is.null(recode)){
    levels(plotdata$f) <- recode
    
    require(dplyr)
    
    plotdata_recode <- dplyr::summarize(dplyr::group_by(plotdata, f, x),
                                        y_m = mean(y),
                                        y_min = min(y),
                                        y_max = max(y))
    
    colourCount <- length(unique(plotdata_recode$f))
    getPalette <- colorRampPalette(brewer.pal(9, brewer))
    
    if(length(levels(plotdata$f)) == 1){alpha <- 1}else{alpha <- 0.3}
    
    p <- ggplot(plotdata_recode, aes(x = x, y = y_m)) +
      geom_ribbon(aes(ymax = y_max, ymin = y_min, fill = f), alpha = alpha) +
      geom_line(aes(col = f)) +
      scale_color_manual(factor.label, values = getPalette(colourCount)) +
      scale_fill_manual(factor.label, values = getPalette(colourCount)) +
      ylim(vert.limits) +
      xlab(var.label) +
      ylab(y.label)+ 
      theme_classic() +
      theme(panel.border=element_rect(colour="black", fill=NA), 
            panel.grid.major=element_line(colour="grey", linetype="dotted"),
            axis.text.y=element_text(angle=0, hjust=0.5, size = 9),
            strip.background = element_rect(colour="black", fill="grey"),
            axis.text.x=element_text(size = 9))
    
    if(max(plotdata_recode$x)>10000){
      scientific_10 <- function(x) {
        parse(text = gsub("e", " %*% 10^", scientific_format(digits = 2)(x)))
      }
      
      brks <- quantile(plotdata_recode$x, c(0.1, 0.5, 0.9))
      
      p <- p + scale_x_continuous(label = scientific_10, 
                                    breaks = brks)
    }
    
    if(max(plotdata_recode$x)<1){
      scientific_10 <- function(x) {
        parse(text = gsub("e", " %*% 10^", scientific_format(digits = 2)(x)))
      }
      brks <- quantile(plotdata_recode$x, c(0.1, 0.5, 0.9))
      
      p <- p + scale_x_continuous(label = scientific_10, 
                                    breaks = brks)
    }
    
  }else{
    colourCount <- length(unique(plotdata$f))
    getPalette <- colorRampPalette(brewer.pal(9, brewer))
    
    p <- ggplot(plotdata, aes(x = x, y = y, col = f)) +
      scale_color_manual(factor.label, values = getPalette(colourCount)) +
      ylim(vert.limits) +
      xlab(var.label) +
      ylab(y.label)+ 
      theme_classic() +
      theme(panel.border=element_rect(colour="black", fill=NA), 
            panel.grid.major=element_line(colour="grey", linetype="dotted"),
            axis.text.y=element_text(angle=0, hjust=0.5, size = 9),
            strip.background = element_rect(colour="black", fill="grey"),
            axis.text.x=element_text(size = 9))
    
    if(smooth == "yes"){
      p <- p + geom_line(alpha = 0.5) + geom_smooth(se = FALSE, size = 0.8)
    }else if(smooth == "no"){
      p <- p + geom_line()
    }else if(smooth == "only"){
      p <- p + geom_smooth(se = TRUE, size = 0.8)
    }else{
      stop("Partameter 'smooth' must be either 'yes', 'no', or 'only'!")
    }
  }
  
 
  
  return(p)
}