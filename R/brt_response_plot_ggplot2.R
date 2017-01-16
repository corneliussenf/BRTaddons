gbm.response.ggplot <- function (gbm.object, 
                                 var, 
                                 var.label = NULL, 
                                 y.label = "fitted value", 
                                 var.range = NULL, 
                                 z.range = NULL, 
                                 smooth = "yes" # can be 'yes', 'no', or 'only'
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
  pred.name <- gbm.call$predictor.names[var]
  if (is.null(var.label)) {
    var.label <- gbm.call$predictor.names[var]
  }
  
  data <- gbm.call$dataframe[, gbm.x, drop = FALSE]
  n.trees <- gbm.call$best.trees
  
  if (is.null(var.range)) {
    pred.var <- seq(min(data[, var], na.rm = T), max(data[, var], na.rm = T), length = 50)
  } else {
    pred.var <- seq(var.range[1], var.range[2], length = 50)
  }
  
  pred.frame <- data.frame(pred.var)
  names(pred.frame) <- c(pred.name)
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
  prediction <- gbm::predict.gbm(gbm.object, pred.frame, n.trees = n.trees, type = "response")
  
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
      z.min <- min(data[, y], na.rm = T)
      z.max <- max(data[, y], na.rm = T)
      z.delta <- z.max - z.min
      z.range <- c(z.min - (1.1 * z.delta), z.max + (1.1 * 
                                                       z.delta))
    }
  }
  
  factor.list <- names(table(pred.frame[, 1]))
  n <- 1
  if (is.null(z.range)) {
    vert.limits <- c(0, max.pred * 1.1)
  }else{
    vert.limits <- z.range
  }
    
  plotdata <- data.frame(x = pred.frame[, pred.name], y = prediction)
  
  p <- ggplot(plotdata, aes(x = x, y = y)) +
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
    p <- p + geom_line(alpha = 0.5) + geom_smooth(se = FALSE, col = "black")
  }else if(smooth == "no"){
    p <- p + geom_line()
  }else if(smooth == "only"){
    p <- p + geom_smooth(se = FALSE, col = "black")
  }else{
    stop("Partameter 'smooth' must be either 'yes', 'no', or 'only'!")
  }
  
  return(p)
}