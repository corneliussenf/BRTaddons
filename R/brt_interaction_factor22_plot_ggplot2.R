gbm.interaction.factor22.ggplot <- function (gbm.objects, 
                                            factor,
                                            cut.breaks,
                                            cut.labels = 1:length(cut.breaks),
                                            var, 
                                            factor.label = NULL, 
                                            var.label = NULL, 
                                            y.label = "fitted value", 
                                            var.range = NULL, 
                                            z.range = NULL, 
                                            brewer = "Set1") 
{
  
  if (!requireNamespace("gbm")) {
    stop("you need to install the gbm package to use this function")
  }
  
  if (!requireNamespace("RColorBrewer")) {
    stop("you need to install the RColorBrewer package to use this function")
  }
  
  library(gbm)
  library(RColorBrewer)
  
  y <- c()
  for(g in gbm.objects){
    gbm.call <- g$gbm.call
    gbm.x <- gbm.call$gbm.x
    data <- gbm.call$dataframe[, gbm.x, drop = FALSE]
    y <- c(y, data[, var])
  }
  if (is.null(var.range)) {
    y.var <- sample(y, 100)
  } else {
    y.var <- sample(y[y > var.range[1] & y < var.range[2]], 100)
  }
  
  plotdata <- list()
  a <- 0
  for(g in gbm.objects){
    a <- a + 1
    gbm.call <- g$gbm.call
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
    x.var <- sample(data[, factor], 100)
    
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
    
    prediction <- gbm::predict.gbm(g, pred.frame, n.trees = n.trees, 
                                   type = "response")
    
    d <- data.frame(x = pred.frame[, y.name], y = prediction, f = pred.frame[, x.name])
    d$a <- a
    plotdata[[a]] <- d
  }
  
  plotdata <- do.call(rbind, plotdata)
  plotdata$f <- cut(plotdata$f, cut.breaks, labels = cut.labels)
  plotdata <- dplyr::summarise(group_by(plotdata, f, x, a), 
                               y_m = mean(y))
  plotdata <- dplyr::summarise(group_by(plotdata, f, x), 
                               y_mm = mean(y_m),
                               y_lower = y_mm - 1.96 * (sd(y_m)/sqrt(length(y_m))),
                               y_upper = y_mm + 1.96 * (sd(y_m)/sqrt(length(y_m))))
  
  if (is.null(z.range)) {
    if (family == "bernoulli") {
      z.range <- c(0, 1)
    }
    else if (family == "poisson") {
      z.range <- c(0, max(plotdata$y_m) * 1.1)
    }
    else {
      z.range <- c(min(plotdata$y_m), max(plotdata$y_m))
    }
  }
  
  if (is.null(z.range)) {
    vert.limits <- c(0, max.pred * 1.1)
  }else{
    vert.limits <- z.range
  }
  
  if(brewer != "bw"){
    
    colourCount <- length(unique(plotdata$f))
    getPalette <- colorRampPalette(brewer.pal(9, brewer))
    
    p <- ggplot(plotdata, aes(x = x, y = y_mm, col = f, group = f)) +
      geom_ribbon(aes(ymax = y_upper, ymin = y_lower, fill = f), alpha = 0.4) +
      geom_line() +
      scale_color_manual(factor.label, values = getPalette(colourCount)) +
      scale_fill_manual(factor.label, values = getPalette(colourCount)) +
      ylim(vert.limits) +
      xlab(var.label) +
      ylab(y.label) + 
      theme_classic() +
      theme(panel.border=element_rect(colour="black", fill=NA), 
            panel.grid.major=element_line(colour="grey", linetype="dotted"),
            axis.text.y=element_text(angle=0, hjust=0.5, size = 9),
            strip.background = element_rect(colour="black", fill="grey"),
            axis.text.x=element_text(size = 9))
  }else{
    p <- ggplot(plotdata, aes(x = x, y = y_mm)) +
      #geom_ribbon(aes(ymax = y_upper, ymin = y_lower, group = f), fill = "grey") +
      geom_line(aes(linetype = f)) +
      scale_linetype_manual(factor.label, values = 1:length(levels(plotdata$f))) +
      ylim(vert.limits) +
      xlab(var.label) +
      ylab(y.label)+ 
      theme_classic() +
      theme(panel.border=element_rect(colour="black", fill=NA), 
            panel.grid.major=element_line(colour="grey", linetype="dotted"),
            axis.text.y=element_text(angle=0, hjust=0.5, size = 9),
            strip.background = element_rect(colour="black", fill="grey"),
            axis.text.x=element_text(size = 9))
  }
  
  return(p)
}