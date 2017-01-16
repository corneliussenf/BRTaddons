gbm.interaction.factor2.ggplot <- function (gbm.object, 
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
    
  plotdata <- data.frame(x = pred.frame[, y.name], y = prediction, f = pred.frame[, x.name])
  plotdata$f <- cut(plotdata$f, cut.breaks, labels = cut.labels)
  plotdata <- dplyr::summarise(group_by(plotdata, f, x), 
                               y_m = mean(y, na.rm = TRUE))
  
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
    
    p <- ggplot(plotdata, aes(x = x, y = y_m, col = f, group = f)) +
      geom_line() +
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
  }else{
    p <- ggplot(plotdata, aes(x = x, y = y_m)) +
      geom_ribbon(aes(ymax = y_upper, ymin = y_lower, group = f), fill = "grey", alpha = 0.3) +
      geom_line(aes(linetype = f)) +
      scale_linetype_manual(factor.label, values = 1:colourCount) +
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