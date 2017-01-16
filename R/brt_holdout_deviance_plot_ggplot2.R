## Author: Cornelius Senf (Humboldt-Universität zu Berlin)
## Contact: corneliussenf@gmail.com

holdout.deviance.ggplot <- function(x){
  p <- ggplot(data = data.frame(Deviance = x$cv.values, Trees = x$trees.fitted), aes(x = Trees, y = Deviance)) +
    geom_line() +
    geom_line(data = data.frame(Deviance = x$cv.values + x$cv.loss.ses, Trees = x$trees.fitted), 
              aes(x = Trees, y = Deviance), linetype = "dotted") +
    geom_line(data = data.frame(Deviance = x$cv.values - x$cv.loss.ses, Trees = x$trees.fitted), 
              aes(x = Trees, y = Deviance), linetype = "dotted") +
    geom_vline(xintercept = x$trees.fitted[match(TRUE, x$cv.values == min(x$cv.values))], linetype = "dashed") +
    geom_hline(yintercept = min(x$cv.values), linetype = "dashed") +
    xlab("Number of trees") +
    ylab("Holdout deviance") +
    theme_classic() +
    theme(panel.border=element_rect(colour="black", fill=NA), 
          panel.grid.major=element_line(colour="grey", linetype="dotted"),
          axis.text.y=element_text(angle=0, hjust=0.5))
  
  return(p)
}

