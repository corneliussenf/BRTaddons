BRTvariableImportanceGGplot <- function(brt){
  require(ggplot2)
  imp <- summary(brt, plot=FALSE)
  colnames(imp) <- c("Variables","Importance")
  ggplot(imp, aes(y=Importance, x=reorder(Variables,Importance))) + 
    geom_bar(stat="identity") +
    theme_classic() +
    scale_fill_brewer(palette="Set1") +
    theme(panel.border=element_rect(colour="black", fill=NA), 
          panel.grid.major=element_line(colour="grey", linetype="dotted"),
          axis.text.y=element_text(angle=0, hjust=0.5)) +
    xlab("") +
    ylab("Relative importance") +
    coord_flip()
}