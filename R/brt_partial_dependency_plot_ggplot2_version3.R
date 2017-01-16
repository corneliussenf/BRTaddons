## Author: Cornelius Senf (Humboldt-Universit?t zu Berlin)
## Contact: corneliussenf@gmail.com

gbm.ggplot3 <- function(gbm.object, 
                        variables,
                        order = variables,
                        labels = variables){
  
  require(gbm)
  require(RColorBrewer)
  require(dplyr)
  
  getResponse <- function (var) {
    response.matrix <- plot.gbm(gbm.object, as.character(var), return.grid = TRUE)
    prediction <- response.matrix[, 1]
    if(class(input.data.frame)=="data.frame"){
      if(is.factor(input.data.frame[,as.character(var)])){
        prediction <- factor(prediction, levels=levels(input.data.frame[,as.character(var)]))
      }
    }else{
      if(is.factor(get(paste("data",i,sep=".")))){
        prediction <- factor(prediction, levels=levels(get(paste("data",i,sep="."))[,as.character(var)]))    
      }
    }
    response <- response.matrix[, 2] - mean(response.matrix[,2])
    return(data.frame(prediction,response))
  }
  
  gbm.call <- gbm.object$gbm.call
  input.data.frame <- gbm.call$dataframe
  
  gbm.outputs <- list()
  a <- 0
  for(i in variables){
    a <- a+1
    o <- getResponse(i)
    o$Variable <- i
    gbm.outputs[[a]] <- o
  }
  
  gbm.output <- rbind_all(gbm.outputs)
  gbm.output$Variable <- factor(gbm.output$Variable, levels=order, labels=labels)
  
  unique(gbm.output$Variable)
  
  p <- ggplot(gbm.output, aes(x=prediction, y=response)) +
    geom_hline(yintercept=0, linetype="dashed") +
    geom_line() +
    geom_smooth(method="loess", se=FALSE, col="black", size=0.8) +
    facet_wrap(~Variable, scales="free_x") +
    theme_classic() +
    theme(panel.border=element_rect(colour="black", fill=NA), 
          panel.grid.major=element_line(colour="grey", linetype="dotted"),
          axis.text.y=element_text(angle=0, hjust=0.5),
          strip.background = element_rect(colour="black", fill="grey")) +
    xlab("") +
    ylab("Fitted function")
  
  return(p)
  
}