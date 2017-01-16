## Author: Cornelius Senf (Humboldt-Universit?t zu Berlin)
## Contact: corneliussenf@gmail.com

gbm.ggplot4 <- function(gbm.objects, 
                        variables,
                        labels = variables,
                        modelnames = NULL,
                        ncol = length(variables),
                        nrow = 1,
                        brewer = "Set1",
                        smooth = "yes", # smooth can be either 'yes', 'no', or 'only'
                        ylim = NULL,
                        ylab = "Fitted function"){ 
  
  require(gbm)
  require(RColorBrewer)
  require(dplyr)
  require(gtable)
  require(gridExtra)
  require(scales)
  
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
  
  if(class(gbm.objects) == "gbm"){
    gbm.object <- gbm.objects
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
    gbm.output$Name <- factor(gbm.output$Variable, levels = variables, labels=labels)
  }else if(class(gbm.objects) == "list"){
    gbm.output <- list()
    b <- 0
    for(gbm.object in gbm.objects){
      b <- b + 1
      gbm.call <- gbm.object$gbm.call
      input.data.frame <- gbm.call$dataframe
      
      gbm.outputs <- list()
      a <- 0
      for(i in variables){
        a <- a + 1
        o <- getResponse(i)
        o$Variable <- i
        gbm.outputs[[a]] <- o
      }
      u <- rbind_all(gbm.outputs)
      u$Name <- factor(u$Variable, levels = variables, labels = labels)
      u$Model <- if(is.null(modelnames)){paste0("M", (1:length(gbm.objects))[b])}else{modelnames[b]}
      gbm.output[[b]] <- u
    }
    gbm.output <- rbind_all(gbm.output)
    gbm.output$Model <- factor(as.character(gbm.output$Model))
  }
  
  p <- list()
  a <- 0
  for(i in unique(gbm.output$Variable)){
    a <- a + 1
    gbm.output.filter <- filter(gbm.output, Variable == i)
    
    if(class(input.data.frame[, i]) == "factor"){
      
      if(class(gbm.objects) == "gbm"){
        pp <- ggplot(gbm.output.filter, aes(x = prediction, y = response))
      }else{
        pp <- ggplot(gbm.output.filter, aes(x = prediction, y = response, fill = Model, linetype = Model))
      }
      
      p[[a]] <- pp + geom_hline(yintercept=0, linetype="dashed") +
        geom_bar(stat = "identity", position = "dodge", col = "black") +
        theme_classic() +
        theme(panel.border=element_rect(colour="black", fill=NA), 
              panel.grid.major=element_line(colour="grey", linetype="dotted"),
              axis.text.y=element_text(angle=0, hjust=0.5, size = 9),
              strip.background = element_rect(colour="black", fill="grey"),
              axis.text.x=element_text(size = 9)) +
        xlab(unique(gbm.output$Name)[a]) +
        ylab("") +
        scale_fill_brewer("", palette = brewer)
    }else{
      
      if(class(gbm.objects) == "gbm"){
        pp <- ggplot(gbm.output.filter, aes(x = prediction, y = response))
      }else{
        pp <- ggplot(gbm.output.filter, aes(x = prediction, y = response, col = Model))
      }

      pp <- pp + geom_hline(yintercept=0, linetype="dashed") +
        theme_classic() +
        theme(panel.border=element_rect(colour="black", fill=NA), 
              panel.grid.major=element_line(colour="grey", linetype="dotted"),
              axis.text.y=element_text(angle=0, hjust=0.5, size = 9),
              strip.background = element_rect(colour="black", fill="grey"),
              axis.text.x=element_text(size = 9)) +
        xlab(unique(gbm.output$Name)[a]) +
        ylab("") +
        scale_color_brewer("", palette = brewer)
      
      if(smooth == "yes"){
        pp <- pp + geom_line(alpha = 0.5)
      }else if(smooth == "no"){
        pp <- pp + geom_line()
      }
      
      if(max(gbm.output.filter$prediction)>10000){
        scientific_10 <- function(x) {
          parse(text = gsub("e", " %*% 10^", scientific_format(digits = 2)(x)))
        }
        brks <- quantile(gbm.output.filter$prediction, c(0.1, 0.5, 0.9))
        
        pp <- pp + scale_x_continuous(label = scientific_10, 
                                      breaks = brks)
      }
      
      if(max(gbm.output.filter$prediction)<1){
        scientific_10 <- function(x) {
          parse(text = gsub("e", " %*% 10^", scientific_format(digits = 2)(x)))
        }
        brks <- quantile(gbm.output.filter$prediction, c(0.1, 0.5, 0.9))
        
        pp <- pp + scale_x_continuous(label = scientific_10, 
                                      breaks = brks)
      }
      
      if(is.null(ylim)){
        ylim <- c(min(gbm.output$response), max(gbm.output$response))
      }
      
      pp <- pp + ylim(ylim)
      
      if(class(gbm.objects) == "gbm"){
        if(smooth %in% c("yes", "only")){
          p[[a]] <- pp + geom_smooth(method = "loess", se = FALSE, size = 0.8, col = "black") 
        }else if(smooth == "no"){
          p[[a]] <- pp
        }
      }else{
        if(smooth %in% c("yes", "only")){
          p[[a]] <- pp + geom_smooth(method = "loess", se = FALSE, size = 0.8)
        }else if(smooth == "no"){
          p[[a]] <- pp
        }
      }
    }
  }
  
  if(class(gbm.objects) == "gbm"){
    p <- grid.arrange(arrangeGrob(grobs = p, 
                                  left = textGrob(ylab, rot = 90, vjust = 1), 
                                  nrow = nrow, ncol = ncol, top = ""), 
                      nrow = 1)
  }else{
    legend <- gtable_filter(ggplot_gtable(ggplot_build(p[[1]])), "guide-box")
    p <- lapply(p, function(x)x+theme(legend.position="none"))
    p <- grid.arrange(arrangeGrob(grobs = p, 
                                  left = textGrob(ylab, rot = 90, vjust = 1), 
                                  nrow = nrow, ncol = ncol, top = ""), 
                      legend, 
                      widths=unit.c(unit(1, "npc") - legend$width, legend$width),
                      nrow = 1)
  }
  
  return(p)
  
}