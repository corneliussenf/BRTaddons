## Author: Cornelius Senf (Humboldt-Universität zu Berlin)
## Contact: corneliussenf@gmail.com

gbm.ggplot <- function(gbm.objects, 
                       variable,
                       input.data.frame,
                       model.label=paste("M",1:length(gbm.objects),sep="."),
                       xlabs=variable, 
                       ylabs="Fitted function",
                       ylims=NULL,
                       xlims=NULL,
                       smooth=FALSE,
                       rug_sample=1,
                       brewer=NULL,
                       factororder=NULL,
                       factorlabels=factororder){
  
  # gbm.objects: A list of gbm objects. Can be 1 to N.
  # variable: Variable name ot be plotted.
  # model.label: The names used for naming the gbm models. Standard is M1 to MN.
  # xlabs: The xlab. Standard is the variable name used in the gbm model.
  # ylabs: The xlab. Standard is 'Fitted fucntion'
  # ylims: The y-axis limits. Must be a vector of dimension 2.
  # smooth: Logical. Should smoothed lines be plotted.
  # rug_sample: Sampling rate for rugs. Sampling can increase readability. Standard is 1.
  
  require(gbm)
  require(RColorBrewer)
  
  # Number of models
  n.models <- length(gbm.objects)
  
  if(length(variable)==1)variable <- rep(variable,n.models)
  if(class(input.data.frame)=="data.frame"){
    inp <- list()
    for(i in n.models){
      inp[[i]] <- input.data.frame
    }
    input.data.frame <- inp
    }
  
  # Loop trough models
  for(i in 1:n.models){
    
    gbm.object <- gbm.objects[[i]]
    
    # Define inputs
    gbm.call <- gbm.object$gbm.call
    assign(paste("data",i,sep="."), input.data.frame[[i]])
    response.matrix <- plot.gbm(gbm.object, as.character(variable[i]), return.grid = TRUE)
    prediction <- response.matrix[, 1]
    if(class(get(paste("data",i,sep=".")))=="data.frame"){
      if(is.factor(get(paste("data",i,sep="."))[,as.character(variable[i])])){
        prediction <- factor(prediction, levels=levels(get(paste("data",i,sep="."))[,as.character(variable[i])]))
      }
    }else{
      if(is.factor(get(paste("data",i,sep=".")))){
        prediction <- factor(prediction, levels=levels(get(paste("data",i,sep="."))[,as.character(variable[i])]))    
      }
    }
    response <- response.matrix[, 2] - mean(response.matrix[,2])
    assign(paste("gbm.output",i,sep="."), data.frame(prediction, response))  
  }
  
  # Plot
  
  df <- data.frame(x=double(),
                   y=double(),
                   name=character()) 
  
  for(j in 1:n.models){
    name <- rep(model.label[j], nrow(get(paste("gbm.output",j,sep="."))))
    df <- rbind(df,data.frame(x=get(paste("gbm.output",j,sep="."))[,1], 
                              y=get(paste("gbm.output",j,sep="."))[,2], 
                              Model=name))
  }
  
  # Set x/y lim
  if(is.null(xlims) & class(df[,"x"])!="factor")xlims <- c(min(df$x),max(df$x))
  if(is.null(ylims))ylims <- c(min(df[,"y"]), max(df[,"y"]))
  
  if(class(df[,"x"])=="factor"){
    colourCount = length(unique(df$Model))
    getPalette = colorRampPalette(brewer.pal(9, brewer))
    
    p <- ggplot(data=df, aes(x=x, y=y, col=Model)) +
      geom_hline(y.intercept=0, linetype="dashed") + 
      geom_point(size=3.5) +
      xlab(xlabs) +
      ylab(ylabs)+
      theme_classic() +
      scale_color_manual(values = getPalette(colourCount)) +
      theme(panel.border=element_rect(colour="black", fill=NA), 
            panel.grid.major=element_line(colour="grey", linetype="dotted"),
            axis.text.y=element_text(angle=0, hjust=0.5)) +
      ylim(ylims)
    if(is.null(factororder)==FALSE){
      p <- p+scale_x_discrete(limits=rev(factororder), labels=rev(factorlabels))
    }
  }else{
    if(smooth==FALSE){
      if(class(df$Model)=="factor"){
        
        colourCount = length(unique(df$Model))
        getPalette = colorRampPalette(brewer.pal(9, brewer))
        
        p <- ggplot(data=df, aes(x=x, y=y, col=Model)) +
          geom_hline(y.intercept=0, linetype="dashed") + 
          geom_line() +
          xlab(xlabs) +
          ylab(ylabs) +
          theme_classic() +
          scale_color_manual(values = getPalette(colourCount)) +
          theme(panel.border=element_rect(colour="black", fill=NA), 
                panel.grid.major=element_line(colour="grey", linetype="dotted"),
                axis.text.y=element_text(angle=0, hjust=0.5)) +
          ylim(ylims)  +
          xlim(xlims)
        if(is.null(factororder)==FALSE){
          p <- p+scale_x_discrete(limits=rev(factororder), labels=rev(factorlabels))
        }
        #geom_rug(data=data[sample(1:nrow(data),rug_sample*nrow(data)),], aes_string(x=variable), sides="b")
      }else{
        
        colourCount = length(unique(df$Model))
        getPalette = colorRampPalette(brewer.pal(9, brewer))
        
        p <- ggplot(data=df, aes(x=x, y=y, col=factor(Model))) +
          geom_hline(y.intercept=0, linetype="dashed") + 
          geom_line() +
          xlab(xlabs) +
          ylab(ylabs) +
          theme_classic() +
          scale_color_manual(values = getPalette(colourCount)) +
          theme(panel.border=element_rect(colour="black", fill=NA), 
                panel.grid.major=element_line(colour="grey", linetype="dotted"),
                axis.text.y=element_text(angle=0, hjust=0.5)) +
          ylim(ylims)  +
          xlim(xlims)
        #geom_rug(data=data[sample(1:nrow(data),rug_sample*nrow(data)),], aes_string(x=variable), sides="b")
      }
    }else{
      
      colourCount = length(unique(df$Model))
      getPalette = colorRampPalette(brewer.pal(9, brewer))
      
      p <- ggplot(data=df, aes(x=x, y=y, col=Model)) + 
        xlab(xlabs) +
        ylab(ylabs) +
        theme_classic() +
        scale_color_manual(values = getPalette(colourCount)) +
        theme(panel.border=element_rect(colour="black", fill=NA), 
              panel.grid.major=element_line(colour="grey", linetype="dotted"),
              axis.text.y=element_text(angle=0, hjust=0.5)) +
        ylim(ylims)  +
        xlim(xlims) +
        geom_hline(y.intercept=0, linetype="dashed") +
        geom_smooth(method="loess", se=FALSE)
        #geom_rug(data=data[sample(1:nrow(data),rug_sample*nrow(data)),], aes_string(x=variable, col="Model"), sides="b")
    }
  }
  
  return(list(Plot=p, Data=df))
  
}