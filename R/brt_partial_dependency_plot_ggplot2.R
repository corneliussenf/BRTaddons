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
                       rug_sample=1){
  
  # gbm.objects: A list of gbm objects. Can be 1 to N.
  # variable: Variable name ot be plotted.
  # model.label: The names used for naming the gbm models. Standard is M1 to MN.
  # xlabs: The xlab. Standard is the variable name used in the gbm model.
  # ylabs: The xlab. Standard is 'Fitted fucntion'
  # ylims: The y-axis limits. Must be a vector of dimension 2.
  # smooth: Logical. Should smoothed lines be plotted.
  # rug_sample: Sampling rate for rugs. Sampling can increase readability. Standard is 1.
  
  require(gbm)
  
  # Helper functions
  extractFromGBM <- function(n.plots, gbm.object, datax, pred.names){
    
    predictors <- list(rep(NA, n.plots))
    responses <- list(rep(NA, n.plots))
    
    for (j in c(1:n.plots)){
      k <- match(gbm.object$contributions$var[j], pred.names)
      var.name <- gbm.call$predictor.names[k]
      #pred.data <- datax[, gbm.call$gbm.x[k]]
      pred.data <- datax[, var.name]
      response.matrix <- plot.gbm(gbm.object, k, return.grid = TRUE)
      predictors[[j]] <- response.matrix[, 1]
      if(is.factor(datax[, gbm.call$gbm.x[k]])){
        predictors[[j]] <- factor(predictors[[j]], levels = levels(datax[,gbm.call$gbm.x[k]]))
      }
      responses[[j]] <- response.matrix[, 2] - mean(response.matrix[,2])
      if(j == 1) {
        ymin = min(responses[[j]])
        ymax = max(responses[[j]])
      }
      else {
        ymin = min(ymin, min(responses[[j]]))
        ymax = max(ymax, max(responses[[j]]))
      }
    }
    return(list(responses=responses, predictors=predictors))
  }
  
  # Number of models
  n.models <- length(gbm.objects)
  
  # Loop trough models
  for(i in 1:n.models){
    
    gbm.object <- gbm.objects[[i]]
    
    # Define inputs
    gbm.call <- gbm.object$gbm.call
    gbm.x <- gbm.call$gbm.x
    assign(paste("pred.names",i,sep="."), gbm.call$predictor.names)
    assign(paste("response.name",i,sep="."), gbm.call$response.name)
    dataframe.name <- gbm.call$dataframe
    #assign(paste("data",i,sep="."), eval(parse(text=dataframe.name)))
    assign(paste("data",i,sep="."), input.data.frame[[i]])
    plot.count <- 0
    n.pages <- 1
    n.plots <- length(get(paste("pred.names",i,sep=".")))
    
    assign(paste("matcher",i,sep="."), match(get(paste("pred.names",i,sep=".")), gbm.object$contributions$var))
    
    # Extract data
    d <- extractFromGBM(n.plots=n.plots, gbm.object=gbm.object, datax=get(paste("data",i,sep=".")), pred.names=get(paste("pred.names",i,sep=".")))
    assign(paste("gbm.output",i,sep="."), d)  
  }
  
  # Plot
  
  for(j in 1:n.models){
    assign(paste("responses",j,sep="."), get(paste("gbm.output",j,sep="."))[["responses"]])
    assign(paste("predictors",j,sep="."), get(paste("gbm.output",j,sep="."))[["predictors"]])
  }
      
  df <- data.frame(x=double(),
                   y=double(),
                   name=character()) 
    
  for(j in 1:n.models){
    name <- rep(model.label[j],length(get(paste("predictors",j,sep="."))[[get(paste("matcher",j,sep="."))[which(get(paste("pred.names",j,sep="."))==variable)]]]))
    df <- rbind(df,data.frame(x=get(paste("predictors",j,sep="."))[[get(paste("matcher",j,sep="."))[which(get(paste("pred.names",j,sep="."))==variable)]]], 
                              y=get(paste("responses",j,sep="."))[[get(paste("matcher",j,sep="."))[which(get(paste("pred.names",j,sep="."))==variable)]]], 
                              Model=name))
  }
  
  if(class(df[,"x"])!="factor"){
    data <- as.data.frame(matrix(NA,nrow=0, ncol=ncol(data.1))) 
    
    for(j in 1:n.models){
      d <- get(paste("data",j,sep="."))
      name <- rep(model.label[j],nrow(d))
      d$Model <- name
      data <- rbind(data,d)
    }
  }
  
  if(is.null(xlims) & class(df[,"x"])!="factor")xlims <- c(min(df$x),max(df$x))
  if(is.null(ylims))ylims <- c(min(df[,"y"]), max(df[,"y"]))
  
  if(class(df[,"x"])=="factor"){
    p <- ggplot(data=df, aes(x=x, y=y, col=Model, shape=Model)) +
      geom_hline(y.intercept=0, linetype="dashed") + 
      geom_point(size=3.5) +
      xlab(xlabs) +
      ylab(ylabs)+
      theme_classic() +
      #scale_color_brewer(palette="Set1") +
      theme(panel.border=element_rect(colour="black", fill=NA), 
            panel.grid.major=element_line(colour="grey", linetype="dotted"),
            axis.text.y=element_text(angle=0, hjust=0.5)) +
      ylim(ylims)
  }else{
    if(smooth==FALSE){
      p <- ggplot(data=df, aes(x=x, y=y, col=Model)) +
        geom_hline(y.intercept=0, linetype="dashed") + 
        geom_line() +
        xlab(xlabs) +
        ylab(ylabs) +
        theme_classic() +
        #if(class(df$Model)=="factor"){scale_color_brewer(palette="Set1") +}
        theme(panel.border=element_rect(colour="black", fill=NA), 
              panel.grid.major=element_line(colour="grey", linetype="dotted"),
              axis.text.y=element_text(angle=0, hjust=0.5)) +
        ylim(ylims)  +
        xlim(xlims) +
        geom_rug(data=data[sample(1:nrow(data),rug_sample*nrow(data)),], aes_string(x=variable), sides="b")
    }else{
      p <- ggplot(data=df, aes(x=x, y=y, col=Model)) + 
        xlab(xlabs) +
        ylab(ylabs) +
        theme_classic() +
        #scale_color_brewer(palette="Set1") +
        theme(panel.border=element_rect(colour="black", fill=NA), 
              panel.grid.major=element_line(colour="grey", linetype="dotted"),
              axis.text.y=element_text(angle=0, hjust=0.5)) +
        ylim(ylims)  +
        xlim(xlims) +
        geom_hline(y.intercept=0, linetype="dashed") +
        geom_smooth(method="loess") +
        geom_rug(data=data[sample(1:nrow(data),rug_sample*nrow(data)),], aes_string(x=variable, col="Model"), sides="b")
    }
  }
    
  return(p)
  
}