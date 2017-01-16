## Calibrating BRT model with (1) autologistic or (2) residual autocovariate term
## Reference: Crase et al. 2012, Ecography, 35, 879-888
## Author: Cornelius Senf (Humboldt-Universit?t zu Berlin)
## Contact: corneliussenf@gmail.com

gbm.sac <- function(dat, 
         index.predictors, 
         index.response, 
         index.coords,
         spatial="Both",
         nb.max=15,
         parallel="NONE",
         tree.complexity=5,
         learning.rate=0.001, 
         bag.fraction=0.5,
         family="gaussian",
         max.trees=15000,
         longlat=FALSE,
         prev.stratify=TRUE,
         step.size=50){
  
#   # Variables for testing
#   dat <- metrics_model_fd
#   index.predictors <- which(names(metrics_model_fd)%in%predictors)
#   index.response <- which(names(metrics_model_fd)=="GDMAG_MEAN")
#   index.coords <- which(names(metrics_model_fd)%in%c("x","y"))
  
  require(dismo)
  require(spdep)
  require(ggplot2)
  require(doParallel)
  require(foreach)
  
  # Calculate intital BRT
  initial.brt <- gbm.step(dat, 
                          gbm.x=index.predictors, 
                          gbm.y=index.response,
                          tree.complexity=tree.complexity,
                          learning.rate=learning.rate, 
                          bag.fraction=bag.fraction,
                          family=family,
                          max.trees=max.trees,
                          prev.stratify=prev.stratify,
                          step.size=step.size)
  
  # Do prediction
  dat$initial.prediction <- predict(object=initial.brt, newdata=dat, n.trees=initial.brt$n.trees, type="response")
  
  # Calculate spatial auto-correlation in residuals and optimal neighborhood (currently only knn)
  dat$initial.residuals <- dat[,index.response]-dat$initial.prediction
  coords <- as.matrix(dat[,index.coords])
  
  # Do in parallel if wanted
  if(parallel=="NONE"){
    mm <- c()
    pp <- c()
    for(i in 1:nb.max){
      knn <- knearneigh(coords, k=i, longlat=longlat)
      nb <- knn2nb(knn)
      listw <- nb2listw(nb, style="W", zero.policy=TRUE)
      m <- moran.test(dat$initial.residuals, listw, randomisation=TRUE)
      mm <- c(mm, m$estimate[1])
      pp <- c(pp, m$p.value)
    }
    morans.stat <- data.frame(MoransI=mm, pvalue=pp)
    morans.stat$Neighbors <- 1:nb.max
  }else if(class(parallel)=="numeric"){
    sac.parallel.func <- function(i){
      knn <- knearneigh(coords, k=i, longlat=longlat)
      nb <- knn2nb(knn)
      listw <- nb2listw(nb, style="W", zero.policy=TRUE)
      m <- moran.test(dat$initial.residuals, listw, randomisation=TRUE)
      return(c(m$estimate[1], m$p.value))
    }
    cl <- makeCluster(parallel)
    registerDoParallel(cl)
    morans.stat <- foreach(i=1:nb.max, .packages=c("spdep"), .combine='rbind') %dopar% {
      sac.parallel.func(i=i)
    } 
    stopCluster(cl)
    morans.stat <- as.data.frame(morans.stat)
    names(morans.stat) <- c("MoransI", "pvalue")
    morans.stat$Neighbors <- 1:nb.max
  }
  
  morans.stat.initial <- morans.stat
  
  k.opt <- (1:nb.max)[sum(abs(diff(morans.stat$MoransI))>(sd(abs(diff(morans.stat$MoransI)))/sqrt(nrow(morans.stat))*2))]
  print(paste("Using ",k.opt," neighbor(s) for spatial models (Morans I=",round(morans.stat.initial[k.opt,"MoransI"],2),
              "; p-value=",round(morans.stat.initial[k.opt,"pvalue"],2),").", sep=""))
  
  # Calculate autologistic (AL) and residual autocovariate (RAC) term
  knn <- knearneigh(coords, k=k.opt, longlat=longlat)
  nb <- knn2nb(knn)
  listw <- nb2listw(nb, style="W", zero.policy=TRUE)
  
  if(spatial=="Both"){
    dat$autologistic <- stats::lag(listw, dat[,index.response])
    dat$residualcovariate <- stats::lag(listw, dat$initial.residuals)
    
    # AL model
    al.brt <- gbm.step(dat, 
                       gbm.x=c(index.predictors,which(names(dat)=="autologistic")), 
                       gbm.y=index.response,
                       tree.complexity=tree.complexity,
                       learning.rate=learning.rate, 
                       bag.fraction=bag.fraction,
                       family=family,
                       max.trees=max.trees,
                       keep.data=TRUE,  
                       keep.fold.models=TRUE, 
                       keep.fold.vector=TRUE, 
                       keep.fold.fit=TRUE,
                       prev.stratify=prev.stratify,
                       step.size=step.size)
    
    dat$al.prediction <- predict(object=al.brt, newdata=dat, n.trees=al.brt$n.trees, type="response")
    
    dat$al.residuals <- dat[,index.response]-dat$al.prediction
    
    # Do in parallel if wanted
    if(parallel=="NONE"){
      mm <- c()
      pp <- c()
      for(i in 1:nb.max){
        knn <- knearneigh(coords, k=i, longlat=longlat)
        nb <- knn2nb(knn)
        listw <- nb2listw(nb, style="W", zero.policy=TRUE)
        m <- moran.test(dat$al.residuals, listw, randomisation=TRUE)
        mm <- c(mm, m$estimate[1])
        pp <- c(pp, m$p.value)
      }
      morans.stat <- data.frame(MoransI=mm, pvalue=pp)
      morans.stat$Neighbors <- 1:nb.max
    }else if(class(parallel)=="numeric"){
      sac.parallel.func <- function(i){
        knn <- knearneigh(coords, k=i, longlat=longlat)
        nb <- knn2nb(knn)
        listw <- nb2listw(nb, style="W", zero.policy=TRUE)
        m <- moran.test(dat$al.residuals, listw, randomisation=TRUE)
        return(c(m$estimate[1], m$p.value))
      }
      cl <- makeCluster(parallel)
      registerDoParallel(cl)
      morans.stat <- foreach(i=1:nb.max, .packages=c("spdep"), .combine='rbind') %dopar% {
        sac.parallel.func(i=i)
      } 
      stopCluster(cl)
      morans.stat <- as.data.frame(morans.stat)
      names(morans.stat) <- c("MoransI", "pvalue")
      morans.stat$Neighbors <- 1:nb.max
    }
    
    morans.stat.initial.al <- rbind(morans.stat.initial, morans.stat)
    
    # RAC model
    rac.brt <- gbm.step(dat, 
                       gbm.x=c(index.predictors,which(names(dat)=="residualcovariate")), 
                       gbm.y=index.response,
                       tree.complexity=tree.complexity,
                       learning.rate=learning.rate, 
                       bag.fraction=bag.fraction,
                       family=family,
                       max.trees=max.trees,
                       keep.data=TRUE,  
                       keep.fold.models=TRUE, 
                       keep.fold.vector=TRUE, 
                       keep.fold.fit=TRUE,
                       prev.stratify=prev.stratify,
                       step.size=step.size)
    
    dat$rac.prediction <- predict(object=rac.brt, newdata=dat, n.trees=rac.brt$n.trees, type="response")
    
    dat$rac.residuals <- dat[,index.response]-dat$rac.prediction
    
    # Do in parallel if wanted
    if(parallel=="NONE"){
      mm <- c()
      pp <- c()
      for(i in 1:nb.max){
        knn <- knearneigh(coords, k=i, longlat=longlat)
        nb <- knn2nb(knn)
        listw <- nb2listw(nb, style="W", zero.policy=TRUE)
        m <- moran.test(dat$rac.residuals, listw, randomisation=TRUE)
        mm <- c(mm, m$estimate[1])
        pp <- c(pp, m$p.value)
      }
      morans.stat <- data.frame(MoransI=mm, pvalue=pp)
      morans.stat$Neighbors <- 1:nb.max
    }else if(class(parallel)=="numeric"){
      sac.parallel.func <- function(i){
        knn <- knearneigh(coords, k=i, longlat=longlat)
        nb <- knn2nb(knn)
        listw <- nb2listw(nb, style="W", zero.policy=TRUE)
        m <- moran.test(dat$rac.residuals, listw, randomisation=TRUE)
        return(c(m$estimate[1], m$p.value))
      }
      cl <- makeCluster(parallel)
      registerDoParallel(cl)
      morans.stat <- foreach(i=1:nb.max, .packages=c("spdep"), .combine='rbind') %dopar% {
        sac.parallel.func(i=i)
      } 
      stopCluster(cl)
      morans.stat <- as.data.frame(morans.stat)
      names(morans.stat) <- c("MoransI", "pvalue")
      morans.stat$Neighbors <- 1:nb.max
    }
    
    morans.stat.initial.al.rac <- rbind(morans.stat.initial.al, morans.stat)
    
    # Plot SAC
    
    rownames(morans.stat.initial.al.rac) <- 1:nrow(morans.stat.initial.al.rac)
    
    morans.stat.initial.al.rac$Model <- c(rep("Initial",nb.max), rep("AL",nb.max), rep("RAC",nb.max))
    
    p <- ggplot(data=morans.stat.initial.al.rac, aes(x=Neighbors, y=MoransI, col=Model)) +
      geom_line() +
      geom_point(aes()) +
      scale_color_brewer(palette="Set1") +
      theme_classic() +
      theme(panel.border=element_rect(colour="black", fill=NA), 
            panel.grid.major=element_line(colour="grey", linetype="dotted"),
            axis.text.y=element_text(angle=0, hjust=0.5)) +
      geom_hline(yintercept=0, linetype="dashed")
    
  }else if(spatial=="AL"){
    
    dat$autologistic <- stats::lag(listw, dat[,index.response])
    
    # AL model
    al.brt <- gbm.step(dat, 
                       gbm.x=c(index.predictors,which(names(dat)=="autologistic")), 
                       gbm.y=index.response,
                       tree.complexity=tree.complexity,
                       learning.rate=learning.rate, 
                       bag.fraction=bag.fraction,
                       family=family,
                       max.trees=max.trees,
                       keep.data=TRUE,  
                       keep.fold.models=TRUE, 
                       keep.fold.vector=TRUE, 
                       keep.fold.fit=TRUE,
                       prev.stratify=prev.stratify,
                       step.size=step.size)
    
    dat$al.prediction <- predict(object=al.brt, newdata=dat, n.trees=al.brt$n.trees, type="response")
    
    dat$al.residuals <- dat[,index.response]-dat$al.prediction
    
    # Do in parallel if wanted
    if(parallel=="NONE"){
      mm <- c()
      pp <- c()
      for(i in 1:nb.max){
        knn <- knearneigh(coords, k=i, longlat=longlat)
        nb <- knn2nb(knn)
        listw <- nb2listw(nb, style="W", zero.policy=TRUE)
        m <- moran.test(dat$al.residuals, listw, randomisation=TRUE)
        mm <- c(mm, m$estimate[1])
        pp <- c(pp, m$p.value)
      }
      morans.stat <- data.frame(MoransI=mm, pvalue=pp)
      morans.stat$Neighbors <- 1:nb.max
    }else if(class(parallel)=="numeric"){
      sac.parallel.func <- function(i){
        knn <- knearneigh(coords, k=i, longlat=longlat)
        nb <- knn2nb(knn)
        listw <- nb2listw(nb, style="W", zero.policy=TRUE)
        m <- moran.test(dat$al.residuals, listw, randomisation=TRUE)
        return(c(m$estimate[1], m$p.value))
      }
      cl <- makeCluster(parallel)
      registerDoParallel(cl)
      morans.stat <- foreach(i=1:nb.max, .packages=c("spdep"), .combine='rbind') %dopar% {
        sac.parallel.func(i=i)
      } 
      stopCluster(cl)
      morans.stat <- as.data.frame(morans.stat)
      names(morans.stat) <- c("MoransI", "pvalue")
      morans.stat$Neighbors <- 1:nb.max
    }
    
    morans.stat.initial.al <- rbind(morans.stat.initial, morans.stat)
    
    # Plot SAC
    
    rownames(morans.stat.initial.al) <- 1:nrow(morans.stat.initial.al)
    
    morans.stat.initial.al$Model <- c(rep("Initial",nb.max), rep("AL",nb.max))
    
    p <- ggplot(data=morans.stat.initial.al, aes(x=Neighbors, y=MoransI, col=Model)) +
      geom_line() +
      geom_point(aes()) +
      scale_color_brewer(palette="Set1") +
      theme_classic() +
      theme(panel.border=element_rect(colour="black", fill=NA), 
            panel.grid.major=element_line(colour="grey", linetype="dotted"),
            axis.text.y=element_text(angle=0, hjust=0.5)) +
      geom_hline(yintercept=0, linetype="dashed")
    
  }else if(spatial=="RAC"){
    
    dat$residualcovariate <- stats::lag(listw, dat$initial.residuals)
    
    # RAC model
    rac.brt <- gbm.step(dat, 
                       gbm.x=c(index.predictors,which(names(dat)=="residualcovariate")), 
                       gbm.y=index.response,
                       tree.complexity=tree.complexity,
                       learning.rate=learning.rate, 
                       bag.fraction=bag.fraction,
                       family=family,
                       max.trees=max.trees,
                       keep.data=TRUE,  
                       keep.fold.models=TRUE, 
                       keep.fold.vector=TRUE, 
                       keep.fold.fit=TRUE,
                       prev.stratify=prev.stratify,
                       step.size=step.size)
    
    dat$rac.prediction <- predict(object=rac.brt, newdata=dat, n.trees=rac.brt$n.trees, type="response")
    
    dat$rac.residuals <- dat[,index.response]-dat$rac.prediction
    
    # Do in parallel if wanted
    if(parallel=="NONE"){
      mm <- c()
      pp <- c()
      for(i in 1:nb.max){
        knn <- knearneigh(coords, k=i, longlat=longlat)
        nb <- knn2nb(knn)
        listw <- nb2listw(nb, style="W", zero.policy=TRUE)
        m <- moran.test(dat$rac.residuals, listw, randomisation=TRUE)
        mm <- c(mm, m$estimate[1])
        pp <- c(pp, m$p.value)
      }
      morans.stat <- data.frame(MoransI=mm, pvalue=pp)
      morans.stat$Neighbors <- 1:nb.max
    }else if(class(parallel)=="numeric"){
      sac.parallel.func <- function(i){
        knn <- knearneigh(coords, k=i, longlat=longlat)
        nb <- knn2nb(knn)
        listw <- nb2listw(nb, style="W", zero.policy=TRUE)
        m <- moran.test(dat$rac.residuals, listw, randomisation=TRUE)
        return(c(m$estimate[1], m$p.value))
      }
      cl <- makeCluster(parallel)
      registerDoParallel(cl)
      morans.stat <- foreach(i=1:nb.max, .packages=c("spdep"), .combine='rbind') %dopar% {
        sac.parallel.func(i=i)
      } 
      stopCluster(cl)
      morans.stat <- as.data.frame(morans.stat)
      names(morans.stat) <- c("MoransI", "pvalue")
      morans.stat$Neighbors <- 1:nb.max
    }
    
    morans.stat.initial.rac <- rbind(morans.stat.initial, morans.stat)
    
    # Plot SAC
    
    rownames(morans.stat.initial.rac) <- 1:nrow(morans.stat.initial.rac)
    
    morans.stat.initial.rac$Model <- c(rep("Initial",nb.max), rep("RAC",nb.max))
    
    p <- ggplot(data=morans.stat.initial.rac, aes(x=Neighbors, y=MoransI, col=Model)) +
      geom_line() +
      geom_point() +
      scale_color_brewer(palette="Set1") +
      theme_classic() +
      theme(panel.border=element_rect(colour="black", fill=NA), 
            panel.grid.major=element_line(colour="grey", linetype="dotted"),
            axis.text.y=element_text(angle=0, hjust=0.5)) +
      geom_hline(yintercept=0, linetype="dashed")
    
  }

  
  if(spatial=="Both"){
    results <- list(initial.model=initial.brt,
                  al.model=al.brt,
                  rac.model=rac.brt,
                  sac.plot=p)
  }else if(spatial=="AL"){
    results <- list(initial.model=initial.brt,
                    al.model=al.brt,
                    sac.plot=p)
  }else if(spatial=="RAC"){
    results <- list(initial.model=initial.brt,
                    rac.model=rac.brt,
                    sac.plot=p)
  }
  
  return(results)
  
}


