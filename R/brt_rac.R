## Calibrating BRT model with residual autocovariate term
## Reference: Crase et al. 2012, Ecography, 35, 879-888
## Author: Cornelius Senf (Humboldt-Universit?t zu Berlin)
## Contact: corneliussenf@gmail.com

brt.rac <- function(dat, 
                    index.predictors, 
                    index.response, 
                    index.coords,
                    nb.max=15,
                    nb.n=1,
                    parallel=NULL,
                    longlat=FALSE,
                    ...){
  
  require(dismo)
  require(spdep)
  require(ggplot2)
  require(doParallel)
  require(foreach)
  
  # Calculate intital BRT
  initial.brt <- gbm.step(dat, 
                          gbm.x=index.predictors, 
                          gbm.y=index.response,
                          ...)
  
  dat$initial.residuals <- initial.brt$residuals
  
  # Calculate spatial auto-correlation in residuals (currently only knn)
  coords <- as.matrix(dat[,index.coords])
  
  # Do in parallel if wanted
  if(is.null(parallel)){
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
    morans.stat.initial <- as.data.frame(morans.stat)
    names(morans.stat.initial) <- c("MoransI", "pvalue")
    morans.stat.initial$Neighbors <- 1:nb.max
    morans.stat.initial$Model <- "Initial"
  }
  
  knn <- knearneigh(coords, k=nb.n, longlat=longlat)
  nb <- knn2nb(knn)
  listw <- nb2listw(nb, style="W", zero.policy=TRUE)
  dat$residualcovariate <- stats::lag(listw, dat$initial.residuals)
  
  # RAC model
  rac.brt <- gbm.step(dat, 
                      gbm.x=c(index.predictors,which(names(dat)=="residualcovariate")), 
                      gbm.y=index.response,
                      ...)
    
  dat$rac.residuals <- rac.brt$residuals
    
  # Do in parallel if wanted
  if(is.null(parallel)){
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
    morans.stat.rac <- as.data.frame(morans.stat)
    names(morans.stat.rac) <- c("MoransI", "pvalue")
    morans.stat.rac$Neighbors <- 1:nb.max
    morans.stat.rac$Model <- "RAC"
  }
    
  morans.stat <- rbind(morans.stat.initial, morans.stat.rac)
    
  # Plot SAC
  
  p <- ggplot(data=morans.stat, aes(x=Neighbors, y=MoransI, col=Model)) +
    geom_line() +
    geom_point() +
    scale_color_brewer(palette="Set1") +
    theme_classic() +
    theme(panel.border=element_rect(colour="black", fill=NA), 
          panel.grid.major=element_line(colour="grey", linetype="dotted"),
          axis.text.y=element_text(angle=0, hjust=0.5)) +
    geom_hline(yintercept=0, linetype="dashed")
  
  results <- list(initial.model=initial.brt,
                  rac.model=rac.brt,
                  sac.plot=p)
  
  return(results)
  
}


