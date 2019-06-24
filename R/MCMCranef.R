#' Visualise the random effect posterior distributions 
#'
#' @param mod model of class MCMCglmm
#' @param group logical, should the random effects be grouped?
#' @keywords MCMCglmm, posterior modes
#' @export
#' @examples
#' # requires plyr, dplyr,reshape2, ggplot2
#' library(plyr); library(tidyverse)
#' model <- MCMCglmm(y ~ x, random = ~ z)
#' MCMCranef(model)

MCMCranef <- function(mod, group = FALSE){
  
  if(attributes(mod)$class != "MCMCglmm"){
    stop("Object not of class MCMCglmm")
  }
  
  if(dim(model$Sol)[2] <= model$Fixed$nfl){
    stop("Re-run the model with parameter option pr = TRUE")
  } 
  
  if(group == TRUE) {
    ref <- data.frame(mod$Sol[,-c(1:mod$Fixed$nfl)])
    
    rdf <- reshape2::melt(ref)
    
    rdf <- rdf %>%
      mutate(group= gsub("\\..*", "", variable))
    
    xdat <- ddply(rdf,"group", transform, val_mean = signif(densMode(value)$x,3), med.x = signif(densMode(value)$x,3), med.y=signif(densMode(value)$y,3))
    xdat <-xdat[!duplicated(xdat$variable),]
    plot<-ggplot(rdf, aes(value, colour=group))+geom_density()+geom_label(data = xdat, aes(x=med.x,y=med.y,label=val_mean))
    
    print(xdat);print(plot)
  } else {
    ref <- data.frame(mod$Sol[,-c(1:mod$Fixed$nfl)])
    
    rdf <- reshape2::melt(ref)
    
    xdat <- ddply(rdf,"variable", transform, val_mean = signif(densMode(value)$x,3), med.x = signif(densMode(value)$x,3), med.y=signif(densMode(value)$y,3))
    xdat <-xdat[!duplicated(xdat$variable),]
    plot<-ggplot(rdf, aes(value, colour=variable))+geom_density()+geom_label(data = xdat, aes(x=med.x,y=med.y,label=val_mean))+theme(legend.position = "none")
    
    print(xdat);print(plot)
  }
}