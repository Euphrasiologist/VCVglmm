#' Visualise the random effect posterior distributions 
#'
#' @param mod model of class MCMCglmm
#' @param group logical, should the random effects be grouped?
#' @keywords MCMCglmm, posterior modes
#' @import data.table
#' @import ggplot2
#' @export
#' @examples
#' # requires data.table, ggplot2
#' library(data.table); library(ggplot2)
#' model <- MCMCglmm(y ~ x, random = ~ z)
#' MCMCranef(model)

MCMCranef <- function(mod, group = FALSE, data = FALSE){
  
  if(attributes(mod)$class != "MCMCglmm"){
    stop("Object not of class MCMCglmm")
  }
  
  if(dim(mod$Sol)[2] <= mod$Fixed$nfl){
    stop("Re-run the model with parameter option pr = TRUE")
  } 
  
  if(group == TRUE) {
    ref <- data.table(mod$Sol[,-c(1:mod$Fixed$nfl)])
    rdf <- suppressWarnings(melt(ref))
    rdf[, group := gsub("\\..*", "", variable)]
    
    xdat <- rdf[, `:=`(val_mean = signif(densMode(value)$x,3), med.x = signif(densMode(value)$x,3), med.y=signif(densMode(value)$y,3)), by = .(group)]
    xdat <-xdat[!duplicated(xdat$variable)]
    plot<-ggplot(rdf, aes(value, colour=group))+geom_density()+geom_label(data = xdat, aes(x=med.x,y=med.y,label=val_mean))
    
    if(data){
      return(xdat)
    }
      else{
        print(plot)
      }
    
  } else {
    ref <- data.table(mod$Sol[,-c(1:mod$Fixed$nfl)])
    
    rdf <- suppressWarnings(melt(ref))
    
    xdat <- rdf[, `:=`(val_mean = signif(densMode(value)$x,3), med.x = signif(densMode(value)$x,3), med.y=signif(densMode(value)$y,3)), by = .(variable)]
    xdat <-xdat[!duplicated(xdat$variable)]
    plot<-ggplot(rdf, aes(value, colour=variable))+geom_density()+geom_label(data = xdat, aes(x=med.x,y=med.y,label=val_mean))+theme(legend.position = "none")
    
    if(data){
      return(xdat)
    }
    else{
      print(plot)
    }
  }
}
