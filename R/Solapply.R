#' Apply a function over each of the stored posterior solutions from a MCMCglmm Model
#'
#' @param model model of class MCMCglmm
#' @param FUN function name to apply over object, default is posterior mode, suggested also is coda::HPDinterval
#' @keywords MCMCglmm, posterior modes
#' @import coda
#' @import data.table
#' @export
#' @examples
#' # requires data.table
#' library(data.table)
#' model <- MCMCglmm(y ~ x, random = ~ z)
#' Solapply(model, coda::HPDinterval)

Solapply <- function(model, FUN = posterior_mode, ...){
  
  if(attributes(model)$class != "MCMCglmm"){
    stop("Object not of class MCMCglmm")
  }
  
  if(dim(model$Sol)[2] <= model$Fixed$nfl){
    stop("Re-run the model with parameter option pr = TRUE")
  } 
  # adapted from MCMCglmm::posterior.mode()
  posterior_mode<-function(x, adjust=0.1, ...){
    find.mode<-function(x,adjust,...){
      dx<-density(x, adjust=adjust, ...)
      dx$x[which.max(dx$y)]
    }
    apply(as.matrix(x), 2, find.mode, adjust=adjust, ...)
  }
  
  FUN <- match.fun(FUN)
  
  # special case for HPDinterval
  if(identical(FUN, HPDinterval)){
    temp <- setDT(as.data.frame(HPDinterval(model$Sol)), keep.rownames = TRUE)[-c(1:model$Fixed$nfl)]
    temp$posterior.mode <- as.data.frame(apply(model$Sol[,-c(1:model$Fixed$nfl)], 2, posterior_mode))[,1]
    colnames(temp) <- c("Variable", "lowerHPD", "upperHPD", "Posterior Mode")
    temp$Group <- gsub("\\..*", "", temp$Variable)
    return(temp)
  }
  
  temp <- setDT(as.data.frame(apply(model$Sol[,-c(1:model$Fixed$nfl)], 2, FUN)), keep.rownames = TRUE)
  colnames(temp) <- c("Variable", "Grouped_Value")
  temp$Group <- gsub("\\..*", "", temp$Variable)
  return(temp)
}