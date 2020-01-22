#' Posterior standard deviations for fixed (and random) effects
#' 
#' @param model model of class MCMCglmm
#' @param pr logical. Should the posterior standard deviations of the random effects be calculated? Option pr = TRUE must be in the MCMCglmm model.
#' 
#' @keywords MCMCglmm, fixed effects, random effects, standard deviation
#' @import data.table
#' @export


posterior.sd <- function(model, pr = FALSE){
  
  if(attributes(model)$class != "MCMCglmm"){
    stop("Model must be of class MCMCglmm!")
  }
  
  v <- var(model$Sol)
  sdev <- sqrt(diag(v))
  sdev <- as.data.table(sdev, keep.rownames = TRUE)
  
  if(pr){
    sdev
  } else {
    sdev[1:model$Fixed$nfl]
  }
}