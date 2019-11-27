#' Lattice output of the MCMC chains from the fixed and random effects from a MCMCglmm model
#' 
#' @param mod model of class MCMCglmm
#' @keywords MCMCglmm, fixed effects, plot
#' @import lattice
#' @export
#' @examples
#' mod <- MCMCglmm(y ~ x1 + x2, random =  ~ z)
#' traces(mod)[[1]]; traces(mod)[[2]]

traces <- function(mod){
  
  if(attributes(mod)$class != "MCMCglmm"){
    stop("Model must be of class MCMCglmm!")
  }
  
  par.settings = list(strip.background=list(col="lightgrey"))
  trace.1<-xyplot(mod$Sol,
                  mean.list = mod$Sol,
                  panel = function(x, y, mean.list) {
                    panel.lines(x, y, col = "black")
                    panel.abline(h = mean(y),
                                 col.line = "red")
                  },
                  par.settings=par.settings)
  trace.2<-xyplot(mod$VCV,
                  mean.list = mod$VCV,
                  panel = function(x, y, mean.list) {
                    panel.lines(x, y)
                    panel.abline(h = mean(y),
                                 col.line = "red")
                  },
                  par.settings=par.settings)
  Newlist <- list(trace.1, trace.2)
  return(Newlist)
}