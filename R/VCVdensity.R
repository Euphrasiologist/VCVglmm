#' Visualise the VCV of a MCMCglmm model
#'
#' @param mod model of class MCMCglmm
#' @keywords MCMCglmm, posterior modes
#' @export
#' @examples
#' # requires ggplot2
#' library(ggplot2)
#' model <- MCMCglmm(y ~ x, random = ~ z)
#' VCVdensity(model)

VCVdensity <- function(mod){
  
  if(attributes(mod)$class != "MCMCglmm"){
    stop("Object not of class MCMCglmm")
  }
  
  ref <- data.frame(mod$VCV)
  rdf <- reshape2::melt(ref)
  ggplot(rdf, aes(x=value, fill=variable))+geom_density(alpha=0.3)
}