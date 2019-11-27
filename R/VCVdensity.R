#' Visualise the VCV of a MCMCglmm model
#'
#' @param mod model of class MCMCglmm
#' @keywords MCMCglmm, posterior modes
#' @import ggplot2
#' @import data.table
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
  
  ref <- data.table(mod$VCV)
  rdf <- suppressWarnings(melt(ref))
  ggplot(rdf, aes(x=value, fill=variable))+geom_density(alpha=0.3)
}