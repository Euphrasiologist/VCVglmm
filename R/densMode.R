#' Calculate x and y axis for peak density on MCMC objects (http://ianmadd.github.io/pages/PeakDensityDistribution.html)
#' 
#' @param x a vector of values from which to calculate a density
#' @keywords MCMCglmm, repeatability, ICC, variance explained
#' @export
#' @examples
#' x <- c(1,2,3,2,2,2,2,1,2,12,2,2)
#' densMode(x)

densMode <- function(x){
  td <- density(x)
  maxDens <- which.max(td$y)
  maxx <- which.max(td$x)
  list(x=td$x[maxDens], y=td$y[maxDens], max.x=td$x[maxx])
}