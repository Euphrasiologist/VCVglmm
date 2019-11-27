#' Performing Wald tests on MCMCglmm fixed effects
#' 
#' @param mod model of class MCMCglmm
#' @param fixed numeric vector specifying terms to be tested. In summary(mod), in location effects, the intercept is 1. Jointly test a factor by including all levels (minus intercept).
#' @keywords MCMCglmm, fixed effects, p-values, Wald test
#' @import aod wald.test
#' @export
#' @examples
#' # needs aod
#' library(aod)
#' mod <-  MCMCglmm(y ~ x, random = ~ z)
#' # if x is a 4 levelled factor
#' Wald.test(mod = mod, fixed = 2:4)


Wald.test <- function(mod, fixed){
  
  if(attributes(mod)$class != "MCMCglmm"){
    stop("Object not of class MCMCglmm")
  }
  # numeric
  fixed <- as.numeric(fixed)
  # define the varcov
  varcov <- cov(mod$Sol[, fixed, drop = FALSE])
  # means of the posterior for the fixed effects
  coefs <- colMeans(mod$Sol[, fixed, drop = FALSE])
  # how many terms to test
  Terms <- length(fixed)
  # requires library(aod)
  res <- wald.test(Sigma = varcov, b = coefs, Terms = Terms)
  
  return(res$result)
}