#' Calculate variance explained by a random variable (repeatability or Intra-Class Coefficient) on the link scale
#'
#' Uses the Poisson distribution-specific variance \eqn{\log(1/\exp(\beta_0) + 1)}. See
#' \code{\link{glmm_varpart}} for the latent, link and data scales.
#'
#' @param mod model of class MCMCglmm
#' @param y character vector of length 1, identical to name of random effect.
#' @keywords MCMCglmm, repeatability, ICC, variance explained
#' @export
#' @examples
#' \dontrun{
#' model <- MCMCglmm(y ~ x, random = ~ z)
#' MCMCReppois(model, "z")
#' }


MCMCReppois<-function(mod, y = "variable"){
  
  if(attributes(mod)$class != "MCMCglmm"){
    stop("Model must be of class MCMCglmm!")
  }
  var.a <- mod$VCV[,y]
  var.e <- rowSums(mod$VCV[, .MCMC_variance_cols(mod), drop = FALSE])
  # mean of the fixed effect linear predictor for each sample
  beta0 <- as.vector(as.matrix(mod$Sol[, 1:ncol(mod$X)]) %*% Matrix::colMeans(mod$X))
  postR.link <- var.a/(var.e+log(1/exp(beta0)+1))
  R.link <- posterior.mode( postR.link )
  CI.link <- coda::HPDinterval(postR.link)[1,]
  res <- list(R.link=R.link, CI.link=CI.link)
  res.2 <- unlist(res)*100
  names(res.2) <- c("Link Scale Point Estimate", "CI Link Scale Lower", "CI Link Scale Upper")
  res.2 <- data.frame(res.2)
  colnames(res.2)[1] <- "%"
  return(res.2)
}