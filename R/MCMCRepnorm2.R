#' Calculate variance explained by a random variable (repeatability or Intra-Class Coefficient) on the data scale for Gaussian models
#'
#' Each variance in the VCV is divided by the sum of all variances (covariances are excluded).
#' See \code{\link{glmm_varpart}} for a more general version (other families, random slopes,
#' latent/link/data scales).
#'
#' @param mod model of class MCMCglmm
#' @keywords MCMCglmm, repeatability, ICC, variance explained
#' @export
#' @examples
#' \dontrun{
#' model <- MCMCglmm(y ~ x, random = ~ z)
#' MCMCRepnorm2(model)
#' }

MCMCRepnorm2 <- function(mod){
  
  if(attributes(mod)$class != "MCMCglmm"){
    stop("Model must be of class MCMCglmm!")
  }
  
  y <- colnames(mod$VCV)[.MCMC_variance_cols(mod)]

  var.a <- list()
  for(i in 1:length(y)){
    var.a[[paste0(y[i])]] <- mod$VCV[,y[i] ]}

  var.e <- rowSums(mod$VCV[, y, drop = FALSE])
  
  postR.link <- lapply(var.a, function(x) x/var.e)
  
  R.link <- lapply(postR.link, function(x) posterior.mode(x))
  CI <- lapply(postR.link, function(x) coda::HPDinterval(x))
  
  res<- data.frame(Variance.Explained = unlist(R.link))
  rownames(res) <- gsub(pattern = ".var1", replacement =  "", x = rownames(res))
  colnames(res) <- "% Var explained"
  print(res*100)
  CI <- lapply(CI, function(x) x*100)
  print(CI)
  invisible(list(R = res*100, CI = CI))
}
