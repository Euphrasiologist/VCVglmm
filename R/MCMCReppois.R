#' Calculate variance explained by a random variable (repeatability or Intra-Class Coefficient) on the link scale
#'
#' @param mod model of class MCMCglmm
#' @param y character vector of length 1, identical to name of random effect.
#' @keywords MCMCglmm, repeatability, ICC, variance explained
#' @export
#' @examples
#' model <- MCMCglmm(y ~ x, random = ~ z)
#' MCMCReppois(model, "z")


MCMCReppois<-function(mod, y = "variable"){
  
  if(attributes(mod)$class != "MCMCglmm"){
    stop("Model must be of class MCMCglmm!")
  }
  var.a <- mod$VCV[,y]
  var.e <- rowSums(mod$VCV)
  beta0 <- sapply(1:dim(mod$Sol)[1],function(z) mean(as.matrix(mod$X)%*%as.matrix(mod$Sol[z,1:ncol(mod$X)])))
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