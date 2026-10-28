#' Performing Wald tests on MCMCglmm fixed effects
#'
#' Jointly tests whether a set of fixed effects are all zero, using the posterior means and
#' posterior covariance matrix of the effects: \eqn{\chi^2 = b' \Sigma^{-1} b} on
#' \code{length(fixed)} degrees of freedom.
#'
#' @param mod model of class MCMCglmm
#' @param fixed numeric vector specifying terms to be tested. In summary(mod), in location effects, the intercept is 1. Jointly test a factor by including all levels (minus intercept).
#' @return named vector with the chi-squared statistic, degrees of freedom and p-value
#' @keywords MCMCglmm, fixed effects, p-values, Wald test
#' @export
#' @examples
#' \donttest{
#' library(MCMCglmm)
#' d <- data.frame(x = factor(rep(letters[1:4], 50)))
#' d$y <- as.numeric(d$x) + rnorm(200)
#' mod <- MCMCglmm(y ~ x, data = d, verbose = FALSE)
#' # x is a 4 levelled factor
#' Wald.test(mod = mod, fixed = 2:4)
#' }

Wald.test <- function(mod, fixed){

  if(!inherits(mod, "MCMCglmm")){
    stop("Object not of class MCMCglmm")
  }
  fixed <- as.numeric(fixed)
  .wald(mod$Sol[, fixed, drop = FALSE])
}

# chi-squared Wald test that all columns of the posterior sample x have mean zero
.wald <- function(x){
  varcov <- stats::cov(as.matrix(x))
  coefs <- colMeans(as.matrix(x))
  chi2 <- as.numeric(t(coefs) %*% solve(varcov, coefs))
  df <- length(coefs)
  c(chi2 = chi2, df = df, P = stats::pchisq(chi2, df, lower.tail = FALSE))
}
