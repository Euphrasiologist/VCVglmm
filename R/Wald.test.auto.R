#' Performing Wald tests on MCMCglmm fixed effects automatically
#' 
#' @param mod model of class MCMCglmm
#' @keywords MCMCglmm, fixed effects, p-values, Wald test
#' @importFrom aod wald.test
#' @export
#' @examples
#' # needs aod
#' library(aod)
#' mod <-  MCMCglmm(y ~ x, random = ~ z)
#' # if x is a 4 levelled factor
#' Wald.test(mod = mod)



Wald.test.auto <- function(mod){
  
  if(attributes(mod)$class != "MCMCglmm"){
    stop("Object not of class MCMCglmm")
  }
  
  # model formula fixed effects
  fixeff <- as.character(mod$Fixed$formula)[3]
  fixeff2 <- unlist(strsplit(fixeff, "\\+"))
  
  # individual covariates
  covars <- gsub(fixeff2, pattern = " ", replacement = "")
  
  # if intercept is explicitly present, remove it
  if(any(covars == "1")){
    sset <- grep(pattern = "[^1]", covars)
    covars <- covars[sset]
  }
  
  
  res <- list()
  for(i in 1: length(covars)){
    # grab the numbers of the levels to test
    fixed <- grep(x = rownames(summary(mod)$solutions), pattern = covars[i])
    
    # define the varcov
    varcov <- cov(mod$Sol[, fixed, drop = FALSE])
    # means of the posterior for the fixed effects
    coefs <- colMeans(mod$Sol[, fixed, drop = FALSE])
    # how many terms to test
    Terms <- 1:length(fixed)
    # requires library(aod)
    res[[i]] <- wald.test(Sigma = varcov, b = coefs, Terms = Terms)$result
    names(res[[i]]) <- covars[i]
  }
  
  
  return(data.frame(res))
}
