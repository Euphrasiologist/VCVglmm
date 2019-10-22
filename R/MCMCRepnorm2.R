#' Calculate variance explained by a random variable (repeatability or Intra-Class Coefficient) on the data scale for Gaussian models
#' 
#' @param mod model of class MCMCglmm
#' @keywords MCMCglmm, repeatability, ICC, variance explained
#' @export
#' @examples
#' model <- MCMCglmm(y ~ x, random = ~ z)
#' MCMCRepnorm2(model, "z")

MCMCRepnorm2 <- function(mod){
  
  if(attributes(mod)$class != "MCMCglmm"){
    stop("Model must be of class MCMCglmm!")
  }
  # extract variables
  y <- c(all.vars(mod$Random$formula), "units")
  
  var.a <- list()
  var.e <- rowSums(mod$VCV)
  
  for(i in 1:length(y)){
    var.a[[paste0(y[i])]] <- mod$VCV[,y[i] ]}
  
  var.e <- rowSums(mod$VCV)
  
  postR.link <- lapply(var.a, function(x) x/var.e)
  
  R.link <- lapply(postR.link, function(x) posterior.mode(x))
  CI <- lapply(postR.link, function(x) coda::HPDinterval(x))
  
  res<- data.frame(Variance.Explained = unlist(R.link))
  rownames(res) <- gsub(pattern = ".var1", replacement =  "", x = rownames(res))
  colnames(res) <- "% Var explained"
  print(res*100)
  print(lapply(CI, function(x) x*100))
}
