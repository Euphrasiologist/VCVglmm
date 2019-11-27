#' ggplot2 output of the fixed effects from a MCMCglmm model
#' 
#' @param mod model of class MCMCglmm
#' @keywords MCMCglmm, fixed effects, plot
#' @import ggplot2
#' @export
#' @examples
#' # requires ggplot2
#' library(ggplot2)
#' mod <- MCMCglmm(y ~ x1 + x2, random =  ~ z)
#' MCMCfixplot(mod)

MCMCfixplot <- function(mod){
  
  if(attributes(mod)$class != "MCMCglmm"){
    stop("Model must be of class MCMCglmm!")
  }
  
  mod$Sol <- mod$Sol[,c(1:mod$Fixed$nfl)]
  rsum<-cbind(B = posterior.mode(mod$Sol[,1:dim(mod$Sol)[2]]), 
              CI = HPDinterval(mod$Sol[,1:dim(mod$Sol)[2]]))
  # put them in a data frame
  rsum<-data.frame(rsum)
  # create a vector of host names
  rsum$variable<- c(rownames(rsum))
  
  # find p values
  summ <- summary(mod)
  pvals <- summ$solutions[,5]
  # add p values to data frame
  rsum$pvals <- pvals
  
  # create a loop to assign asterisks to certain significance levels
  rsum$ptext <- rep(NA, dim(rsum)[1])
  for(i in 1:dim(rsum)[1]){
    if(rsum$pvals[i] <= 0.05 & rsum$pvals[i] > 0.01){
      rsum$ptext[i] <- "*"
    } else if(rsum$pvals[i] <= 0.01 & rsum$pvals[i] > 0.001){
      rsum$ptext[i] <- "**"
    } else if(rsum$pvals[i] <= 0.001 & rsum$pvals[i] > 0){
      rsum$ptext[i] <- "***"
    } else rsum$ptext[i] <- NA
  }
  
  # plot the graph with the confidence intervals, and add asterisks
  ggplot(rsum,aes(x = variable , y = B))+
    geom_pointrange(aes(ymin=lower,
                        ymax=upper))+
    geom_text(aes(label = ptext), nudge_x = 0.2) +
    coord_flip()+
    theme_bw()+
    labs(y = "Posterior modes", x = "Variables")
}