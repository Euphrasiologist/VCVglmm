#' Calculate variance explained from a lme4 model
#'
#' @param mod model of class (g)lmerMod
#' @keywords lme4, variance explained, repeatability
#' @export
#' @examples
#' mod <- lmer(y ~ x + (1 | z))
#' VarExpl.mer(mod)

VarExpl.mer <- function(mod){
    
    if(class(mod)[1] == "glmerMod" | class(mod)[1] == "lmerMod"){
      
      if(grepl(names(unlist(mod@cnms)), pattern = "Obs")[1] == FALSE){
        stop("Please fit an observation level random effect, called Obs, as last random effect in model formula")
      }
        
      ranvar <- data.frame(unlist(summary(mod)$varcor))
      colnames(ranvar) <- "Var"
      test <- vector()
      for(i in 1:dim(ranvar)[1])
        test[i] <- c(ranvar[i,]/sum(ranvar$Var))
      
      ranvar$"% Var Explained" <- test*100
    } else stop("Object not (g)lmerMod, from package:lme4")
  print(ranvar)
}