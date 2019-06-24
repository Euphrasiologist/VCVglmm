#' Calculating p-values for the annoying lme4 Gaussian mixed model
#' 
#' @param lmermod model of class lmerMod
#' @keywords lme4, fixed effects, p-values
#' @export
#' @examples
#' # needs data.table
#' library(data.table)
#' mod <- lmer(y ~ x + (1 | z))
#' calc_pvals(mod)

calc_pvals <- function(lmermod){
  if(attributes(lmermod)$class == "lmerMod"){
    dat<-summary(lmermod)$coefficients
    
    dat <- data.table::as.data.table(as.data.frame(dat), keep.rownames = T)
    colnames(dat)[1] <- "Levels"
    
    dat$pval_upperdf <- 1-pt(q = dat$`t value`, df = stats::nobs(lmermod))
    dat$pval_lowerdf <- 1-pt(q = dat$`t value`, df = summary(lmermod)$ngrps)
    
    return(dat)
  } else stop("Model not of class lmerMod")
  
}
