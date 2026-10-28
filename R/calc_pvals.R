#' Calculating p-values for the lme4 Gaussian mixed model.
#' 
#' P-values are not reported for this model type due to difficulties in estimating degrees of freedom in these types of model and the fact that it is not known for sure whether the t-values do indeed follow the t distribution. 
#' With that in mind, the following function should be used with caution. If we accept that the t-value will at least approximately follow the t-distribution, then degrees of freedom must lie between the highest order grouping variable
#' in the data and the number of observations themselves.
#'
#' Please see https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html and https://stat.ethz.ch/pipermail/r-help/2006-May/094765.html for detailed information.
#' 
#' Personal observations are that the results from this function lie broadly in line with output from MCMCglmm, at least for simple models.
#' 
#' @param lmermod model of class lmerMod
#' @keywords lme4, fixed effects, p-values
#' @importFrom data.table as.data.table
#' @importFrom stats nobs
#' @import lme4
#' @export
#' @examples
#' # needs data.table
#' library(data.table)
#' library(lme4)
#' mod <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
#' calc_pvals(mod)

calc_pvals <- function(lmermod){
  if(inherits(lmermod, "lmerMod")){
    dat<-summary(lmermod)$coefficients
    
    dat <- data.table::as.data.table(as.data.frame(dat), keep.rownames = T)
    colnames(dat)[1] <- "Levels"
    
    # two-sided p-values, bracketing the unknown denominator degrees of freedom
    dat$pval_upperdf <- 2 * stats::pt(q = abs(dat$`t value`), df = stats::nobs(lmermod), lower.tail = FALSE)
    dat$pval_lowerdf <- 2 * stats::pt(q = abs(dat$`t value`), df = sum(summary(lmermod)$ngrps), lower.tail = FALSE)
    
    return(dat)
  } else stop("Model not of class lmerMod")
  
}

