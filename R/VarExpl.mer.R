#' Calculate variance explained from a lme4 model
#'
#' Proportion of variance explained by each variance component (and the residual, for lmer
#' models). For glmer models an observation-level random effect called Obs must be fitted, which
#' then plays the role of the residual. For random slopes, the variance at covariate = 0 is used;
#' see \code{\link{glmm_varpart}} for a version that averages over covariate values and works on
#' the link and data scales too.
#'
#' @param mod model of class (g)lmerMod
#' @keywords lme4, variance explained, repeatability
#' @export
#' @examples
#' library(lme4)
#' mod <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
#' VarExpl.mer(mod)

VarExpl.mer <- function(mod){

    if(inherits(mod, "glmerMod") || inherits(mod, "lmerMod")){

      if(inherits(mod, "glmerMod") && !any(grepl(names(mod@cnms), pattern = "^Obs$"))){
        stop("Please fit an observation level random effect, called Obs, as last random effect in model formula")
      }

      vc <- as.data.frame(lme4::VarCorr(mod))
      # variances only (covariances have a second variable); includes the residual for lmer
      vc <- vc[is.na(vc$var2), ]
      ranvar <- data.frame(Var = vc$vcov,
                           row.names = ifelse(vc$grp == "Residual" | vc$var1 == "(Intercept)",
                                              vc$grp, paste(vc$grp, vc$var1, sep = ".")))

      ranvar$"% Var Explained" <- ranvar$Var / sum(ranvar$Var) * 100
    } else stop("Object not (g)lmerMod, from package:lme4")
  print(ranvar)
}
