#' Diagnostics for lme4 mixed models
#'
#' Runs a set of checks on a fitted (g)lmer model and lists potential problems in plain English:
#' \itemize{
#'   \item convergence warnings from the optimiser, and the scaled gradient at the optimum;
#'   \item singular fits (variances estimated at zero, or correlations at +/- 1);
#'   \item grouping factors with few levels (< 5), for which variances are poorly estimated;
#'   \item overdispersion (Pearson chi-squared / residual df) for Poisson and binomial models;
#'   \item strongly correlated fixed-effect estimates;
#'   \item very large fixed-effect standard errors (separation, in binomial models).
#' }
#'
#' @param mod model of class lmerMod or glmerMod
#' @param max.cor fixed-effect estimate correlations above this (absolute value) are reported
#' @param print logical, print the results?
#' @return invisibly, a data.frame of checks with \code{attr(x, "issues")}
#' @keywords lme4, diagnostics, singular, overdispersion
#' @seealso \code{\link{merdiagplot}}
#' @export
#' @examples
#' \donttest{
#' library(lme4)
#' mod <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
#' merdiag(mod)
#' }

merdiag <- function(mod, max.cor = 0.9, print = TRUE){
  .check_merMod(mod)
  issues <- character(0)
  checks <- list()

  # convergence
  msgs <- c(mod@optinfo$conv$lme4$messages, mod@optinfo$warnings)
  msgs <- unique(unlist(lapply(msgs, as.character)))
  grad <- tryCatch({
    dd <- mod@optinfo$derivs
    if(is.null(dd)) NA else max(abs(solve(chol(dd$Hessian), dd$gradient)))
  }, error = function(e) NA)
  checks$convergence <- c(value = if(length(msgs)) paste(msgs, collapse = "; ") else "OK",
                          note = sprintf("max |scaled gradient| = %s", signif(grad, 3)))
  if(length(msgs) || (!is.na(grad) && grad > 0.001)){
    issues <- c(issues, sprintf("Convergence problems (%s). Try centring/scaling covariates, a different optimiser (control = %sControl(optimizer = \"bobyqa\")) or allFit() to check the estimates are stable.",
                                if(length(msgs)) paste(msgs, collapse = "; ") else sprintf("scaled gradient %s", signif(grad, 3)),
                                if(inherits(mod, "glmerMod")) "glmer" else "lmer"))
  }

  # singularity
  sing <- lme4::isSingular(mod)
  checks$singular <- c(value = if(sing) "yes" else "no", note = "")
  if(sing){
    vc <- as.data.frame(lme4::VarCorr(mod))
    zero <- vc$grp[is.na(vc$var2) & vc$vcov < 1e-8 & vc$grp != "Residual"]
    corr1 <- vc[!is.na(vc$var2) & abs(vc$sdcor) > 0.999, ]
    what <- c(if(length(zero)) sprintf("zero variance for %s", paste(unique(zero), collapse = ", ")),
              if(nrow(corr1)) sprintf("correlation of +/-1 between %s", paste(sprintf("%s:%s (%s)", corr1$var1, corr1$var2, corr1$grp), collapse = ", ")))
    issues <- c(issues, sprintf("Singular fit%s. The random effects structure is too complex for the data; simplify it (e.g. drop the correlation or the term), or use a Bayesian model with a weakly informative prior.",
                                if(length(what)) paste0(" (", paste(what, collapse = "; "), ")") else ""))
  }

  # number of levels
  ng <- lme4::ngrps(mod)
  checks$levels <- c(value = paste(sprintf("%s: %d", names(ng), ng), collapse = ", "), note = "")
  few <- ng[ng < 5]
  if(length(few)){
    issues <- c(issues, sprintf("Grouping factor(s) with fewer than 5 levels (%s): their variance is poorly estimated; consider fitting them as fixed effects.",
                                paste(sprintf("%s = %d", names(few), few), collapse = ", ")))
  }

  # overdispersion
  fam <- .mer_family(mod)
  if(inherits(mod, "glmerMod") && fam %in% c("poisson", "logit", "probit")){
    rp <- stats::residuals(mod, type = "pearson")
    rdf <- stats::df.residual(mod)
    ratio <- sum(rp^2) / rdf
    p <- stats::pchisq(sum(rp^2), rdf, lower.tail = FALSE)
    checks$dispersion <- c(value = sprintf("%.2f", ratio), note = sprintf("p = %s", format.pval(p, digits = 2)))
    binary <- fam != "poisson" && all(stats::weights(mod) == 1)
    if(ratio > 1.2 && p < 0.05 && !binary){
      issues <- c(issues, sprintf("Overdispersion: Pearson chi2/df = %.2f (p = %s). Standard errors are too small; add an observation-level random effect (1 | obs)%s.",
                                  ratio, format.pval(p, digits = 2),
                                  if(fam == "poisson") " or use a negative binomial model (glmer.nb)" else ""))
    }
  }

  # fixed effect correlations and standard errors
  V <- as.matrix(stats::vcov(mod))
  if(ncol(V) > 1){
    cm <- stats::cov2cor(V)
    hi <- which(abs(cm) > max.cor & upper.tri(cm), arr.ind = TRUE)
    if(nrow(hi)){
      issues <- c(issues, sprintf("Strongly correlated fixed-effect estimates: %s. Consider centring covariates or check for collinearity.",
                                  paste(sprintf("%s & %s (r = %.2f)", rownames(cm)[hi[, 1]], colnames(cm)[hi[, 2]], cm[hi]), collapse = "; ")))
    }
  }
  se <- sqrt(diag(V))
  if(fam %in% c("logit", "probit") && any(se > 10)){
    issues <- c(issues, sprintf("Very large standard errors for %s: possible (quasi-)complete separation.",
                                paste(names(se)[se > 10], collapse = ", ")))
  }

  res <- data.frame(check = names(checks),
                    value = vapply(checks, `[[`, character(1), "value"),
                    note = vapply(checks, `[[`, character(1), "note"),
                    row.names = NULL, stringsAsFactors = FALSE)
  attr(res, "issues") <- issues
  if(print){
    print(res, row.names = FALSE)
    cat("\n")
    if(length(issues) == 0){
      cat("No problems detected. Check the residual plots too: merdiagplot().\n")
    } else {
      cat("Potential problems:\n")
      for(i in issues) cat(paste(strwrap(i, width = getOption("width") - 4, prefix = "    ", initial = "  - "), collapse = "\n"), "\n")
    }
  }
  invisible(res)
}

#' Diagnostic plots for lme4 mixed models
#'
#' Returns a list of ggplot2 plots:
#' \describe{
#'   \item{fitted}{Pearson residuals against fitted values with a smoother: the smoother should be flat
#'   (otherwise the mean structure is mis-specified), and the spread constant.}
#'   \item{qq}{normal QQ plot of the residuals (most relevant for Gaussian models).}
#'   \item{scale}{scale-location plot: sqrt(|residuals|) against fitted values, to check for
#'   heteroscedasticity.}
#'   \item{ranef}{normal QQ plots of the predicted random effects (conditional modes), per term:
#'   strong departures suggest outlying groups or a non-normal random effect distribution.}
#' }
#'
#' @param mod model of class lmerMod or glmerMod
#' @return a named list of ggplot objects; printing it draws all of them
#' @keywords lme4, diagnostics, residuals, plot
#' @import ggplot2
#' @export
#' @examples
#' \donttest{
#' library(lme4)
#' mod <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
#' p <- merdiagplot(mod)
#' p$fitted
#' p$ranef
#' }

merdiagplot <- function(mod){
  .check_merMod(mod)
  d <- data.frame(fitted = stats::fitted(mod),
                  resid = stats::residuals(mod, type = "pearson"))
  out <- list()
  out$fitted <- ggplot(d, aes(x = fitted, y = resid)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "red") +
    theme_bw() + labs(x = "Fitted values", y = "Pearson residuals", title = "Residuals vs fitted")
  out$qq <- ggplot(d, aes(sample = resid)) +
    stat_qq(alpha = 0.5) + stat_qq_line(colour = "red") +
    theme_bw() + labs(x = "Theoretical quantiles", y = "Pearson residuals", title = "Normal QQ (residuals)")
  out$scale <- ggplot(d, aes(x = fitted, y = sqrt(abs(resid)))) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "red") +
    theme_bw() + labs(x = "Fitted values", y = "sqrt(|Pearson residuals|)", title = "Scale-location")
  re <- lme4::ranef(mod)
  rl <- do.call(rbind, lapply(names(re), function(g){
    do.call(rbind, lapply(names(re[[g]]), function(eff){
      data.frame(term = paste0(g, ": ", eff), value = re[[g]][[eff]], stringsAsFactors = FALSE)
    }))
  }))
  out$ranef <- ggplot(rl, aes(sample = value)) +
    stat_qq(alpha = 0.6) + stat_qq_line(colour = "red") +
    facet_wrap(~ term, scales = "free") +
    theme_bw() + labs(x = "Theoretical quantiles", y = "Conditional modes", title = "Normal QQ (random effects)")
  class(out) <- "merdiagplot"
  out
}

#' @export
print.merdiagplot <- function(x, ...){
  for(p in unclass(x)) print(p)
  invisible(x)
}
