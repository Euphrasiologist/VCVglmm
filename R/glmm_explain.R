#' Explain a mixed model in plain English
#'
#' Writes a text summary of an MCMCglmm or lme4 model: what was fitted, whether the chain looks
#' healthy (MCMCglmm) or the fit has warnings (lme4), what each fixed effect means on the latent
#' (link) scale and on the data (response) scale, how much variance each random term explains on
#' the latent, link and data scales, and the marginal/conditional R-squared.
#'
#' Data-scale effects of fixed effects are evaluated with all other covariates at zero / their
#' reference level, and are "population averaged": the random effects (and for MCMCglmm the
#' \code{units} overdispersion) are integrated out. For a log link the ratio \code{exp(b)} is unaffected
#' by this, but differences in counts and all probabilities are. Intercepts and effects are
#' therefore easiest to interpret when continuous covariates are centred.
#'
#' Categorical responses with more than two categories get a section per contrast (log-odds and
#' odds ratios against the reference category) and a joint data-scale section: the probability of
#' each category at baseline, how each covariate changes those probabilities, and the variance in
#' each category indicator explained by each term.
#'
#' Multi-response MCMCglmm models get one section per trait (fixed effects and variance
#' partitioning, using that trait's family), followed by the between-trait correlations from
#' \code{\link{glmm_cor}}. For ordinal/threshold models with more than two categories, data-scale
#' effects are given as changes in the expected category (1, ..., K) and in the probability of the
#' highest category.
#'
#' For lme4 models, intervals are Wald-type (draws from the sampling distribution of the fixed
#' effects) and variance components are point estimates.
#'
#' @param mod model of class MCMCglmm (univariate or multi-response), lmerMod or glmerMod
#' @param prob probability mass of credible (HPD) / confidence intervals
#' @param digits significant digits to report
#' @param n.trials see \code{\link{glmm_varpart}}
#' @param nsim lme4 only: number of draws from the sampling distribution of the fixed effects
#' @param nmc categorical responses with > 2 categories: Monte Carlo draws per posterior sample
#' @param print logical, print the text?
#' @return invisibly, a list with the text (\code{text}), a table of fixed effects on both scales
#' (\code{fixed}), the variance partitioning (\code{varpart}), R-squared (\code{R2}) and
#' correlations (\code{cor}).
#' @keywords MCMCglmm, lme4, interpretation, summary
#' @seealso \code{\link{glmm_varpart}}, \code{\link{glmm_R2}}, \code{\link{glmm_cor}}, \code{\link{MCMCdiag}}, \code{\link{merdiag}}
#' @export
#' @examples
#' \donttest{
#' library(lme4)
#' cbpp$prop <- cbpp$incidence / cbpp$size
#' mod <- glmer(prop ~ period + (1 | herd), family = binomial, weights = size, data = cbpp)
#' glmm_explain(mod)
#' }

glmm_explain <- function(mod, prob = 0.95, digits = 3, n.trials = NULL, nsim = 1000, nmc = 10000, print = TRUE){

  infos <- .glmm_infos(mod, nsim = nsim, n.trials = n.trials)
  multi <- length(infos) > 1
  bayes <- infos[[1]]$bayes
  f <- function(x) vapply(x, function(v){
    format(signif(v, digits), digits = digits, scientific = abs(v) > 1e5 || (abs(v) < 1e-3 && v != 0))
  }, character(1), USE.NAMES = FALSE)
  pc <- function(x) sprintf("%.1f%%", 100 * x)
  ci <- function(lo, hi, fun = f) sprintf("[%s, %s]", fun(lo), fun(hi))
  pint <- paste0(round(100 * prob), "% ", if(bayes) "HPD" else "CI")

  txt <- character(0)
  add <- function(...) txt <<- c(txt, paste0(...))
  wrap <- function(s, indent = 2){
    paste(strwrap(s, width = getOption("width") - 2, indent = indent, exdent = indent + 2), collapse = "\n")
  }
  para <- function(s, indent = 2) add(wrap(s, indent))

  ## 1. the model ---------------------------------------------------------
  add("== Model ==")
  if(bayes){
    mc <- attr(mod$Sol, "mcpar")
    if(multi){
      para(sprintf("Multi-response MCMCglmm with %d traits: %s. %d posterior samples (iterations %d to %d, thinning interval %d).",
                   length(infos),
                   paste(vapply(infos, function(i) sprintf("%s [%s, %d rows]", i$trait, .family_label(i$family, i$cp), i$n), character(1)), collapse = "; "),
                   infos[[1]]$S, mc[1], mc[2], mc[3]))
    } else {
      para(sprintf("MCMCglmm, %s family. %d observations; %d posterior samples (iterations %d to %d, thinning interval %d).",
                   .family_label(infos[[1]]$family, infos[[1]]$cp), infos[[1]]$n, infos[[1]]$S, mc[1], mc[2], mc[3]))
    }
    para(sprintf("Fixed: %s | Random: %s | Residual: %s",
                 paste(deparse(mod$Fixed$formula), collapse = ""),
                 if(is.null(mod$Random$formula)) "none" else paste(deparse(mod$Random$formula), collapse = ""),
                 paste(deparse(mod$Residual$formula), collapse = "")))
    para(sprintf("Point estimates are posterior means with %s intervals; the posterior mode is often better for variances.", pint))
  } else {
    para(sprintf("lme4 %s, %s family. %d observations.", class(mod)[1], .family_label(infos[[1]]$family), infos[[1]]$n))
    para(sprintf("Formula: %s", paste(deparse(stats::formula(mod)), collapse = "")))
    para(sprintf("Point estimates are maximum-likelihood/REML estimates with Wald-type %s.", pint))
  }
  add("")

  ## 2. health check ------------------------------------------------------
  add("== Convergence / fit checks ==")
  iss <- attr(if(bayes) MCMCdiag(mod) else merdiag(mod, print = FALSE), "issues")
  if(length(iss) == 0){
    para(if(bayes) "No problems detected from this chain (ESS, autocorrelation, stationarity). Confirm with several chains via MCMCgelman()."
         else "No problems detected (convergence, singularity, overdispersion).")
  } else {
    for(i in iss) add(wrap(paste("-", i), 2))
  }
  add("")

  ## 3. per trait ---------------------------------------------------------
  fixed.tabs <- vps <- r2s <- list()
  for(info in infos){
    if(multi){
      add(sprintf("======== Trait: %s - %s ========", info$trait, .family_label(info$family, info$cp)))
      add("")
    }
    res <- .explain_trait(info, mod, prob, add, para, wrap, f, pc, ci, pint)
    fixed.tabs[[length(fixed.tabs) + 1]] <- res$fixed
    vps[[length(vps) + 1]] <- res$varpart
    r2s[[length(r2s) + 1]] <- res$R2
  }

  ## 4. categorical responses: joint data scale -----------------------------
  if(bayes){
    for(g in .MCMC_cat_groups(mod)){
      res <- .explain_categorical(mod, g, prob, add, para, wrap, f, pc, ci, nmc)
      vps <- c(vps, res$varpart)
      r2s <- c(r2s, res$R2)
    }
  }

  ## 5. correlations ------------------------------------------------------
  cr <- suppressMessages(glmm_cor(mod, prob))
  if(!is.null(cr)){
    add("== Correlations (latent scale) ==")
    # show trait effects by trait name ("traity1" -> "y1")
    tn <- vapply(infos, function(i) if(is.null(i$trait)) "" else i$trait, character(1))
    pretty <- function(e) ifelse(sub("^trait", "", e) %in% tn, sub("^trait", "", e), e)
    for(i in seq_len(nrow(cr))){
      r <- cr[i, ]
      r$effect1 <- pretty(r$effect1)
      r$effect2 <- pretty(r$effect2)
      what <- sprintf("%s correlation between %s and %s (%s)",
                      if(r$level == "residual") "Residual" else "Random-effect", r$effect1, r$effect2, r$term)
      if(!bayes){
        line <- sprintf("%s: %s.%s", what, f(r$mean),
                        if(abs(r$mean) > 0.999) " Estimated at +/-1: the fit is singular." else "")
      } else if(r$upper == r$lower){
        line <- sprintf("%s: fixed at %s in the prior.", what, f(r$mean))
      } else {
        line <- sprintf("%s: %s (mode %s) %s, P(r > 0) = %s. %s", what, f(r$mean), f(r$mode),
                        ci(r$lower, r$upper), f(r$P.positive),
                        if(r$lower > 0) "The interval excludes zero: positively correlated."
                        else if(r$upper < 0) "The interval excludes zero: negatively correlated."
                        else "The interval includes zero.")
      }
      add(wrap(line, 2))
    }
    add("")
  }

  if(print) cat(txt, sep = "\n")
  fixed <- do.call(rbind, fixed.tabs)
  rownames(fixed) <- NULL
  invisible(list(text = txt, fixed = fixed,
                 varpart = if(all(vapply(vps, is.null, logical(1)))) NULL else .combine_traits(Filter(Negate(is.null), vps), "glmm_varpart"),
                 R2 = if(all(vapply(r2s, is.null, logical(1)))) NULL else .combine_traits(Filter(Negate(is.null), r2s), "glmm_R2"),
                 cor = cr))
}

# fixed effects and variance components of one trait (or a univariate model)
.explain_trait <- function(info, mod, prob, add, para, wrap, f, pc, ci, pint){
  fam <- info$family
  ordK <- NCOL(info$cp) + 1 # number of categories for ordinal/threshold traits
  ordinal <- fam %in% c("probit", "threshold") && ordK > 2
  latent.name <- switch(fam, gaussian = "data", poisson = "log", logit = "logit (log-odds)",
                        probit = "probit", threshold = "probit (liability)", "latent")
  if(fam == "categorical"){
    para(sprintf("This trait is the log-odds of '%s' against the reference category (latent scale). Category probabilities on the data scale are given in the section for the categorical response below.",
                 info$trait), 0)
    add("")
  } else if(fam == "other"){
    para("Note: this family is not supported for data-scale calculations; only latent-scale results are given.")
    add("")
  }

  ## fixed effects ----------------------------------------------------------
  add("== Fixed effects ==")
  B <- info$beta
  comps <- do.call(cbind, info$comps)
  Vtot <- if(is.null(comps)) rep(0, info$S) else rowSums(comps)
  X <- info$X
  Xs <- as.matrix(X[seq_len(min(nrow(X), 5000)), , drop = FALSE])
  # columns that are 1 for every row (intercept, trait indicator) make up the baseline
  base.cols <- which(apply(Xs, 2, function(v) all(v == 1)))
  b0 <- if(length(base.cols)) rowSums(B[, base.cols, drop = FALSE]) else rep(0, info$S)
  is.binary <- apply(Xs, 2, function(v) all(v %in% c(0, 1)))
  # name of each coefficient within this trait: drop trait indicators from interactions
  disp <- vapply(colnames(B), function(nm){
    pieces <- strsplit(nm, ":", fixed = TRUE)[[1]]
    keep <- pieces[!pieces %in% colnames(B)[base.cols]]
    if(length(keep) == 0 || length(keep) == length(pieces)) nm else paste(keep, collapse = ":")
  }, character(1))

  bs <- .post_summary(B, prob)
  if(info$bayes){
    pval <- apply(B, 2, function(v) max(2 * min(mean(v > 0), mean(v < 0)), 1 / length(v)))
    est <- bs$mean
  } else {
    est <- lme4::fixef(mod)[colnames(B)]
    pval <- 2 * stats::pnorm(-abs(est / sqrt(diag(as.matrix(stats::vcov(mod)))[colnames(B)])))
  }

  what <- switch(fam, poisson = "counts", gaussian = "values",
                 if(ordinal) sprintf("category numbers (1 to %d)", ordK) else "probabilities")
  if(fam == "categorical"){
    para(sprintf("Coefficients are log-odds of '%s' against the reference category.", info$trait), 0)
  } else if(fam == "other"){
    para("Coefficients are on the latent scale.", 0)
  } else {
    para(sprintf("Coefficients are on the %s scale. Data-scale values are expected %s with other covariates at 0 / reference levels, averaging over the random effects%s.",
                 latent.name, what,
                 if(info$bayes && fam %in% c("poisson", "logit")) " and the units (overdispersion) term" else ""), 0)
  }
  if(ordinal){
    para(sprintf("Cutpoints (category boundaries on the latent scale, the first fixed at 0): %s.",
                 paste(f(colMeans(as.matrix(info$cp))[-1]), collapse = ", ")), 0)
  }
  add("")

  data.mean <- function(eta) .over_draws(.ey, fam, eta, Vtot, info$cp)
  top.prob <- function(eta) .over_draws(function(...) utils::tail(.cat_probs(...), 1), fam, eta, Vtot, info$cp)
  base <- if(!fam %in% c("other", "categorical")) data.mean(b0) else NULL

  fixed.tab <- data.frame(term = colnames(B), estimate = est, lower = bs$lower, upper = bs$upper,
                          p = pval, stringsAsFactors = FALSE, row.names = NULL)
  if(!is.null(info$trait)) fixed.tab <- cbind(trait = info$trait, fixed.tab, stringsAsFactors = FALSE)
  fixed.tab$data.estimate <- fixed.tab$data.lower <- fixed.tab$data.upper <- NA_real_
  fixed.tab$ratio <- fixed.tab$ratio.lower <- fixed.tab$ratio.upper <- NA_real_

  header <- function(j){
    ptxt <- if(info$bayes) sprintf("pMCMC = %s", format.pval(pval[j], digits = 2, eps = 1 / info$S))
            else sprintf("Wald p = %s", format.pval(pval[j], digits = 2))
    nm <- colnames(B)[j]
    lab <- if(disp[j] != nm) sprintf("%s (%s)", disp[j], nm) else nm
    add(wrap(sprintf("%s: %s %s %s, %s.", lab, f(est[j]), pint, ci(bs$lower[j], bs$upper[j]), ptxt), 2))
  }

  # baseline
  for(j in base.cols) header(j)
  if(!is.null(base)){
    s <- .post_summary(cbind(base), prob)
    if(length(base.cols)) fixed.tab[base.cols[1], c("data.estimate", "data.lower", "data.upper")] <- s[1, c("mean", "lower", "upper")]
    if(ordinal){
      probs <- t(vapply(seq_len(info$S), function(i){
        .cat_probs(fam, b0[i], Vtot[i], if(is.matrix(info$cp)) info$cp[i, ] else info$cp)
      }, numeric(ordK)))
      add(wrap(sprintf("Baseline expected category (all covariates 0 / reference): %s %s. Category probabilities: %s.",
                       f(s$mean), ci(s$lower, s$upper),
                       paste(sprintf("%d: %s", seq_len(ordK), f(colMeans(probs))), collapse = ", ")), 4))
    } else {
      w <- switch(fam, poisson = "expected count", gaussian = "expected value",
                  "probability of a success (y = 1)")
      add(wrap(sprintf("Baseline %s (all covariates 0 / reference): %s %s.",
                       w, f(s$mean), ci(s$lower, s$upper)), 4))
    }
  }

  for(j in setdiff(seq_len(ncol(B)), base.cols)){
    header(j)
    nm <- disp[j]
    excl <- bs$lower[j] > 0 || bs$upper[j] < 0
    change <- if(grepl(":", nm)) {
      sprintf("the interaction %s modifies the effects of its components (data-scale value shown at baseline only)", nm)
    } else if(is.binary[j]) {
      sprintf("going from the reference level to '%s'", nm)
    } else {
      sprintf("a one-unit increase in %s", nm)
    }
    dir <- if(excl) {
      if(est[j] > 0) "The interval excludes zero: evidence of a positive effect." else "The interval excludes zero: evidence of a negative effect."
    } else "The interval includes zero: no clear evidence of an effect."

    if(fam == "categorical"){
      r <- .post_summary(cbind(exp(B[, j])), prob)
      line <- sprintf("%s changes the log-odds of '%s' vs the reference category by %s (conditional odds ratio %s %s).",
                      change, info$trait, f(est[j]), f(r$mean), ci(r$lower, r$upper))
      substr(line, 1, 1) <- toupper(substr(line, 1, 1))
      add(wrap(paste(line, dir), 4))
      next
    }
    if(fam == "other"){
      add(wrap(sprintf("On the latent scale, %s changes the linear predictor by %s. %s", change, f(est[j]), dir), 4))
      next
    }

    new <- data.mean(b0 + B[, j])
    diff <- new - base
    s <- .post_summary(cbind(diff = diff, new = new), prob)
    fixed.tab[fixed.tab$term == colnames(B)[j], c("data.estimate", "data.lower", "data.upper")] <- s["diff", c("mean", "lower", "upper")]

    if(fam == "gaussian"){
      line <- sprintf("%s changes the expected response by %s %s.", change, f(est[j]), ci(bs$lower[j], bs$upper[j]))
    } else if(fam == "poisson"){
      r <- .post_summary(cbind(exp(B[, j])), prob)
      fixed.tab[fixed.tab$term == colnames(B)[j], c("ratio", "ratio.lower", "ratio.upper")] <- r[1, c("mean", "lower", "upper")]
      line <- sprintf("%s multiplies the expected count by %s %s (rate ratio), i.e. from %s to %s, a change of %s %s.",
                      change, f(r$mean), ci(r$lower, r$upper), f(mean(base)), f(s["new", "mean"]),
                      f(s["diff", "mean"]), ci(s["diff", "lower"], s["diff", "upper"]))
    } else if(ordinal){
      tp <- .post_summary(cbind(top.prob(b0 + B[, j]) - top.prob(b0)), prob)
      line <- sprintf("%s changes the expected category from %s to %s, a difference of %s %s, and the probability of the highest category (%d) by %s %s percentage points.",
                      change, f(mean(base)), f(s["new", "mean"]),
                      f(s["diff", "mean"]), ci(s["diff", "lower"], s["diff", "upper"]),
                      ordK, f(100 * tp$mean), ci(100 * tp$lower, 100 * tp$upper))
    } else {
      extra <- ""
      if(fam == "logit"){
        # MCMCglmm's units variance attenuates population-averaged odds ratios;
        # report the conditional (subject-specific) odds ratio from the coefficient
        r <- .post_summary(cbind(exp(B[, j])), prob)
        fixed.tab[fixed.tab$term == colnames(B)[j], c("ratio", "ratio.lower", "ratio.upper")] <- r[1, c("mean", "lower", "upper")]
        extra <- sprintf(" The conditional odds ratio (holding random effects fixed) is %s %s.",
                         f(r$mean), ci(r$lower, r$upper))
      }
      line <- sprintf("%s changes the probability of a success from %s to %s, a difference of %s %s percentage points.%s",
                      change, f(mean(base)), f(s["new", "mean"]),
                      f(100 * s["diff", "mean"]), ci(100 * s["diff", "lower"], 100 * s["diff", "upper"]), extra)
    }
    substr(line, 1, 1) <- toupper(substr(line, 1, 1))
    add(wrap(paste(line, dir), 4))
  }
  add("")

  ## variance components ----------------------------------------------------
  add("== Variance components ==")
  vp <- NULL
  r2 <- NULL
  if(!is.null(comps) && ncol(comps) > 0){
    scales <- .usable_scales(info)
    vp <- .varpart_draws_to_df(.varpart_draws(info, scales, fixed = FALSE), info, prob, fixed = FALSE)
    vs <- .post_summary(comps, prob)
    para(paste(c("Proportions are 'adjusted' (conditional on the fixed effects).",
               if("latent" %in% scales) sprintf("Latent: share of the %s-scale variance%s.", latent.name,
                                                 if(info$bayes && fam != "gaussian") " (including units)" else ""),
               if("link" %in% scales) sprintf("Link: also counting the distribution-specific variance%s.",
                                               switch(fam, poisson = " (log(1 + 1/lambda))", logit = " (pi^2/3)", probit = " (1)", "")),
               if("data" %in% scales) {
                 if(ordinal) "Data: share of the variance of the observed category number." else "Data: share of the variance of the observed response."
               }), collapse = " "), 0)
    if(!is.null(info$trait)){
      para("Each term's variance is the part of its covariance matrix belonging to this trait.", 0)
    }
    add("")
    for(cn in colnames(comps)){
      v <- vs[cn, ]
      vtxt <- if(info$bayes){
        sprintf("variance %s (mode %s) %s", f(v$mean), f(v$mode), ci(v$lower, v$upper))
      } else sprintf("variance %s", f(v$mean))
      if(v$upper == v$lower && info$bayes) vtxt <- sprintf("variance fixed at %s in the prior", f(v$mean))
      role <- if(cn %in% info$resid){
        if(fam == "gaussian") "Residual" else "Residual/overdispersion"
      } else "Random"
      add(wrap(sprintf("%s term '%s': %s.", role, cn, vtxt), 2))
      parts <- vapply(unique(vp$scale), function(sc){
        r <- vp[vp$component == cn & vp$scale == sc, ]
        if(info$bayes) sprintf("%s %s %s", sc, pc(r$mode), ci(r$lower, r$upper, pc))
        else sprintf("%s %s", sc, pc(r$mean))
      }, character(1))
      add(wrap(paste0("Proportion of variance", if(info$bayes) " (mode, interval)" else "", ": ", paste(parts, collapse = "; "), "."), 4))
      if(info$bayes && v$upper > v$lower && v$lower < 0.01 * stats::median(comps[, cn])){
        add(wrap("The posterior is pressed against zero: the data say little about this variance, so check prior sensitivity (MCMCpriorplot()).", 4))
      }
      if(!info$bayes && v$mean < 1e-8 && !(cn %in% info$resid)){
        add(wrap("Estimated as (almost) exactly zero: singular fit. The data do not support this term as specified.", 4))
      }
    }
    if(info$bayes && fam %in% c("logit", "probit", "threshold") && length(info$resid)){
      para(sprintf("Note: with %s data the units variance is not identifiable and is usually fixed (e.g. at 1); proportions depend on that choice on the latent/link scales, much less so on the data scale.",
                   if(ordinal) "ordinal" else "binary"), 2)
    }
    add("")
    r2 <- .R2_from_info(info, prob, scales)
    rs <- function(sc, w){
      r <- r2[r2$component == w & r2$scale == sc, ]
      if(info$bayes) sprintf("%s %s", pc(r$mode), ci(r$lower, r$upper, pc)) else pc(r$mean)
    }
    para(sprintf("Fixed effects alone explain (marginal R2) %s of the variance on the %s scale; fixed + random effects (conditional R2) explain %s.",
                 rs(scales[1], "R2m"), scales[1], rs(scales[1], "R2c")), 0)
    if("data" %in% scales && scales[1] != "data"){
      para(sprintf("On the data scale these are R2m = %s and R2c = %s.", rs("data", "R2m"), rs("data", "R2c")), 0)
    }
  } else {
    para("No variance components.", 2)
  }
  add("")
  list(fixed = fixed.tab, varpart = vp, R2 = r2)
}
