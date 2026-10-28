#' Variance partitioning (ICC/repeatability) on the latent, link and data scales
#'
#' Calculates the proportion of variance explained by each random (and residual) term of a
#' mixed model, with posterior modes, means and HPD intervals for MCMCglmm models.
#' This generalises \code{MCMCRepnorm2()} and \code{MCMCReppois()} to other families,
#' to random slopes / covariance structures, and to the data (observed) scale.
#'
#' Three scales are available:
#' \describe{
#'   \item{latent}{The scale of the linear predictor (including the MCMCglmm \code{units}
#'   term). \eqn{V_k / (V_F + \sum V)}. For a Gaussian model this is the usual ICC.}
#'   \item{link}{As latent, but the distribution-specific variance is added to the
#'   denominator (\eqn{\pi^2/3} for logit, 1 for probit, \eqn{\log(1 + 1/\lambda)} for
#'   Poisson): the Nakagawa & Schielzeth (2010, 2013) definition. For Gaussian and threshold models it
#'   equals the latent scale, so it is only returned when asked for without \code{"latent"}.}
#'   \item{data}{The observed (data/response) scale: \eqn{Var(E[y | a_k]) / Var(y)}, computed
#'   exactly (Poisson) or by Gauss-Hermite quadrature (binary families), following
#'   de Villemereuil et al. (2016). For non-Gaussian families two rows account for the rest of the
#'   variance so that the column sums to one: \code{Non-additive}, the part of the variance of the
#'   expected response that can't be attributed to a single component because of the non-linear link,
#'   and \code{Distribution-specific}, the sampling (e.g. binomial or Poisson) variance around it.}
#' }
#'
#' \strong{Ordinal and threshold models} with more than two categories use the cutpoints: on the
#' data scale the response is the category number (1, ..., K), so proportions refer to the
#' variance of the category score. The latent and link scales don't depend on the cutpoints.
#'
#' \strong{Multi-response models} (\code{cbind(y1, y2) ~ trait - 1 + ...}) are partitioned
#' separately for each trait, using the variances (diagonal elements) belonging to that trait
#' and that trait's family; see \code{\link{glmm_cor}} for the between-trait correlations.
#' Categorical models with more than two categories are fitted by MCMCglmm as several latent
#' traits, the log-odds of each category against the reference category. The latent scale is given
#' per contrast. The data scale is given per category, for the indicator \eqn{1(y = k)}:
#' \eqn{Var(E[p_k | a]) / (\bar{p}_k (1 - \bar{p}_k))}, computed jointly over the contrasts by
#' simulation (the link scale is not defined for these models). The reference category is labelled
#' "reference", as the model object doesn't store its name.
#'
#' The variance contributed by a term is averaged over the observations,
#' \eqn{mean(z_i' \Sigma z_i)} (Johnson 2014), so for random slopes (\code{us(1 + x):g})
#' it depends on the values of \code{x}, and \code{idh(f):g} terms are weighted by the
#' frequency of each level of \code{f}.
#'
#' For lme4 models variance components are point estimates; the intervals only reflect
#' uncertainty in the fixed effects (draws from their sampling distribution) and
#' are therefore too narrow. Use \code{lme4::bootMer()} if you need proper intervals.
#'
#' @param mod model of class MCMCglmm (univariate or multi-response), lmerMod or glmerMod.
#' @param scale one or more of "latent", "link", "data".
#' @param fixed logical. Should the variance explained by the fixed effects be part of the
#' denominator? FALSE (default) gives "adjusted" repeatabilities (conditional on the fixed effects),
#' TRUE gives proportions of the total phenotypic variance, as in R-squared.
#' @param prob probability mass of the HPD interval.
#' @param n.trials for binomial models, the number of trials; the data scale then refers to
#' the observed proportion. Defaults to 1 (binary data) for MCMCglmm and to the mean of the
#' prior weights for glmer.
#' @param nsim lme4 only: number of draws from the fixed-effect sampling distribution.
#' @param trait multi-response models only: names or numbers of the traits to use (default all).
#' @param nmc categorical responses with > 2 categories only: number of Monte Carlo draws per
#' posterior sample for the data scale.
#' @return a data.frame of class \code{glmm_varpart} with one row per component and scale (and
#' trait, with a \code{trait} column, for multi-response models); the draws are stored in
#' \code{attr(x, "draws")}, a list by scale (by trait, then scale, for multi-response models).
#' There is a \code{plot()} method.
#' @references
#' Nakagawa, S. & Schielzeth, H. (2010) Biol Rev 85: 935-956.
#' Johnson, P.C.D. (2014) Methods Ecol Evol 5: 944-946.
#' de Villemereuil, P. et al. (2016) Genetics 204: 1281-1294.
#' @keywords MCMCglmm, lme4, repeatability, ICC, variance explained
#' @seealso \code{\link{glmm_R2}}, \code{\link{glmm_explain}}
#' @export
#' @examples
#' \donttest{
#' library(MCMCglmm)
#' set.seed(1)
#' d <- data.frame(g = factor(rep(1:20, each = 10)), x = rnorm(200))
#' d$y <- rpois(200, exp(0.5 + 0.3 * d$x + rnorm(20, 0, 0.7)[d$g]))
#' prior <- list(R = list(V = 1, nu = 0.002),
#'               G = list(G1 = list(V = 1, nu = 1, alpha.mu = 0, alpha.V = 1000)))
#' mod <- MCMCglmm(y ~ x, random = ~ g, family = "poisson", data = d,
#'                 prior = prior, verbose = FALSE)
#' vp <- glmm_varpart(mod)
#' vp
#' plot(vp)
#' }

glmm_varpart <- function(mod, scale = c("latent", "link", "data"), fixed = FALSE,
                         prob = 0.95, n.trials = NULL, nsim = 1000, trait = NULL, nmc = 10000){

  scale <- match.arg(scale, several.ok = TRUE)
  infos <- .glmm_infos(mod, nsim = nsim, n.trials = n.trials, trait = trait)
  parts <- lapply(infos, function(info){
    sc <- .check_scales(info, scale)
    .varpart_draws_to_df(.varpart_draws(info, sc, fixed), info, prob, fixed)
  })
  if("data" %in% scale) parts <- c(parts, .cat_parts(mod, prob, fixed, trait, "varpart", nmc))
  .combine_traits(parts, "glmm_varpart")
}

# restrict the requested scales to the usable ones, saying why
.check_scales <- function(info, scale){
  ok <- .usable_scales(info, scale)
  dropped <- setdiff(scale, ok)
  # categorical contrasts: explained once per response by .cat_parts()
  if(length(dropped) && info$family != "categorical"){
    why <- if(info$family == "other") "only the latent scale is available for this family"
      else "there is no residual term on the latent scale of this model, so it is degenerate"
    # the link scale of Gaussian/threshold models is dropped silently (documented)
    if(!identical(dropped, "link") || info$family == "other" || length(info$resid) == 0) message(if(!is.null(info$trait)) paste0("Trait ", info$trait, ": "),
            "not computing the ", paste(dropped, collapse = "/"), " scale: ", why)
  }
  ok
}

# list (by scale) of S x components matrices of proportions
.varpart_draws <- function(info, scale, fixed){
  comps <- do.call(cbind, info$comps)
  Vf <- if(fixed) info$Vf else rep(0, info$S)
  Vtot <- rowSums(comps)
  fam <- info$family
  out <- list()

  if("latent" %in% scale){
    denom <- Vf + Vtot
    m <- comps / denom
    if(fixed) m <- cbind(`Fixed effects` = Vf / denom, m)
    out$latent <- m
  }
  if("link" %in% scale){
    vd <- .over_draws(.dist_var, fam, info$mu, Vtot + Vf)
    denom <- Vf + Vtot + vd
    m <- comps / denom
    if(fixed) m <- cbind(`Fixed effects` = Vf / denom, m)
    if(fam != "gaussian" && fam != "threshold"){
      m <- cbind(m, `Distribution-specific` = vd / denom)
    }
    out$link <- m
  }
  if("data" %in% scale){
    V <- Vtot + Vf
    vy <- .over_draws(.vary, fam, info$mu, V, info$n.trials, info$cp)
    m <- sapply(colnames(comps), function(cn){
      .over_draws(.vcomp, fam, info$mu, comps[, cn], V, info$cp) / vy
    })
    m <- matrix(m, ncol = ncol(comps), dimnames = list(NULL, colnames(comps)))
    if(fixed){
      m <- cbind(`Fixed effects` = .over_draws(.vcomp, fam, info$mu, Vf, V, info$cp) / vy, m)
    }
    if(fam != "gaussian"){
      # the rest of Var(y): Var(E[y | l]) is more than the sum of the parts because
      # of the non-linear link, and E[Var(y | l)] is the sampling variance
      # (threshold models: y is fixed given l, so Var(E[y | l]) = Var(y))
      vl <- if(fam == "threshold") vy else .over_draws(.vcomp, fam, info$mu, V, V, info$cp)
      m <- cbind(m, `Non-additive` = vl / vy - rowSums(m))
      if(fam != "threshold") m <- cbind(m, `Distribution-specific` = 1 - vl / vy)
    }
    out$data <- m
  }
  out
}

.varpart_draws_to_df <- function(draws, info, prob, fixed){
  res <- do.call(rbind, lapply(names(draws), function(sc){
    s <- .post_summary(draws[[sc]], prob)
    data.frame(component = rownames(s), scale = sc, s, row.names = NULL,
               stringsAsFactors = FALSE)
  }))
  if(!is.null(info$trait)) res <- cbind(trait = info$trait, res, stringsAsFactors = FALSE)
  class(res) <- c("glmm_varpart", "data.frame")
  attr(res, "draws") <- draws
  attr(res, "family") <- .family_label(info$family, info$cp)
  attr(res, "fixed") <- fixed
  attr(res, "prob") <- prob
  attr(res, "bayes") <- info$bayes
  res
}

# stack per-trait results (a no-op for univariate models)
.combine_traits <- function(parts, cls){
  if(length(parts) == 1){
    res <- parts[[1]]
  } else {
    res <- do.call(rbind, lapply(parts, function(x) as.data.frame(unclass(x), stringsAsFactors = FALSE)))
    rownames(res) <- NULL
    tr <- vapply(parts, function(x) x$trait[1], character(1))
    attr(res, "draws") <- stats::setNames(lapply(parts, attr, "draws"), tr)
    attr(res, "family") <- stats::setNames(vapply(parts, attr, character(1), "family"), tr)
    for(a in c("fixed", "prob", "bayes")) attr(res, a) <- attr(parts[[1]], a)
  }
  class(res) <- unique(c(cls, "glmm_varpart", "data.frame"))
  res
}

# print a table one block per trait (if any) and scale
.print_blocks <- function(x, digits, first = "component"){
  y <- as.data.frame(unclass(x), stringsAsFactors = FALSE)
  num <- c("mode", "mean", "lower", "upper")
  y[num] <- round(y[num] * 100, digits)
  names(y)[names(y) == "lower"] <- paste0("lower(", attr(x, "prob") * 100, "%HPD)")
  names(y)[names(y) == "upper"] <- paste0("upper(", attr(x, "prob") * 100, "%HPD)")
  names(y)[names(y) == "component"] <- first
  traits <- if("trait" %in% names(y)) unique(y$trait) else NA
  fam <- attr(x, "family")
  for(tr in traits){
    yt <- if(is.na(tr)) y else y[y$trait == tr, setdiff(names(y), "trait")]
    if(!is.na(tr)) cat("==== Trait:", tr, "-", fam[[tr]], "====\n")
    else cat("Family:", fam, "\n")
    for(sc in unique(yt$scale)){
      cat("--", sc, "scale --\n")
      print(yt[yt$scale == sc, setdiff(names(yt), "scale")], row.names = FALSE)
    }
    cat("\n")
  }
}

#' @export
print.glmm_varpart <- function(x, digits = 1, ...){
  fixed <- attr(x, "fixed")
  cat("Proportion of variance explained (%),",
      if(isTRUE(fixed)) "including fixed effects in the denominator" else "conditional on the fixed effects (adjusted)", "\n")
  if(!isTRUE(attr(x, "bayes"))){
    cat("Note: lme4 model, variance components are point estimates; intervals are approximate.\n")
  }
  cat("\n")
  .print_blocks(x, digits)
  invisible(x)
}

#' Plot the posterior distributions of the variance proportions
#'
#' @param x object returned by \code{glmm_varpart()} or \code{glmm_R2()}
#' @param ... unused
#' @import ggplot2
#' @export
plot.glmm_varpart <- function(x, ...){
  draws <- attr(x, "draws")
  multi <- "trait" %in% names(x)
  if(!multi) draws <- list(draws)
  long <- do.call(rbind, lapply(seq_along(draws), function(i){
    do.call(rbind, lapply(names(draws[[i]]), function(sc){
      m <- draws[[i]][[sc]]
      data.frame(trait = if(multi) names(draws)[i] else NA, scale = sc,
                 component = rep(colnames(m), each = nrow(m)),
                 value = as.vector(m) * 100, stringsAsFactors = FALSE)
    }))
  }))
  lev <- unique(long$scale)
  long$scale <- factor(long$scale, levels = lev)
  sm <- as.data.frame(unclass(x))
  sm$scale <- factor(sm$scale, levels = lev)
  if(multi) long$trait <- factor(long$trait, levels = names(draws))
  ggplot(long, aes(x = component, y = value)) +
    geom_violin(fill = "grey85", colour = NA, scale = "width") +
    geom_pointrange(data = sm, aes(y = mode * 100, ymin = lower * 100, ymax = upper * 100)) +
    {if(multi) facet_grid(trait ~ scale, scales = "free_y") else facet_wrap(~ scale, nrow = 1)} +
    coord_flip() +
    theme_bw() +
    labs(x = NULL, y = "% variance explained (posterior mode and HPD interval)")
}

#' Marginal and conditional R-squared for mixed models
#'
#' Posterior distributions of the marginal (fixed effects only) and conditional (fixed + random effects)
#' R-squared of Nakagawa & Schielzeth (2013) and Nakagawa et al. (2017), on the latent, link
#' and data scales. For MCMCglmm models the \code{units} term (and for lmer the residual) is
#' treated as unexplained variance. Multi-response models get one R-squared per trait.
#'
#' @inheritParams glmm_varpart
#' @return data.frame of class \code{glmm_varpart} (R2m and R2c rows, per scale and trait).
#' @references Nakagawa, S. & Schielzeth, H. (2013) Methods Ecol Evol 4: 133-142.
#' Nakagawa, S., Johnson, P.C.D. & Schielzeth, H. (2017) J R Soc Interface 14: 20170213.
#' @keywords MCMCglmm, lme4, R2, variance explained
#' @export
#' @examples
#' \donttest{
#' library(lme4)
#' mod <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
#' glmm_R2(mod)
#' }

glmm_R2 <- function(mod, scale = c("latent", "link", "data"), prob = 0.95,
                    n.trials = NULL, nsim = 1000, trait = NULL, nmc = 10000){
  scale <- match.arg(scale, several.ok = TRUE)
  infos <- .glmm_infos(mod, nsim = nsim, n.trials = n.trials, trait = trait)
  parts <- lapply(infos, function(info){
    .R2_from_info(info, prob, .check_scales(info, scale))
  })
  if("data" %in% scale) parts <- c(parts, .cat_parts(mod, prob, TRUE, trait, "R2", nmc))
  .combine_traits(parts, "glmm_R2")
}

#' @export
print.glmm_R2 <- function(x, digits = 1, ...){
  cat("Marginal (R2m, fixed effects) and conditional (R2c, fixed + random effects) R-squared (%)\n")
  if(!isTRUE(attr(x, "bayes"))){
    cat("Note: lme4 model, variance components are point estimates; intervals are approximate.\n")
  }
  cat("\n")
  .print_blocks(x, digits, first = "R2")
  invisible(x)
}

# R2 draws from an already-built info object (shared with glmm_explain())
.R2_from_info <- function(info, prob, scale){
  comps <- do.call(cbind, info$comps)
  G <- rowSums(comps[, setdiff(colnames(comps), info$resid), drop = FALSE])
  Vtot <- rowSums(comps)
  Vf <- info$Vf
  fam <- info$family
  out <- list()
  if("latent" %in% scale){
    d <- Vf + Vtot
    out$latent <- cbind(R2m = Vf / d, R2c = (Vf + G) / d)
  }
  if("link" %in% scale){
    d <- Vf + Vtot + .over_draws(.dist_var, fam, info$mu, Vtot + Vf)
    out$link <- cbind(R2m = Vf / d, R2c = (Vf + G) / d)
  }
  if("data" %in% scale){
    V <- Vtot + Vf
    vy <- .over_draws(.vary, fam, info$mu, V, info$n.trials, info$cp)
    out$data <- cbind(R2m = .over_draws(.vcomp, fam, info$mu, Vf, V, info$cp) / vy,
                      R2c = .over_draws(.vcomp, fam, info$mu, Vf + G, V, info$cp) / vy)
  }
  .varpart_draws_to_df(out, info, prob, fixed = TRUE)
}
