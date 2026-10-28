# Internal helpers shared by glmm_varpart(), glmm_R2(), glmm_explain() and the
# diagnostic functions. None of these are exported.
#
# The central idea is .glmm_info(), which turns either an MCMCglmm or an lme4
# (g)lmerMod model into a common description. Multi-response MCMCglmm models
# get one description per trait (.glmm_infos()), computed from that trait's
# rows of the design matrices, so each trait can be treated like a univariate
# model:
#   - family : one of "gaussian", "poisson", "logit", "probit", "threshold"
#              (or "categorical" for one contrast of a > 2 category response,
#              see glmm_categorical.R, or "other": only latent-scale quantities)
#   - beta   : S x p matrix of draws of the fixed effects (posterior draws for
#              MCMCglmm, draws from the sampling distribution for lme4)
#   - comps  : named list of S-vectors, the latent-scale variance contributed by
#              each random/residual term, averaged over the observations
#              (Johnson 2014, Methods Ecol Evol 5: 944-946), so random slopes
#              and heterogeneous (idh) structures are handled properly
#   - mu, Vf : mean and variance of the fixed-effect linear predictor for every
#              draw, from the column means and covariance of the design (so the
#              n x S prediction matrix is never formed)
#   - cp     : cutpoints for ordinal/threshold traits (0 for binary data)

.check_MCMCglmm <- function(mod){
  if(!inherits(mod, "MCMCglmm")){
    stop("Model must be of class MCMCglmm!")
  }
  invisible(TRUE)
}

.check_merMod <- function(mod){
  if(!inherits(mod, "merMod")){
    stop("Model must be an lme4 model of class lmerMod or glmerMod")
  }
  invisible(TRUE)
}

# probabilists' Gauss-Hermite nodes and weights (Golub-Welsch), so that
# E[f(Z)] ~= sum(w * f(x)) for Z ~ N(0, 1)
.gh <- local({
  cache <- list()
  function(n = 32){
    key <- as.character(n)
    if(is.null(cache[[key]])){
      J <- matrix(0, n, n)
      off <- sqrt(seq_len(n - 1))
      J[cbind(1:(n - 1), 2:n)] <- off
      J[cbind(2:n, 1:(n - 1))] <- off
      e <- eigen(J, symmetric = TRUE)
      cache[[key]] <<- list(x = e$values, w = e$vectors[1, ]^2)
    }
    cache[[key]]
  }
})

# summary of posterior draws: one row per column of x
.post_summary <- function(x, prob = 0.95){
  x <- as.matrix(x)
  ok <- apply(x, 2, function(v) all(is.finite(v)) && stats::var(v) > 0)
  mode <- mean <- lower <- upper <- rep(NA_real_, ncol(x))
  mean <- colMeans(x)
  if(any(ok)){
    mode[ok] <- MCMCglmm::posterior.mode(coda::as.mcmc(x[, ok, drop = FALSE]))
    hpd <- coda::HPDinterval(coda::as.mcmc(x[, ok, drop = FALSE]), prob = prob)
    lower[ok] <- hpd[, 1]
    upper[ok] <- hpd[, 2]
  }
  # constant (e.g. fixed) parameters
  lower[!ok] <- upper[!ok] <- mode[!ok] <- mean[!ok]
  data.frame(mode = mode, mean = mean, lower = lower, upper = upper,
             row.names = colnames(x), check.names = FALSE)
}

# multi-response models (and categorical models with > 2 categories, which
# MCMCglmm fits as several latent traits) have trait in the formula
.is_multiresponse <- function(mod){
  "trait" %in% all.vars(mod$Fixed$formula) ||
    any(grepl("cbind", deparse(mod$Fixed$formula[[2]])))
}

# Describe each (co)variance block of an MCMCglmm model: which VCV columns it
# owns, its dimension, and the n x k matrix W of per-row weights of each effect,
# so that the variance it contributes over a set of rows is
# mean(w_i' Sigma w_i) = sum(Sigma * M) with M = crossprod(W[rows, ]) / length(rows).
.MCMC_blocks <- function(mod){
  out <- list()
  vcv.names <- colnames(mod$VCV)
  col <- 0

  make <- function(part, Z){
    if(is.null(part$nfl)) return(list())
    tl <- .rhs_terms(part$formula)
    nrt <- part$nrt
    # if the term labels can't be matched up to the blocks, fall back on names
    term.of.block <- if(length(tl) == length(nrt)) rep(tl, nrt) else rep(NA, length(part$nfl))
    nfl <- part$nfl
    nrl <- part$nrl
    Zok <- !is.null(Z) && ncol(Z) == sum(nfl * nrl)
    zcol <- 0
    res <- list()
    for(b in seq_along(nfl)){
      k <- nfl[b]
      cols <- col + seq_len(k^2)
      col <<- col + k^2
      diag.cols <- cols[seq(1, k^2, by = k + 1)]
      if(Zok){
        W <- sapply(seq_len(k), function(j){
          idx <- zcol + (j - 1) * nrl[b] + seq_len(nrl[b])
          Matrix::rowSums(Z[, idx, drop = FALSE])
        })
        W <- matrix(W, ncol = k)
      } else {
        # without the design we can only use the variances at covariate = 0
        W <- NULL
      }
      zcol <- zcol + k * nrl[b]
      res[[length(res) + 1]] <- list(
        term = if(is.na(term.of.block[b])) vcv.names[diag.cols[1]] else term.of.block[b],
        cols = cols, diag.cols = diag.cols, k = k, W = W, Zok = Zok
      )
    }
    res
  }

  G <- make(mod$Random, mod$Z)
  R <- make(mod$Residual, mod$ZR)
  for(i in seq_along(G)) G[[i]]$residual <- FALSE
  for(i in seq_along(R)) R[[i]]$residual <- TRUE
  c(G, R)
}

# the terms of a random/residual formula as written (terms() would reorder
# the variables of an interaction, turning idh(f):g into g:idh(f))
.rhs_terms <- function(formula){
  split <- function(e){
    if(is.call(e) && identical(e[[1]], as.name("+"))){
      c(if(length(e) == 3) split(e[[2]]), split(e[[length(e)]]))
    } else if(is.call(e) && identical(e[[1]], as.name("("))){
      split(e[[2]])
    } else paste(deparse(e), collapse = "")
  }
  split(formula[[length(formula)]])
}

# indices of VCV columns that are variances (as opposed to covariances)
.MCMC_variance_cols <- function(mod){
  blocks <- .MCMC_blocks(mod)
  if(length(blocks) == 0 || max(unlist(lapply(blocks, function(b) b$cols))) != ncol(mod$VCV)){
    # couldn't work out the structure, assume everything is a variance
    return(seq_len(ncol(mod$VCV)))
  }
  sort(unlist(lapply(blocks, function(b) b$diag.cols)))
}

# the M matrix of a block for a set of rows
.block_M <- function(b, rows){
  if(is.null(b$W)) return(diag(c(1, rep(0, b$k - 1)), b$k))
  W <- b$W[rows, , drop = FALSE]
  crossprod(W) / nrow(W)
}

# family of a set of rows of an MCMCglmm model
.MCMC_family <- function(mod, rows = NULL){
  fam <- as.character(mod$family)
  if(length(fam) != nrow(mod$X)) fam <- as.character(mod$Residual$family)
  else if(!is.null(rows)) fam <- fam[rows]
  fam <- unique(fam)
  if(length(fam) != 1) return("other")
  switch(fam,
         gaussian = , cengaussian = "gaussian",
         poisson = , cenpoisson = "poisson",
         multinomial2 = "logit",
         ordinal = "probit",
         threshold = "threshold",
         multinomial = "categorical", # one contrast of a > 2 category response
         "other")
}

# Traits of an MCMCglmm model: their names and the rows of X/Z/ZR belonging to
# each. MCMCglmm stacks the traits, naming the rows "<observation>.<trait>".
.MCMC_traits <- function(mod){
  n <- nrow(mod$X)
  lhs <- mod$Fixed$formula[[2]]
  if(!.is_multiresponse(mod)){
    return(list(names = all.vars(lhs)[1], rows = list(seq_len(n))))
  }
  idx <- suppressWarnings(as.integer(sub("^.*\\.", "", rownames(mod$X))))
  if(is.null(rownames(mod$X)) || anyNA(idx)){
    stop("Could not work out which rows of the model belong to which trait")
  }
  ids <- sort(unique(idx))
  nt <- length(ids)
  # names: from cbind(), else from the trait indicator columns, else numbered
  nm <- all.vars(lhs)
  if(length(nm) != nt){
    tc <- grep("^trait[^:]*$", colnames(mod$X), value = TRUE)
    nm <- if(length(tc) == nt) sub("^trait", "", tc) else paste0("trait", ids)
  }
  list(names = nm, rows = lapply(ids, function(i) which(idx == i)))
}

# cutpoints (S x (K - 1), first fixed at 0) for an ordinal/threshold trait, or 0
.MCMC_cutpoints <- function(mod, trait, ntraits){
  if(is.null(mod$CP)) return(0)
  CP <- as.matrix(mod$CP)
  cn <- colnames(CP)
  tr <- sub("\\.[0-9]+$", "", sub("^cutpoint\\.trait", "", cn))
  sel <- if(ntraits == 1) rep(TRUE, length(cn)) else tr == trait
  if(!any(sel)) return(0)
  cbind(0, CP[, sel, drop = FALSE])
}

.mer_family <- function(mod){
  if(inherits(mod, "lmerMod")) return("gaussian")
  f <- stats::family(mod)
  if(f$family == "poisson" && f$link == "log") return("poisson")
  if(f$family == "binomial" && f$link == "logit") return("logit")
  if(f$family == "binomial" && f$link == "probit") return("probit")
  if(f$family == "gaussian" && f$link == "identity") return("gaussian")
  "other"
}

.family_label <- function(family, cp = 0){
  if(family %in% c("probit", "threshold") && NCOL(cp) > 1){
    return(sprintf("%s with %d ordered categories (probit link)",
                   if(family == "probit") "ordinal" else "threshold", NCOL(cp) + 1))
  }
  switch(family,
         gaussian = "Gaussian (identity link)",
         poisson = "Poisson (log link)",
         logit = "binomial/categorical (logit link)",
         probit = "binomial/ordinal (probit link)",
         threshold = "threshold (probit link, latent residual)",
         categorical = "categorical contrast (log-odds vs the reference category)",
         category = "categorical (indicator of this category, data scale)",
         "unsupported family")
}

# One model description per trait (a single one for univariate and lme4 models)
.glmm_infos <- function(mod, nsim = 1000, n.trials = NULL, trait = NULL){
  if(!inherits(mod, "MCMCglmm")) return(list(.glmm_info(mod, nsim, n.trials)))
  tr <- .MCMC_traits(mod)
  which <- seq_along(tr$names)
  if(!is.null(trait)){
    which <- if(is.numeric(trait)) trait else match(trait, tr$names)
    if(anyNA(which) || any(which > length(tr$names))){
      stop("Unknown trait; the traits are: ", paste(tr$names, collapse = ", "))
    }
  }
  lapply(which, function(i) .glmm_info(mod, nsim, n.trials, trait = i))
}

# Build the common model description (for MCMCglmm, of one trait)
.glmm_info <- function(mod, nsim = 1000, n.trials = NULL, trait = 1){
  if(inherits(mod, "MCMCglmm")){
    tr <- .MCMC_traits(mod)
    rows <- tr$rows[[trait]]
    multi <- length(tr$names) > 1
    X <- mod$X[rows, , drop = FALSE]
    p <- mod$Fixed$nfl
    # only the fixed effects that apply to this trait
    used <- which(Matrix::colSums(abs(X)) > 0)
    beta <- as.matrix(mod$Sol[, used, drop = FALSE])
    X <- X[, used, drop = FALSE]
    blocks <- .MCMC_blocks(mod)
    if(length(blocks) == 0 || max(unlist(lapply(blocks, function(b) b$cols))) != ncol(mod$VCV)){
      stop("Could not match the VCV columns to the random/residual structure of this model")
    }
    if(!all(vapply(blocks, function(b) b$Zok, logical(1)))){
      warning("Random effect design matrices not stored (saveZ = FALSE?). ",
              "Covariance structures are evaluated at covariate values of zero.")
    }
    VCV <- as.matrix(mod$VCV)
    terms <- unique(vapply(blocks, function(b) b$term, character(1)))
    comps <- list()
    resid <- character(0)
    for(tm in terms){
      bl <- Filter(function(b) b$term == tm, blocks)
      v <- Reduce(`+`, lapply(bl, function(b) as.vector(VCV[, b$cols, drop = FALSE] %*% as.vector(.block_M(b, rows)))))
      # terms that don't apply to this trait (e.g. at.level()) contribute nothing
      if(multi && all(v == 0)) next
      comps[[tm]] <- v
      if(bl[[1]]$residual) resid <- c(resid, tm)
    }
    family <- .MCMC_family(mod, rows)
    if(is.null(n.trials)) n.trials <- 1
    rf <- if(multi) mod$Residual$original.family else unique(mod$Residual$original.family)
    out <- list(class = "MCMCglmm", family = family,
                raw.family = if(multi && length(rf) == length(tr$names)) rf[trait] else rf[1],
                trait = if(multi) tr$names[trait] else NULL,
                cp = if(family %in% c("probit", "threshold")) .MCMC_cutpoints(mod, tr$names[trait], length(tr$names)) else 0,
                n = nrow(X), S = nrow(beta), bayes = TRUE)
  } else if(inherits(mod, "merMod")){
    family <- .mer_family(mod)
    X <- lme4::getME(mod, "X")
    b <- lme4::fixef(mod)
    V <- as.matrix(stats::vcov(mod))
    # draws from the (approximate) sampling distribution of the fixed effects
    L <- tryCatch(chol(V), error = function(e) NULL)
    if(is.null(L)){
      beta <- matrix(b, nsim, length(b), byrow = TRUE)
    } else {
      beta <- matrix(stats::rnorm(nsim * length(b)), nsim) %*% L
      beta <- sweep(beta, 2, b, "+")
    }
    colnames(beta) <- names(b)
    Ztall <- lme4::getME(mod, "Zt")
    Gp <- lme4::getME(mod, "Gp")
    vc <- lme4::VarCorr(mod)
    comps <- list()
    for(t in seq_along(vc)){
      Sig <- as.matrix(vc[[t]])
      k <- nrow(Sig)
      Zt <- Ztall[(Gp[t] + 1):Gp[t + 1], , drop = FALSE]
      # within a term, lme4 orders the rows of Zt by level, then by effect within level
      W <- sapply(seq_len(k), function(j){
        Matrix::colSums(Zt[seq(j, nrow(Zt), by = k), , drop = FALSE])
      })
      W <- matrix(W, ncol = k)
      M <- crossprod(W) / nrow(W)
      nm <- names(vc)[t]
      if(k > 1 || !identical(colnames(Sig), "(Intercept)")){
        nm <- paste0(nm, " (", paste(colnames(Sig), collapse = ", "), ")")
      }
      if(nm %in% names(comps)) nm <- make.unique(c(names(comps), nm))[length(comps) + 1]
      comps[[nm]] <- rep(sum(Sig * M), nsim)
    }
    resid <- character(0)
    if(family == "gaussian"){
      comps[["Residual"]] <- rep(stats::sigma(mod)^2, nsim)
      resid <- "Residual"
    }
    if(is.null(n.trials)){
      n.trials <- if(family %in% c("logit", "probit")) mean(stats::weights(mod)) else 1
    }
    out <- list(class = "merMod", family = family,
                raw.family = if(inherits(mod, "lmerMod")) "gaussian" else stats::family(mod)$family,
                trait = NULL, cp = 0, n = nrow(X), S = nsim, bayes = FALSE)
  } else {
    stop("Model must be of class MCMCglmm or merMod (lme4)")
  }

  # moments of the fixed effect design
  n <- nrow(X)
  Xbar <- Matrix::colMeans(X)
  XtX <- as.matrix(Matrix::crossprod(X)) / n
  Xcov <- (XtX - tcrossprod(Xbar)) * n / max(n - 1, 1)
  out$beta <- beta
  out$X <- X
  out$mu <- as.vector(beta %*% Xbar)
  out$Vf <- pmax(rowSums((beta %*% Xcov) * beta), 0)
  out$comps <- comps
  out$resid <- resid
  out$n.trials <- n.trials
  out
}

## Data-scale maths --------------------------------------------------------
# All functions are for a single draw: mu is the mean of the linear predictor,
# V the total latent variance (everything that varies around mu), Va the part
# of V due to the component of interest. They follow de Villemereuil et al.
# (2016, Genetics 204: 1281-1294) and Nakagawa & Schielzeth (2010, Biol Rev 85:
# 935-956), computing everything by Gauss-Hermite quadrature unless a closed
# form exists.

# extra latent variance that is not part of V for the probit families
.extra_var <- function(family) switch(family, probit = 1, 0)

# distribution-specific variance used on the "link" scale
.dist_var <- function(family, mu, V){
  switch(family,
         gaussian = 0,
         poisson = log(1 + 1 / exp(mu + V / 2)), # lognormal approximation
         logit = pi^2 / 3,
         probit = 1,
         threshold = 0,
         NA_real_)
}

# For the probit families, y is scored 0, ..., K - 1 and P(y > k | l) =
# P(l + e > cp[k]) with cutpoints cp (cp = 0 for binary data, so that E[y] is
# the probability of a success). For K > 2 categories, E[y] is reported on the
# 1, ..., K scale of the category numbers instead; variances are unaffected.

# marginal probability of each category (probit families)
.cat_probs <- function(family, mu, V, cp = 0){
  gt <- stats::pnorm((mu - cp) / sqrt(V + .extra_var(family))) # P(y > k)
  -diff(c(1, gt, 0))
}

# expected value on the data scale
.ey <- function(family, mu, V, cp = 0){
  switch(family,
         gaussian = mu,
         poisson = exp(mu + V / 2),
         probit = , threshold = sum(stats::pnorm((mu - cp) / sqrt(V + .extra_var(family)))) + (length(cp) > 1),
         logit = { g <- .gh(); sum(g$w * stats::plogis(mu + sqrt(V) * g$x)) },
         NA_real_)
}

# total variance on the data scale (for binomial families this is the variance
# of the observed proportion out of n trials)
.vary <- function(family, mu, V, n = 1, cp = 0){
  switch(family,
         gaussian = V,
         poisson = { m <- exp(mu + V / 2); m + m^2 * (exp(V) - 1) },
         probit = , threshold = {
           p <- .cat_probs(family, mu, V, cp)
           k <- seq_along(p) - 1
           sum(k^2 * p) - sum(k * p)^2
         },
         logit = {
           g <- .gh()
           pr <- stats::plogis(mu + sqrt(V) * g$x)
           Ep <- sum(g$w * pr); Ep2 <- sum(g$w * pr^2)
           (Ep - Ep2) / n + (Ep2 - Ep^2)
         },
         NA_real_)
}

# variance of E[y | a] on the data scale, a ~ N(0, Va), the rest ~ N(0, V - Va)
.vcomp <- function(family, mu, Va, V, cp = 0){
  Vr <- max(V - Va, 0)
  switch(family,
         gaussian = Va,
         poisson = exp(2 * mu + V) * (exp(Va) - 1),
         probit = , threshold = {
           g <- .gh()
           s <- sqrt(max(Vr + .extra_var(family), 1e-12))
           # E[y | a] = sum_k P(y > k | a), rows: nodes for a, columns: cutpoints
           ga <- rowSums(stats::pnorm(outer(mu + sqrt(Va) * g$x, cp, `-`) / s))
           sum(g$w * ga^2) - sum(g$w * ga)^2
         },
         logit = {
           g <- .gh()
           # rows: nodes for a, columns: nodes for the rest
           pr <- stats::plogis(outer(mu + sqrt(Va) * g$x, sqrt(Vr) * g$x, `+`))
           ga <- as.vector(pr %*% g$w)
           sum(g$w * ga^2) - sum(g$w * ga)^2
         },
         NA_real_)
}

# helper to apply one of the above over draws: vectors of length S and
# matrices with S rows (cutpoints) vary by draw, anything else is constant
.over_draws <- function(f, ...){
  args <- list(...)
  S <- max(vapply(args[-1], function(x) if(is.matrix(x)) nrow(x) else length(x), numeric(1)))
  vapply(seq_len(S), function(s){
    a <- lapply(args, function(x){
      if(is.matrix(x) && nrow(x) == S) x[s, ]
      else if(!is.matrix(x) && length(x) == S && S > 1) x[s]
      else x
    })
    do.call(f, a)
  }, numeric(1))
}

# Which scales are meaningful for a model. lme4 GLMMs have no residual term on the
# latent scale, so latent-scale proportions are degenerate (they sum to 1 over
# the random effects alone) and only the link and data scales are used.
.usable_scales <- function(info, scale = c("latent", "link", "data")){
  if(info$family %in% c("other", "categorical")) return("latent")
  if(length(info$resid) == 0) scale <- setdiff(scale, "latent")
  if(length(scale) == 0) scale <- "link"
  scale
}

# significance stars, used by the print methods
.stars <- function(p){
  ifelse(is.na(p), "",
         ifelse(p <= 0.001, "***",
                ifelse(p <= 0.01, "**",
                       ifelse(p <= 0.05, "*", ""))))
}
