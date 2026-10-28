# Data scale for categorical (multinomial logit) responses with > 2 categories.
#
# MCMCglmm fits a K-category response as K - 1 latent traits l = (l_2, ..., l_K),
# the log-odds of each category against the reference (first) category, with
# P(y = k | l) = exp(l_k) / (1 + sum(exp(l))). Random and residual terms have a
# covariance matrix across these contrasts, so the data scale has to be
# computed jointly. For each category we use the indicator y_k = 1(y = k):
#   proportion explained by term a = Var_a(E[p_k | a]) / (pbar_k (1 - pbar_k))
# Var_a(E[p_k | a]) is estimated without nested simulation as
# Cov(p_k(mu + a + e1), p_k(mu + a + e2)): two latent draws sharing a but with
# independent e. The same standard normal draws are reused for every posterior
# draw (common random numbers), so Monte Carlo noise doesn't add to the
# posterior spread.

# Group the traits of an MCMCglmm model into categorical responses: runs of
# K - 1 traits with residual family "multinomialK", K > 2.
.MCMC_cat_groups <- function(mod){
  rf <- as.character(mod$Residual$family)
  if(!any(grepl("^multinomial[0-9]+$", rf))) return(list())
  tr <- .MCMC_traits(mod)
  if(length(rf) != length(tr$names)) return(list())
  out <- list()
  i <- 1
  while(i <= length(rf)){
    K <- suppressWarnings(as.integer(sub("^multinomial", "", rf[i])))
    if(!is.na(K) && K > 2){
      idx <- i:(i + K - 2)
      nm <- tr$names[idx]
      resp <- sub("\\.[^.]*$", "", nm[1])
      cats <- sub(paste0("^", resp, "\\."), "", nm)
      out[[length(out) + 1]] <- list(response = resp, traits = idx, K = K,
                                     categories = c("reference", cats),
                                     labels = paste0(resp, ": ", c("reference", cats)))
      i <- i + K - 1
    } else {
      i <- i + 1
    }
  }
  out
}

# Joint (across contrasts) description of one categorical response: mean
# linear predictor (S x d), fixed-effect covariance and the covariance matrix of
# each random/residual term (lists of S x d x d arrays).
.cat_info <- function(mod, grp){
  tr <- .MCMC_traits(mod)
  X <- mod$X
  obs <- sub("\\.[0-9]+$", "", rownames(X))
  rows <- tr$rows[grp$traits]
  ids <- obs[rows[[1]]]
  # pair the rows of the different contrasts by observation
  rows <- lapply(rows, function(r) r[match(ids, obs[r])])
  if(anyNA(unlist(rows))) stop("Could not pair the rows of the categorical response's contrasts")
  d <- length(rows)
  n <- length(ids)
  p <- mod$Fixed$nfl
  beta <- as.matrix(mod$Sol[, seq_len(p), drop = FALSE])
  S <- nrow(beta)

  Xk <- lapply(rows, function(r) X[r, , drop = FALSE])
  mk <- lapply(Xk, Matrix::colMeans)
  mu <- sapply(mk, function(m) as.vector(beta %*% m))
  mu <- matrix(mu, S, d)
  Vf <- array(0, c(S, d, d))
  for(k in 1:d) for(l in k:d){
    C <- (as.matrix(Matrix::crossprod(Xk[[k]], Xk[[l]])) / n - tcrossprod(mk[[k]], mk[[l]])) * n / max(n - 1, 1)
    v <- rowSums((beta %*% C) * beta)
    Vf[, k, l] <- Vf[, l, k] <- v
  }

  blocks <- .MCMC_blocks(mod)
  if(!all(vapply(blocks, function(b) b$Zok, logical(1)))){
    stop("The categorical data scale needs the random effect design matrices (fit with saveZ = TRUE)")
  }
  VCV <- as.matrix(mod$VCV)
  terms <- unique(vapply(blocks, function(b) b$term, character(1)))
  comps <- list()
  resid <- character(0)
  for(tm in terms){
    bl <- Filter(function(b) b$term == tm, blocks)
    A <- array(0, c(S, d, d))
    for(b in bl){
      for(k in 1:d) for(l in k:d){
        M <- crossprod(b$W[rows[[k]], , drop = FALSE], b$W[rows[[l]], , drop = FALSE]) / n
        v <- as.vector(VCV[, b$cols, drop = FALSE] %*% as.vector(M))
        A[, k, l] <- A[, k, l] + v
        if(l != k) A[, l, k] <- A[, l, k] + v
      }
    }
    if(all(A == 0)) next
    comps[[tm]] <- A
    if(bl[[1]]$residual) resid <- c(resid, tm)
  }

  # coefficients by contrast: baseline columns (all 1) and named covariates
  cn <- colnames(beta)
  coefs <- lapply(seq_len(d), function(k){
    Xs <- as.matrix(Xk[[k]][seq_len(min(n, 5000)), , drop = FALSE])
    used <- which(colSums(abs(Xs)) > 0)
    base <- used[apply(Xs[, used, drop = FALSE], 2, function(v) all(v == 1))]
    other <- setdiff(used, base)
    disp <- vapply(cn[other], function(nm){
      pieces <- strsplit(nm, ":", fixed = TRUE)[[1]]
      keep <- pieces[!pieces %in% cn[base]]
      if(length(keep) == 0 || length(keep) == length(pieces)) nm else paste(keep, collapse = ":")
    }, character(1))
    binary <- apply(Xs[, other, drop = FALSE], 2, function(v) all(v %in% c(0, 1)))
    list(base = base, other = other, disp = unname(disp), binary = binary)
  })

  list(grp = grp, S = S, d = d, n = n, beta = beta, mu = mu, Vf = Vf,
       comps = comps, resid = resid, coefs = coefs)
}

# softmax with the reference category first
.mn_p <- function(L){
  L <- cbind(0, L)
  # row maxima (for numerical stability); much faster than apply(L, 1, max)
  m <- do.call(pmax, lapply(seq_len(ncol(L)), function(j) L[, j]))
  e <- exp(L - m)
  e / rowSums(e)
}

# matrix square root (robust to singular matrices): Z %*% t(.msqrt(S)) ~ N(0, S)
.msqrt <- function(S){
  S <- (S + t(S)) / 2
  e <- eigen(S, symmetric = TRUE)
  e$vectors %*% diag(sqrt(pmax(e$values, 0)), nrow(S))
}

# common random numbers, without disturbing the user's RNG stream
.cat_Z <- function(N, d){
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if(had) old <- get(".Random.seed", envir = globalenv())
  on.exit(if(had) assign(".Random.seed", old, envir = globalenv())
          else if(exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv()))
  set.seed(5L)
  list(a = matrix(stats::rnorm(N * d), N), e1 = matrix(stats::rnorm(N * d), N),
       e2 = matrix(stats::rnorm(N * d), N))
}

# mean category probabilities for latent mean mu and covariance S
.mn_pbar <- function(mu, S, Z){
  colMeans(.mn_p(sweep(Z$e1 %*% t(.msqrt(S)), 2, mu, "+")))
}

# Var_a(E[p_k | a]) for each category, a ~ N(0, Sa), the rest ~ N(0, Stot - Sa)
.mn_vcomp <- function(mu, Sa, Stot, Z){
  a <- Z$a %*% t(.msqrt(Sa))
  Lr <- .msqrt(Stot - Sa)
  P1 <- .mn_p(sweep(a + Z$e1 %*% t(Lr), 2, mu, "+"))
  P2 <- .mn_p(sweep(a + Z$e2 %*% t(Lr), 2, mu, "+"))
  pbar <- (colMeans(P1) + colMeans(P2)) / 2
  pmax(colMeans(P1 * P2) - pbar^2, 0)
}

# data-scale proportions per category: list by category label of S x components matrices
.cat_varpart_draws <- function(ci, fixed, nmc = 10000){
  Z <- .cat_Z(nmc, ci$d)
  K <- ci$d + 1
  cn <- names(ci$comps)
  out <- array(NA_real_, c(ci$S, K, length(cn) + fixed))
  for(s in seq_len(ci$S)){
    Ss <- lapply(ci$comps, function(A) A[s, , , drop = TRUE])
    Ss <- lapply(Ss, function(m) matrix(m, ci$d, ci$d))
    Vf <- matrix(ci$Vf[s, , ], ci$d, ci$d)
    Stot <- Reduce(`+`, Ss) + if(fixed) Vf else 0
    pb <- .mn_pbar(ci$mu[s, ], Stot, Z)
    vy <- pb * (1 - pb)
    j <- 0
    if(fixed){
      j <- 1
      out[s, , 1] <- .mn_vcomp(ci$mu[s, ], Vf, Stot, Z) / vy
    }
    for(c in seq_along(Ss)) out[s, , j + c] <- .mn_vcomp(ci$mu[s, ], Ss[[c]], Stot, Z) / vy
  }
  nm <- c(if(fixed) "Fixed effects", cn)
  stats::setNames(lapply(seq_len(K), function(k){
    m <- matrix(out[, k, ], ci$S, length(nm))
    colnames(m) <- nm
    m
  }), ci$grp$labels)
}

# data-scale marginal and conditional R2 per category
.cat_R2_draws <- function(ci, nmc = 10000){
  Z <- .cat_Z(nmc, ci$d)
  K <- ci$d + 1
  G <- setdiff(names(ci$comps), ci$resid)
  out <- array(NA_real_, c(ci$S, K, 2))
  for(s in seq_len(ci$S)){
    Ss <- lapply(ci$comps, function(A) matrix(A[s, , ], ci$d, ci$d))
    Vf <- matrix(ci$Vf[s, , ], ci$d, ci$d)
    Stot <- Reduce(`+`, Ss) + Vf
    Gs <- Vf + if(length(G)) Reduce(`+`, Ss[G]) else 0
    pb <- .mn_pbar(ci$mu[s, ], Stot, Z)
    vy <- pb * (1 - pb)
    out[s, , 1] <- .mn_vcomp(ci$mu[s, ], Vf, Stot, Z) / vy
    out[s, , 2] <- .mn_vcomp(ci$mu[s, ], Gs, Stot, Z) / vy
  }
  stats::setNames(lapply(seq_len(K), function(k){
    m <- matrix(out[, k, ], ci$S, 2)
    colnames(m) <- c("R2m", "R2c")
    m
  }), ci$grp$labels)
}

# glmm_varpart()/glmm_R2() rows for the categorical responses of a model
.cat_parts <- function(mod, prob, fixed, trait, what = c("varpart", "R2"), nmc = 10000){
  what <- match.arg(what)
  if(!inherits(mod, "MCMCglmm")) return(list())
  groups <- .MCMC_cat_groups(mod)
  if(!is.null(trait)){
    tr <- .MCMC_traits(mod)
    sel <- if(is.numeric(trait)) trait else match(trait, tr$names)
    groups <- Filter(function(g) any(g$traits %in% sel), groups)
  }
  parts <- list()
  for(g in groups){
    message(sprintf("Categorical response %s: latent scale per contrast (%s); data scale per category (%s), by simulation.",
                    g$response, paste(.MCMC_traits(mod)$names[g$traits], collapse = ", "),
                    paste(g$categories, collapse = ", ")))
    ci <- .cat_info(mod, g)
    draws <- if(what == "varpart") .cat_varpart_draws(ci, fixed, nmc) else .cat_R2_draws(ci, nmc)
    for(lab in names(draws)){
      parts[[length(parts) + 1]] <- .varpart_draws_to_df(
        list(data = draws[[lab]]),
        list(trait = lab, family = "category", cp = 0, bayes = TRUE),
        prob, fixed = if(what == "R2") TRUE else fixed)
    }
  }
  parts
}

# glmm_explain() section for one categorical response
.explain_categorical <- function(mod, g, prob, add, para, wrap, f, pc, ci, nmc){
  cinfo <- .cat_info(mod, g)
  d <- cinfo$d
  S <- cinfo$S
  Z <- .cat_Z(nmc, d)
  add(sprintf("======== Categorical response: %s (%d categories) - data scale ========", g$response, g$K))
  para(sprintf("Probabilities of each category (%s), averaging over the random effects and the units term, with other covariates at 0 / reference levels; computed jointly over the contrasts by simulation. The reference category is the first level of the response (its name isn't stored in the model).",
               paste(g$categories, collapse = ", ")), 0)
  add("")

  # latent covariance of everything except the fixed effects, per draw
  sq <- lapply(seq_len(S), function(s){
    .msqrt(Reduce(`+`, lapply(cinfo$comps, function(A) matrix(A[s, , ], d, d))))
  })
  probs <- function(eta){
    t(vapply(seq_len(S), function(s){
      colMeans(.mn_p(sweep(Z$e1 %*% t(sq[[s]]), 2, eta[s, ], "+")))
    }, numeric(d + 1)))
  }
  cats <- g$categories
  eta0 <- sapply(cinfo$coefs, function(cf) rowSums(cinfo$beta[, cf$base, drop = FALSE]))
  eta0 <- matrix(eta0, S, d)
  P0 <- probs(eta0)
  s0 <- .post_summary(P0, prob)
  add(wrap(sprintf("Baseline probabilities: %s.",
                   paste(sprintf("%s %s %s", cats, f(s0$mean), ci(s0$lower, s0$upper)), collapse = "; ")), 2))

  vars <- unique(unlist(lapply(cinfo$coefs, `[[`, "disp")))
  for(v in vars){
    delta <- matrix(0, S, d)
    binary <- NA
    for(k in seq_len(d)){
      cf <- cinfo$coefs[[k]]
      idx <- cf$other[cf$disp == v]
      if(length(idx)){
        delta[, k] <- rowSums(cinfo$beta[, idx, drop = FALSE])
        if(is.na(binary)) binary <- cf$binary[cf$disp == v][1]
      }
    }
    D <- probs(eta0 + delta) - P0
    sd <- .post_summary(D, prob)
    change <- if(grepl(":", v)) {
      sprintf("the interaction %s (shown at baseline only)", v)
    } else if(isTRUE(binary)) {
      sprintf("going from the reference level to '%s'", v)
    } else sprintf("a one-unit increase in %s", v)
    excl <- sd$lower > 0 | sd$upper < 0
    line <- sprintf("%s changes the category probabilities by (percentage points): %s.%s", change,
                    paste(sprintf("%s %s %s%s", cats, f(100 * sd$mean), ci(100 * sd$lower, 100 * sd$upper),
                                  ifelse(excl, "*", "")), collapse = "; "),
                    if(any(excl)) " (* interval excludes zero)" else " No interval excludes zero.")
    substr(line, 1, 1) <- toupper(substr(line, 1, 1))
    add(wrap(line, 2))
  }
  add("")

  # variance in each category indicator
  vpd <- .cat_varpart_draws(cinfo, FALSE, nmc)
  r2d <- .cat_R2_draws(cinfo, nmc)
  para("Proportion of variance in each category indicator 1(y = k) explained (adjusted, data scale; mode and interval):", 0)
  for(cn in names(cinfo$comps)){
    parts <- vapply(seq_along(vpd), function(k){
      s <- .post_summary(vpd[[k]][, cn, drop = FALSE], prob)
      sprintf("%s %s %s", cats[k], pc(s$mode), ci(s$lower, s$upper, pc))
    }, character(1))
    add(wrap(sprintf("%s term '%s': %s.", if(cn %in% cinfo$resid) "Residual" else "Random", cn,
                     paste(parts, collapse = "; ")), 2))
  }
  parts <- vapply(seq_along(r2d), function(k){
    s <- .post_summary(r2d[[k]], prob)
    sprintf("%s R2m %s, R2c %s", cats[k], pc(s["R2m", "mode"]), pc(s["R2c", "mode"]))
  }, character(1))
  add(wrap(sprintf("R-squared (data scale, mode): %s.", paste(parts, collapse = "; ")), 2))
  add("")

  info <- function(lab) list(trait = lab, family = "category", cp = 0, bayes = TRUE)
  list(varpart = lapply(names(vpd), function(lab) .varpart_draws_to_df(list(data = vpd[[lab]]), info(lab), prob, FALSE)),
       R2 = lapply(names(r2d), function(lab) .varpart_draws_to_df(list(data = r2d[[lab]]), info(lab), prob, TRUE)))
}
