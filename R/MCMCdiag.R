#' Convergence and mixing diagnostics for MCMCglmm models
#'
#' Collects the usual MCMC diagnostics for every fixed effect and (co)variance component in
#' one table and flags anything that looks problematic. The print method gives a plain-English
#' list of issues and suggestions.
#'
#' For each parameter the following are reported:
#' \describe{
#'   \item{ESS}{effective sample size (\code{coda::effectiveSize}). Flagged when below \code{min.ess}.}
#'   \item{acf1}{autocorrelation between successive stored samples. Flagged when above \code{max.acf}
#'   (increase \code{thin}).}
#'   \item{geweke.z}{Geweke's z-score comparing the first 10\% and last 50\% of the chain. Flagged when
#'   |z| > 2.58 (not stationary, increase \code{burnin}).}
#'   \item{heidel}{whether the Heidelberger-Welch stationarity test passed.}
#' The two stationarity tests can disagree. Geweke's test compares the start of the chain with its
#' second half, so a drift in the first 10\% (or strong autocorrelation) inflates z. The
#' Heidelberger-Welch test discards up to half of the chain from the start until the rest looks
#' stationary, so it passes when only the early part drifts. A Geweke failure with an H-W pass
#' therefore usually means the burn-in is too short. When they disagree, the issue text says so.
#'   \item{boundary}{variances only: the posterior is piled up against zero (lower HPD bound below
#'   1\% of the posterior median). The estimate is then likely sensitive to the prior; see \code{MCMCpriorplot()}.}
#' }
#' In addition, the model-level checks look at the number of stored samples, strong posterior
#' correlations between fixed effects (poor identifiability or collinearity), whether a residual
#' variance that cannot be estimated (binary data) was fixed, and extreme latent-scale coefficients that
#' suggest (quasi-)complete separation. Coefficients are first divided by the latent standard deviation
#' (all variance components plus the distribution-specific variance), because they grow with it: an
#' intercept of -6 is unremarkable when the random effects have a variance of 20.
#'
#' @param mod model of class MCMCglmm
#' @param min.ess minimum acceptable effective sample size
#' @param max.acf maximum acceptable lag-1 autocorrelation
#' @param max.cor fixed effects whose posterior correlation exceeds this (in absolute value) are reported
#' @param pr logical. Also include the random effect predictions (needs \code{pr = TRUE} in the model)?
#' @return a data.frame of class \code{MCMCdiag}; the model-level issues are in \code{attr(x, "issues")}.
#' @keywords MCMCglmm, convergence, diagnostics, effective sample size
#' @seealso \code{\link{MCMCtrace}}, \code{\link{MCMCacfplot}}, \code{\link{MCMCgelman}}
#' @import coda
#' @export
#' @examples
#' \donttest{
#' library(MCMCglmm)
#' d <- data.frame(g = factor(rep(1:20, each = 10)), x = rnorm(200))
#' d$y <- d$x + rnorm(20)[d$g] + rnorm(200)
#' mod <- MCMCglmm(y ~ x, random = ~ g, data = d, verbose = FALSE)
#' MCMCdiag(mod)
#' }

MCMCdiag <- function(mod, min.ess = 400, max.acf = 0.1, max.cor = 0.9, pr = FALSE){

  .check_MCMCglmm(mod)

  nfl <- mod$Fixed$nfl
  Sol <- if(pr) mod$Sol else mod$Sol[, seq_len(nfl), drop = FALSE]
  S <- nrow(mod$Sol)
  var.cols <- .MCMC_variance_cols(mod)

  type <- c(ifelse(seq_len(ncol(Sol)) <= nfl, "fixed", "random effect"),
            ifelse(seq_len(ncol(mod$VCV)) %in% var.cols, "variance", "covariance"))
  chains <- cbind(as.matrix(Sol), as.matrix(mod$VCV))
  colnames(chains) <- c(colnames(Sol), colnames(mod$VCV))

  diag1 <- function(v){
    if(stats::var(v) == 0){
      return(c(ESS = NA, acf1 = NA, geweke.z = NA, heidel = NA))
    }
    m <- coda::as.mcmc(v)
    ess <- coda::effectiveSize(m)
    a1 <- stats::acf(v, lag.max = 1, plot = FALSE)$acf[2]
    gz <- tryCatch(coda::geweke.diag(m)$z, error = function(e) NA)
    hw <- tryCatch(suppressWarnings(coda::heidel.diag(m)[1, "stest"]), error = function(e) NA)
    c(ESS = unname(ess), acf1 = unname(a1), geweke.z = unname(gz), heidel = unname(hw))
  }
  d <- t(apply(chains, 2, diag1))
  sm <- .post_summary(chains)

  res <- data.frame(parameter = colnames(chains), type = type,
                    post.mean = sm$mean, lower = sm$lower, upper = sm$upper,
                    ESS = round(d[, "ESS"]), ESS.ratio = d[, "ESS"] / S,
                    acf1 = d[, "acf1"], geweke.z = d[, "geweke.z"],
                    heidel = ifelse(is.na(d[, "heidel"]), NA, d[, "heidel"] == 1),
                    stringsAsFactors = FALSE, row.names = NULL)
  res$fixed.par <- apply(chains, 2, stats::var) == 0
  res$boundary <- res$type == "variance" & !res$fixed.par &
    res$lower < 0.01 * apply(chains, 2, stats::median)

  flag <- function(i){
    if(res$fixed.par[i]) return("fixed in prior")
    f <- c(if(!is.na(res$ESS[i]) && res$ESS[i] < min.ess) "low ESS",
           if(!is.na(res$acf1[i]) && res$acf1[i] > max.acf) "autocorrelated",
           if(!is.na(res$geweke.z[i]) && abs(res$geweke.z[i]) > 2.58) "not stationary (Geweke)",
           if(!is.na(res$heidel[i]) && !res$heidel[i]) "not stationary (H-W)",
           if(res$boundary[i]) "near zero")
    paste(f, collapse = ", ")
  }
  res$flags <- vapply(seq_len(nrow(res)), flag, character(1))

  ## model-level issues
  issues <- character(0)
  mc <- attr(mod$Sol, "mcpar")
  if(S < 1000){
    issues <- c(issues, sprintf("Only %d samples stored; aim for >= 1000 (increase nitt or reduce thin).", S))
  }
  low <- res$parameter[grepl("low ESS", res$flags)]
  if(length(low)){
    issues <- c(issues, sprintf("Effective sample size < %d for: %s. Run the chain longer (nitt) or improve mixing (parameter-expanded priors for variances).",
                                min.ess, paste(low, collapse = ", ")))
  }
  ac <- res$parameter[grepl("autocorrelated", res$flags)]
  if(length(ac)){
    issues <- c(issues, sprintf("Lag-1 autocorrelation > %s for: %s. Increase thin (currently %d).",
                                max.acf, paste(ac, collapse = ", "), mc[3]))
  }
  ns <- res$parameter[grepl("not stationary", res$flags)]
  if(length(ns)){
    issues <- c(issues, sprintf("Possible lack of convergence for: %s. Increase burnin (currently %d iterations discarded) and check traces with MCMCtrace(); compare chains with MCMCgelman().",
                                paste(ns, collapse = ", "), mc[1] - 1))
    gw <- grepl("(Geweke)", res$flags, fixed = TRUE)
    hw <- grepl("(H-W)", res$flags, fixed = TRUE)
    if(any(gw & !hw & !is.na(res$heidel))){
      issues <- c(issues, sprintf("The stationarity tests disagree for %s: Geweke fails but Heidelberger-Welch passes. H-W may discard up to half of the chain before testing, whereas Geweke compares the first 10%% with the second half, so this usually means the start of the chain is still drifting (or strong autocorrelation is inflating z). Look at the start of the trace; a longer burnin normally resolves it.",
                                  paste(res$parameter[gw & !hw & !is.na(res$heidel)], collapse = ", ")))
    }
    if(any(hw & !gw & !is.na(res$geweke.z))){
      issues <- c(issues, sprintf("The stationarity tests disagree for %s: Heidelberger-Welch fails but Geweke passes. H-W looks at the whole chain, so this suggests a slow drift or a change late in the chain that the first 10%% vs second half comparison misses. Run the chain longer and compare chains with MCMCgelman().",
                                  paste(res$parameter[hw & !gw & !is.na(res$geweke.z)], collapse = ", ")))
    }
  }
  bd <- res$parameter[res$boundary]
  if(length(bd)){
    issues <- c(issues, sprintf("Variance posterior piled up near zero for: %s. The data contain little information about these; check prior sensitivity with MCMCpriorplot() and consider parameter-expanded priors.",
                                paste(bd, collapse = ", ")))
  }

  # fixed effect correlations
  if(nfl > 1){
    fs <- as.matrix(mod$Sol[, seq_len(nfl), drop = FALSE])
    keep <- apply(fs, 2, stats::var) > 0
    if(sum(keep) > 1){
      cm <- stats::cor(fs[, keep, drop = FALSE])
      hi <- which(abs(cm) > max.cor & upper.tri(cm), arr.ind = TRUE)
      if(nrow(hi)){
        pairs <- sprintf("%s & %s (r = %.2f)", rownames(cm)[hi[, 1]], colnames(cm)[hi[, 2]], cm[hi])
        issues <- c(issues, sprintf("Strongly correlated fixed effects: %s. Consider centring/scaling covariates or check for collinearity.",
                                    paste(pairs, collapse = "; ")))
      }
    }
  }

  # binary data: residual variance not identifiable
  fam <- .MCMC_family(mod)
  if(fam %in% c("logit", "probit", "threshold") && "units" %in% colnames(mod$VCV) &&
     stats::var(mod$VCV[, "units"]) > 0 && all(mod$Residual$original.family %in% c("categorical", "ordinal", "threshold"))){
    issues <- c(issues, "Binary response with an estimated residual (units) variance: it is not identifiable. Fix it in the prior, e.g. R = list(V = 1, fix = 1).")
  }
  if(fam %in% c("logit", "probit", "threshold")){
    # coefficients scale with the latent standard deviation sqrt(V + distribution variance),
    # so standardise them first: a value of 5 is a probability of about 1e-7 or 1 - 1e-7
    infos <- tryCatch(suppressWarnings(.glmm_infos(mod)), error = function(e) NULL)
    if(is.null(infos)){
      b0 <- colMeans(as.matrix(mod$Sol[, seq_len(nfl), drop = FALSE]))
      big <- names(b0)[abs(b0) > if(fam == "logit") 10 else 5]
    } else {
      big <- unique(unlist(lapply(infos, function(info){
        if(!info$family %in% c("logit", "probit", "threshold")) return(NULL)
        sdl <- sqrt(rowSums(do.call(cbind, c(list(0), info$comps))) + .extra_var(info$family) +
                      if(info$family == "logit") pi^2 / 3 else 0)
        b0 <- colMeans(info$beta / sdl)
        names(b0)[abs(b0) > 5]
      })))
    }
    if(length(big)){
      issues <- c(issues, sprintf("Very large standardised latent-scale coefficients (%s, more than 5 latent standard deviations): possible (quasi-)complete separation; consider a stronger prior on the fixed effects (B).",
                                  paste(big, collapse = ", ")))
    }
  }

  class(res) <- c("MCMCdiag", "data.frame")
  attr(res, "issues") <- issues
  attr(res, "n.samples") <- S
  attr(res, "mcpar") <- mc
  res
}

#' @export
print.MCMCdiag <- function(x, digits = 3, ...){
  mc <- attr(x, "mcpar")
  # subsets (x[i, j]) lose the attributes: print the table only
  if(is.null(mc)){
    print(as.data.frame(unclass(x)), row.names = FALSE, digits = digits, ...)
    return(invisible(x))
  }
  cat(sprintf("MCMC diagnostics: %d samples (iterations %d-%d, thin %d)\n\n",
              attr(x, "n.samples"), mc[1], mc[2], mc[3]))
  y <- as.data.frame(unclass(x))
  # a column subset (x[, cols]) keeps the class: print whatever columns are left
  cols <- intersect(c("parameter", "type", "post.mean", "ESS", "acf1", "geweke.z", "heidel", "flags"), names(y))
  if(length(cols)) y <- y[, cols, drop = FALSE]
  num <- intersect(c("post.mean", "acf1", "geweke.z"), names(y))
  y[num] <- lapply(y[num], signif, digits = digits)
  print(y, row.names = FALSE)
  cat("\n")
  iss <- attr(x, "issues")
  if(length(iss) == 0){
    cat("No problems detected. (Diagnostics from a single chain can't prove convergence; compare several chains with MCMCgelman().)\n")
  } else {
    cat("Potential problems:\n")
    for(i in iss) cat(paste(strwrap(i, width = getOption("width") - 4, prefix = "    ", initial = "  - "), collapse = "\n"), "\n")
  }
  invisible(x)
}

#' Gelman-Rubin diagnostics across several MCMCglmm chains
#'
#' Fit the same model several times (ideally with dispersed starting values, see the
#' \code{start} argument of \code{MCMCglmm}), and pass the fits here. Potential scale
#' reduction factors (PSRF, "R-hat") close to 1 indicate the chains agree; values above
#' about 1.05 suggest the chains have not converged to the same distribution.
#'
#' @param ... two or more MCMCglmm models, or a single list of them
#' @param which "Sol" (fixed effects), "VCV" or both
#' @param threshold PSRF above which a parameter is flagged
#' @return data.frame with point estimate and upper CI of the PSRF per parameter
#' @keywords MCMCglmm, convergence, Gelman-Rubin, Rhat
#' @export
#' @examples
#' \donttest{
#' library(MCMCglmm)
#' d <- data.frame(g = factor(rep(1:20, each = 10)), x = rnorm(200))
#' d$y <- d$x + rnorm(20)[d$g] + rnorm(200)
#' chains <- lapply(1:3, function(i) MCMCglmm(y ~ x, random = ~ g, data = d, verbose = FALSE))
#' MCMCgelman(chains)
#' }

MCMCgelman <- function(..., which = c("Sol", "VCV"), threshold = 1.05){
  mods <- list(...)
  if(length(mods) == 1 && is.list(mods[[1]]) && !inherits(mods[[1]], "MCMCglmm")) mods <- mods[[1]]
  if(length(mods) < 2) stop("Supply at least two chains (models)")
  lapply(mods, .check_MCMCglmm)
  which <- match.arg(which, several.ok = TRUE)

  out <- list()
  for(w in which){
    chains <- lapply(mods, function(m){
      if(w == "Sol") m$Sol[, seq_len(m$Fixed$nfl), drop = FALSE] else m$VCV
    })
    # drop parameters fixed in the prior
    keep <- apply(as.matrix(chains[[1]]), 2, stats::var) > 0
    chains <- coda::mcmc.list(lapply(chains, function(x) coda::as.mcmc(as.matrix(x)[, keep, drop = FALSE])))
    g <- coda::gelman.diag(chains, autoburnin = FALSE, multivariate = FALSE)$psrf
    out[[w]] <- data.frame(parameter = rownames(g), component = w,
                           PSRF = g[, 1], upper.CI = g[, 2],
                           converged = g[, 2] < threshold,
                           row.names = NULL, stringsAsFactors = FALSE)
  }
  res <- do.call(rbind, out)
  rownames(res) <- NULL
  if(any(!res$converged)){
    message("Chains disagree (upper CI of PSRF > ", threshold, ") for: ",
            paste(res$parameter[!res$converged], collapse = ", "))
  }
  res
}
