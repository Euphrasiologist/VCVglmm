#' ggplot2 trace plots for one or more MCMCglmm chains
#'
#' Trace plots with a running mean (dashed), which should flatten out quickly if the chain
#' has converged. If several models (chains of the same model) are supplied they are overlaid in
#' different colours: they should be indistinguishable.
#'
#' @param mod model of class MCMCglmm, or a list of them (chains)
#' @param which "VCV" (variance components) or "Sol" (fixed effects)
#' @param pars optional character vector of parameter names (or regular expression) to plot
#' @param density logical, plot posterior densities instead of traces
#' @keywords MCMCglmm, trace, convergence, plot
#' @import ggplot2
#' @export
#' @examples
#' \donttest{
#' library(MCMCglmm)
#' d <- data.frame(g = factor(rep(1:20, each = 10)), x = rnorm(200))
#' d$y <- d$x + rnorm(20)[d$g] + rnorm(200)
#' mod <- MCMCglmm(y ~ x, random = ~ g, data = d, verbose = FALSE)
#' MCMCtrace(mod)
#' MCMCtrace(mod, which = "Sol", density = TRUE)
#' }

MCMCtrace <- function(mod, which = c("VCV", "Sol"), pars = NULL, density = FALSE){
  which <- match.arg(which)
  mods <- if(inherits(mod, "MCMCglmm")) list(mod) else mod
  lapply(mods, .check_MCMCglmm)

  long <- do.call(rbind, lapply(seq_along(mods), function(i){
    m <- mods[[i]]
    x <- if(which == "Sol") m$Sol[, seq_len(m$Fixed$nfl), drop = FALSE] else m$VCV
    it <- seq(attr(x, "mcpar")[1], by = attr(x, "mcpar")[3], length.out = nrow(x))
    x <- as.matrix(x)
    if(!is.null(pars)){
      sel <- colnames(x) %in% pars | grepl(paste(pars, collapse = "|"), colnames(x))
      x <- x[, sel, drop = FALSE]
    }
    data.frame(chain = factor(i), iteration = rep(it, ncol(x)),
               parameter = factor(rep(colnames(x), each = nrow(x)), levels = colnames(x)),
               value = as.vector(x),
               running.mean = as.vector(apply(x, 2, function(v) cumsum(v) / seq_along(v))))
  }))
  if(nrow(long) == 0) stop("No parameters selected")

  multi <- length(mods) > 1
  if(density){
    p <- ggplot(long, aes(x = value)) +
      {if(multi) geom_density(aes(colour = chain)) else geom_density(fill = "grey80")} +
      facet_wrap(~ parameter, scales = "free") +
      labs(x = "Value", y = "Posterior density")
  } else {
    p <- ggplot(long, aes(x = iteration, y = value)) +
      {if(multi) geom_line(aes(colour = chain), alpha = 0.6) else geom_line(colour = "grey40")} +
      geom_line(aes(y = running.mean, group = chain), linetype = "dashed", colour = "red") +
      facet_wrap(~ parameter, scales = "free_y") +
      labs(x = "Iteration", y = "Value")
  }
  p + theme_bw()
}

#' Autocorrelation plot for MCMCglmm chains
#'
#' Autocorrelation of the stored samples by lag. Bars should drop below the dashed line
#' (0.1) by lag 1; if not, increase \code{thin} (and \code{nitt}).
#'
#' @param mod model of class MCMCglmm
#' @param which "VCV" or "Sol"
#' @param lag.max maximum lag
#' @keywords MCMCglmm, autocorrelation, plot
#' @import ggplot2
#' @export

MCMCacfplot <- function(mod, which = c("VCV", "Sol"), lag.max = 20){
  .check_MCMCglmm(mod)
  which <- match.arg(which)
  x <- if(which == "Sol") mod$Sol[, seq_len(mod$Fixed$nfl), drop = FALSE] else mod$VCV
  x <- as.matrix(x)
  x <- x[, apply(x, 2, stats::var) > 0, drop = FALSE]
  long <- do.call(rbind, lapply(colnames(x), function(cn){
    a <- stats::acf(x[, cn], lag.max = lag.max, plot = FALSE)
    data.frame(parameter = cn, lag = as.vector(a$lag), acf = as.vector(a$acf))
  }))
  long$parameter <- factor(long$parameter, levels = colnames(x))
  ggplot(long[long$lag > 0, ], aes(x = lag, y = acf)) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = c(-0.1, 0.1), linetype = "dashed", colour = "red") +
    geom_segment(aes(xend = lag, yend = 0)) +
    facet_wrap(~ parameter) +
    theme_bw() +
    labs(x = "Lag (in stored samples)", y = "Autocorrelation")
}

#' Heatmap of posterior correlations between parameters
#'
#' Strong correlations between fixed effects indicate collinearity or poorly identified
#' parameters (e.g. an intercept and an uncentred covariate), and are a common cause of
#' poor mixing.
#'
#' @param mod model of class MCMCglmm
#' @param which "Sol" (fixed effects), "VCV" or "both"
#' @keywords MCMCglmm, correlation, plot
#' @import ggplot2
#' @export

MCMCcorplot <- function(mod, which = c("Sol", "VCV", "both")){
  .check_MCMCglmm(mod)
  which <- match.arg(which)
  x <- switch(which,
              Sol = mod$Sol[, seq_len(mod$Fixed$nfl), drop = FALSE],
              VCV = mod$VCV,
              both = cbind(as.matrix(mod$Sol[, seq_len(mod$Fixed$nfl), drop = FALSE]), as.matrix(mod$VCV)))
  x <- as.matrix(x)
  x <- x[, apply(x, 2, stats::var) > 0, drop = FALSE]
  cm <- stats::cor(x)
  long <- data.frame(row = factor(rep(rownames(cm), ncol(cm)), levels = rev(rownames(cm))),
                     col = factor(rep(colnames(cm), each = nrow(cm)), levels = colnames(cm)),
                     r = as.vector(cm))
  ggplot(long, aes(x = col, y = row, fill = r)) +
    geom_tile(colour = "white") +
    geom_text(aes(label = sprintf("%.2f", r)), size = 3) +
    scale_fill_gradient2(limits = c(-1, 1), low = "#2166ac", mid = "white", high = "#b2182b") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = NULL, y = NULL, fill = "Posterior\ncorrelation")
}

#' Compare prior and posterior distributions of variance components
#'
#' If the posterior of a variance component looks much like its prior, the data contain
#' little information about it and conclusions depend on the prior. This simulates from the
#' (possibly parameter-expanded) inverse-Wishart prior of each scalar variance component and
#' overlays it on the posterior, on a log10 scale, zoomed to an order of magnitude either side of
#' the posterior.
#'
#' Only variance components with a scalar (1 x 1) prior are supported; others are skipped.
#' Prior elements are matched to the model in order: G1, G2, ... to the random terms, R to the
#' residual. A term like \code{idh(f):g} with a multi-dimensional prior is skipped.
#'
#' @param mod model of class MCMCglmm
#' @param prior the prior list that was passed to \code{MCMCglmm()}
#' @param n number of draws from the prior
#' @keywords MCMCglmm, prior, sensitivity, plot
#' @import ggplot2
#' @export
#' @examples
#' \donttest{
#' library(MCMCglmm)
#' d <- data.frame(g = factor(rep(1:20, each = 10)), x = rnorm(200))
#' d$y <- d$x + rnorm(20)[d$g] + rnorm(200)
#' prior <- list(R = list(V = 1, nu = 0.002),
#'               G = list(G1 = list(V = 1, nu = 1, alpha.mu = 0, alpha.V = 1000)))
#' mod <- MCMCglmm(y ~ x, random = ~ g, data = d, prior = prior, verbose = FALSE)
#' MCMCpriorplot(mod, prior)
#' }

MCMCpriorplot <- function(mod, prior, n = 10000){
  .check_MCMCglmm(mod)
  blocks <- .MCMC_blocks(mod)
  G <- Filter(function(b) !b$residual, blocks)
  R <- Filter(function(b) b$residual, blocks)
  pri <- c(prior$G, if(is.list(prior$R) && !is.null(prior$R$V)) list(R = prior$R) else prior$R)
  bl <- c(G, R)
  if(length(pri) != length(bl)){
    stop("Could not match the prior elements to the (co)variance structures of the model")
  }

  sim <- function(p){
    V <- as.matrix(p$V)
    if(!all(dim(V) == 1) || !is.null(p$fix)) return(NULL)
    nu <- if(is.null(p$nu)) 0 else p$nu
    if(nu <= 0) return(NULL)
    s2 <- 1 / stats::rgamma(n, shape = nu / 2, rate = nu * V[1, 1] / 2)
    if(!is.null(p$alpha.V)){
      a.mu <- if(is.null(p$alpha.mu)) 0 else p$alpha.mu
      s2 <- s2 * stats::rnorm(n, a.mu, sqrt(p$alpha.V))^2
    }
    s2
  }

  long <- list()
  skipped <- character(0)
  for(i in seq_along(bl)){
    nm <- colnames(mod$VCV)[bl[[i]]$diag.cols[1]]
    if(bl[[i]]$k != 1){ skipped <- c(skipped, nm); next }
    pr <- sim(pri[[i]])
    if(is.null(pr)){ skipped <- c(skipped, nm); next }
    post <- as.vector(mod$VCV[, bl[[i]]$cols])
    long[[nm]] <- rbind(data.frame(parameter = nm, distribution = "prior", value = pr),
                        data.frame(parameter = nm, distribution = "posterior", value = post))
  }
  if(length(skipped)){
    message("Skipped (fixed, improper or non-scalar prior): ", paste(skipped, collapse = ", "))
  }
  if(length(long) == 0) stop("No variance components with a usable scalar prior")
  # densities of log10(variance), evaluated over a window around the posterior
  # (vague priors span hundreds of orders of magnitude); the prior density is
  # estimated from all its draws, so its height in the window is not distorted
  dens <- do.call(rbind, lapply(long, function(x){
    x <- x[is.finite(x$value) & x$value > 0, ]
    lp <- log10(x$value[x$distribution == "posterior"])
    from <- min(lp) - 1
    to <- max(lp) + 1
    do.call(rbind, lapply(c("prior", "posterior"), function(w){
      dd <- stats::density(log10(x$value[x$distribution == w]), from = from, to = to, n = 256)
      data.frame(parameter = x$parameter[1], distribution = w, value = 10^dd$x, density = dd$y)
    }))
  }))
  dens$distribution <- factor(dens$distribution, levels = c("prior", "posterior"))
  ggplot(dens, aes(x = value, y = density, fill = distribution)) +
    geom_area(alpha = 0.4, position = "identity") +
    scale_x_log10() +
    facet_wrap(~ parameter, scales = "free") +
    scale_fill_manual(values = c(prior = "grey50", posterior = "#d95f02")) +
    theme_bw() +
    labs(x = "Variance (log10 scale)", y = "Density of log10(variance)", fill = NULL)
}
