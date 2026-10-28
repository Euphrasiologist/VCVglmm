#' Correlations between traits or random effects
#'
#' Extracts every correlation implied by the covariance structures of a model, with posterior
#' modes, means and HPD intervals for MCMCglmm models. In a multi-response model with
#' \code{random = ~ us(trait):g} and \code{rcov = ~ us(trait):units} these are the among-group
#' and residual (within-group) correlations between the traits. In a random-slope model
#' (\code{us(1 + x):g}, or \code{(1 + x | g)} in lme4) it is the intercept-slope correlation.
#'
#' Correlations are on the latent scale. Covariances fixed in the prior (e.g. the residual
#' structure of a categorical model) are reported as constants.
#'
#' @param mod model of class MCMCglmm, lmerMod or glmerMod
#' @param prob probability mass of the HPD interval
#' @return data.frame of class \code{glmm_cor}: the term, the two effects/traits correlated, the
#' posterior mode, mean and HPD interval (MCMCglmm) or the estimate (lme4), and for MCMCglmm the
#' posterior probability that the correlation is positive. The draws are in \code{attr(x, "draws")}.
#' @keywords MCMCglmm, lme4, correlation, multi-response
#' @seealso \code{\link{glmm_varpart}}, \code{\link{glmm_explain}}
#' @export
#' @examples
#' \donttest{
#' library(MCMCglmm)
#' set.seed(1)
#' d <- data.frame(g = factor(rep(1:30, each = 10)))
#' u1 <- rnorm(30)
#' u2 <- 0.6 * u1 + rnorm(30, 0, 0.8) # among-group correlation of 0.6
#' d$y1 <- u1[d$g] + rnorm(300)
#' d$y2 <- u2[d$g] + rnorm(300)
#' prior <- list(R = list(V = diag(2), nu = 1.002), G = list(G1 = list(V = diag(2), nu = 1.002)))
#' mod <- MCMCglmm(cbind(y1, y2) ~ trait - 1, random = ~ us(trait):g, rcov = ~ us(trait):units,
#'                 family = c("gaussian", "gaussian"), data = d, prior = prior, verbose = FALSE)
#' glmm_cor(mod)
#' }

glmm_cor <- function(mod, prob = 0.95){
  if(inherits(mod, "MCMCglmm")){
    blocks <- Filter(function(b) b$k > 1, .MCMC_blocks(mod))
    VCV <- as.matrix(mod$VCV)
    vn <- colnames(VCV)
    rows <- list()
    draws <- list()
    for(b in blocks){
      k <- b$k
      eff <- vapply(b$diag.cols, function(i) .diag_effect(vn[i])$effect, character(1))
      grp <- .diag_effect(vn[b$diag.cols[1]])$group
      for(j in 1:(k - 1)) for(l in (j + 1):k){
        cov <- VCV[, b$cols[(l - 1) * k + j]]
        r <- cov / sqrt(VCV[, b$cols[(j - 1) * k + j]] * VCV[, b$cols[(l - 1) * k + l]])
        nm <- sprintf("%s ~ %s (%s)", eff[j], eff[l], grp)
        draws[[nm]] <- r
        rows[[nm]] <- data.frame(term = b$term, level = if(b$residual) "residual" else "random",
                                 effect1 = eff[j], effect2 = eff[l],
                                 P.positive = mean(r > 0), stringsAsFactors = FALSE)
      }
    }
    if(length(rows) == 0){
      message("No covariance structures (us()/cor() terms) in this model")
      return(invisible(NULL))
    }
    s <- .post_summary(do.call(cbind, draws), prob)
    res <- cbind(do.call(rbind, rows)[, 1:4], s, P.positive = vapply(rows, `[[`, numeric(1), "P.positive"))
    bayes <- TRUE
  } else if(inherits(mod, "merMod")){
    vc <- as.data.frame(lme4::VarCorr(mod))
    vc <- vc[!is.na(vc$var2), ]
    if(nrow(vc) == 0){
      message("No correlated random effects in this model")
      return(invisible(NULL))
    }
    res <- data.frame(term = vc$grp, level = "random", effect1 = vc$var1, effect2 = vc$var2,
                      mode = vc$sdcor, mean = vc$sdcor, lower = NA_real_, upper = NA_real_,
                      stringsAsFactors = FALSE)
    draws <- NULL
    bayes <- FALSE
  } else {
    stop("Model must be of class MCMCglmm or merMod (lme4)")
  }
  rownames(res) <- NULL
  class(res) <- c("glmm_cor", "data.frame")
  attr(res, "draws") <- draws
  attr(res, "prob") <- prob
  attr(res, "bayes") <- bayes
  res
}

# split a VCV diagonal name "<effect>:<effect>.<group>" into its parts
.diag_effect <- function(name){
  pos <- gregexpr(":", name, fixed = TRUE)[[1]]
  for(i in pos[pos > 0]){
    p <- substr(name, 1, i - 1)
    pre <- paste0(p, ":", p, ".")
    if(startsWith(name, pre)) return(list(effect = p, group = substring(name, nchar(pre) + 1)))
  }
  list(effect = name, group = "")
}

#' @export
print.glmm_cor <- function(x, digits = 2, ...){
  cat("Correlations (latent scale)\n")
  if(!isTRUE(attr(x, "bayes"))){
    cat("Note: lme4 estimates; use confint(mod, method = \"profile\") for intervals.\n")
  }
  y <- as.data.frame(unclass(x))
  num <- intersect(c("mode", "mean", "lower", "upper", "P.positive"), names(y))
  y[num] <- lapply(y[num], round, digits = digits)
  if(!isTRUE(attr(x, "bayes"))) y <- y[, setdiff(names(y), c("mode", "lower", "upper"))]
  print(y, row.names = FALSE)
  invisible(x)
}
