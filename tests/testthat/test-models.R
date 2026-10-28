suppressMessages({library(MCMCglmm); library(lme4)})

set.seed(10)
d <- data.frame(g = factor(rep(1:25, each = 12)), x = rnorm(300, 1, 1.5),
                f = factor(sample(letters[1:3], 300, TRUE)))
d$x2 <- rnorm(300)
d$y <- 1 + 0.5 * d$x + rnorm(25)[d$g] + rnorm(25, 0, 0.3)[d$g] * d$x + rnorm(300)

test_that("lme4 random-slope variance is averaged over the covariate", {
  m <- lmer(y ~ x + (1 + x | g), data = d)
  info <- VCVglmm:::.glmm_info(m, nsim = 10)
  S <- as.matrix(VarCorr(m)$g)
  by.hand <- S[1, 1] + 2 * S[1, 2] * mean(d$x) + S[2, 2] * mean(d$x^2)
  expect_equal(info$comps[[1]][1], by.hand)
  expect_equal(info$comps$Residual[1], sigma(m)^2)
})

test_that("MCMCglmm us() random-slope variance is averaged over the covariate", {
  m <- MCMCglmm(y ~ x, random = ~ us(1 + x):g, data = d, verbose = FALSE,
                nitt = 3000, burnin = 1000, thin = 10,
                prior = list(R = list(V = 1, nu = 0.002), G = list(G1 = list(V = diag(2), nu = 2))))
  info <- VCVglmm:::.glmm_info(m)
  V <- m$VCV
  by.hand <- V[, 1] + 2 * V[, 2] * mean(d$x) + V[, 4] * mean(d$x^2)
  expect_equal(unname(info$comps[[1]]), unname(as.vector(by.hand)))
  expect_equal(unname(info$comps$units), unname(as.vector(V[, "units"])))
  # the fixed effect mean and variance match the explicit linear predictor
  xb <- as.matrix(m$X %*% t(m$Sol[, 1:2]))
  expect_equal(unname(info$mu), unname(colMeans(xb)))
  expect_equal(unname(info$Vf), unname(apply(xb, 2, var)))
})

test_that("Gaussian varpart agrees with MCMCRepnorm2 and idh terms are combined", {
  m <- MCMCglmm(y ~ x, random = ~ g + idh(f):g, data = d, verbose = FALSE,
                nitt = 3000, burnin = 1000, thin = 10,
                prior = list(R = list(V = 1, nu = 0.002),
                             G = list(G1 = list(V = 1, nu = 0.002), G2 = list(V = diag(3), nu = 0.002))))
  vp <- glmm_varpart(m, scale = "latent")
  expect_setequal(vp$component, c("g", "idh(f):g", "units"))
  info <- VCVglmm:::.glmm_info(m)
  w <- as.vector(table(d$f)) / nrow(d)
  expect_equal(unname(info$comps[["idh(f):g"]]), unname(as.vector(m$VCV[, 2:4] %*% w)))

  m1 <- MCMCglmm(y ~ x, random = ~ g, data = d, verbose = FALSE, nitt = 3000, burnin = 1000, thin = 10)
  vp1 <- glmm_varpart(m1)
  icc <- m1$VCV[, "g"] / rowSums(m1$VCV)
  expect_equal(vp1$mean[vp1$scale == "latent" & vp1$component == "g"], mean(icc))
  expect_equal(vp1$mean[vp1$scale == "data"], vp1$mean[vp1$scale == "latent"])
})

test_that("Wald.test.auto attributes coefficients to the right terms", {
  m <- MCMCglmm(y ~ x + x2 + f + x:f, data = d, verbose = FALSE, nitt = 3000, burnin = 1000, thin = 10)
  w <- Wald.test.auto(m)
  expect_equal(colnames(w), c("x", "x2", "f", "x:f"))
  expect_equal(unname(unlist(w["df", ])), c(1, 1, 2, 2))
})

test_that("calc_pvals gives two-sided p-values", {
  m <- lmer(y ~ x + (1 | g), data = d)
  p <- calc_pvals(m)
  t <- summary(m)$coefficients[, "t value"]
  expect_equal(p$pval_upperdf, unname(2 * pt(-abs(t), nobs(m))))
})

test_that("VarExpl.mer includes the residual variance", {
  m <- lmer(y ~ x + (1 | g), data = d)
  v <- capture.output(r <- VarExpl.mer(m))
  expect_equal(sum(r[, 2]), 100)
  expect_true("Residual" %in% rownames(r))
})

test_that("diagnostics and explain run on several families", {
  d$yb <- rbinom(300, 1, plogis(0.3 * d$x + rnorm(25)[d$g]))
  m <- MCMCglmm(yb ~ x, random = ~ g, family = "categorical", data = d, verbose = FALSE,
                nitt = 3000, burnin = 1000, thin = 10,
                prior = list(R = list(V = 1, fix = 1), G = list(G1 = list(V = 1, nu = 0.002))))
  dg <- MCMCdiag(m)
  expect_true(dg$fixed.par[dg$parameter == "units"])
  expect_output(glmm_explain(m), "Variance components")
  gm <- glmer(yb ~ x + (1 | g), family = binomial, data = d)
  expect_output(glmm_explain(gm), "odds ratio")
  expect_message(vp <- glmm_varpart(gm), "degenerate")
  expect_false("latent" %in% vp$scale)
  expect_s3_class(merdiagplot(gm)$ranef, "ggplot")
  expect_s3_class(MCMCtrace(m), "ggplot")
})

test_that("Bbar genus-level off-diagonal mean is computed per genus", {
  skip_if_not_installed("ape")
  set.seed(3)
  tree <- ape::rtree(10, rooted = TRUE, tip.label = c(paste("G1", 1:5), paste("G10", 1:5)))
  v <- ape::vcv(tree)
  gen <- sub(" .*", "", colnames(v))
  by.hand <- mean(sapply(unique(gen), function(gg){
    x <- v[gen == gg, gen == gg]; n <- nrow(x)
    2 * mean(diag(x)) + 2 * (sum(x) - sum(diag(x))) / (n^2 - n)
  }))
  expect_equal(Bbar(tree, "Genus"), by.hand)
})

test_that("MCMCdiag prints column subsets and explains disagreeing stationarity tests", {
  m <- MCMCglmm(y ~ x, random = ~ g, data = d, verbose = FALSE, nitt = 3000, burnin = 1000, thin = 2)
  expect_output(print(MCMCdiag(m)[, c("parameter", "ESS")]), "ESS")
  # Geweke fails everywhere, H-W passes everywhere
  local_mocked_bindings(geweke.diag = function(x, ...) list(z = c(var1 = -7.6)),
                        heidel.diag = function(x, ...) matrix(1, 1, 1, dimnames = list(NULL, "stest")),
                        .package = "coda")
  iss <- attr(MCMCdiag(m), "issues")
  expect_true(any(grepl("Geweke fails but Heidelberger-Welch passes", iss)))
})
