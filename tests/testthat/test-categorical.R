suppressMessages(library(MCMCglmm))

set.seed(4)
n <- 400
d <- data.frame(g = factor(rep(1:40, each = 10)), x = rnorm(n))
u <- matrix(rnorm(80, 0, 0.8), 40)
# log-odds of categories b, c, d against a
L <- cbind(0, 0.3 + 0.8 * d$x + u[d$g, 1], -0.2 + u[d$g, 2], 0.1 - 0.5 * d$x)
P <- exp(L) / rowSums(exp(L))
d$yc <- factor(apply(P, 1, function(p) sample(c("a", "b", "c", "d"), 1, prob = p)))
d$y2 <- rnorm(n)
IJ <- (1/4) * (diag(3) + matrix(1, 3, 3))
mc <- MCMCglmm(yc ~ trait - 1 + trait:x, random = ~ us(trait):g, rcov = ~ us(trait):units,
               family = "categorical", data = d, verbose = FALSE, nitt = 13000, burnin = 3000, thin = 20,
               prior = list(R = list(V = IJ, fix = 1), G = list(G1 = list(V = diag(3), nu = 3))))

test_that("categorical responses are grouped and paired correctly", {
  g <- VCVglmm:::.MCMC_cat_groups(mc)
  expect_length(g, 1)
  expect_equal(g[[1]]$categories, c("reference", "b", "c", "d"))
  ci <- VCVglmm:::.cat_info(mc, g[[1]])
  # the random-effect covariance matrix is the us(trait):g matrix
  expect_equal(unname(ci$comps[["us(trait):g"]][, 2, 3]), unname(as.vector(mc$VCV[, "traityc.c:traityc.d.g"])))
  # the fitted model reproduces the observed category frequencies
  pr <- rowMeans(sapply(seq_len(ci$S), function(s){
    St <- Reduce(`+`, lapply(ci$comps, function(A) A[s, , ])) + ci$Vf[s, , ]
    VCVglmm:::.mn_pbar(ci$mu[s, ], St, VCVglmm:::.cat_Z(5000, 3))
  }))
  expect_equal(unname(pr), as.vector(table(d$yc)) / n, tolerance = 0.1)
})

test_that("varpart, R2 and explain give category-level data-scale results", {
  expect_message(vp <- glmm_varpart(mc, nmc = 2000), "data scale per category")
  dat <- vp[vp$scale == "data", ]
  expect_setequal(unique(dat$trait), paste0("yc: ", c("reference", "b", "c", "d")))
  expect_true(all(dat$mean >= 0 & dat$mean <= 1))
  expect_setequal(unique(vp$trait[vp$scale == "latent"]), c("yc.b", "yc.c", "yc.d"))
  r2 <- suppressMessages(glmm_R2(mc, nmc = 2000))
  expect_true(all(r2$mean[r2$scale == "data" & r2$component == "R2c"] >=
                  r2$mean[r2$scale == "data" & r2$component == "R2m"]))
  out <- capture.output(e <- glmm_explain(mc, nmc = 2000))
  expect_true(any(grepl("Categorical response: yc", out)))
  expect_true(any(grepl("changes the category probabilities", out)))
  expect_true(any(grepl("conditional odds ratio", out)))
  # x raises b and lowers d relative to a in the simulation
  expect_s3_class(e$varpart, "glmm_varpart")
})

test_that("categorical and Gaussian responses can be mixed", {
  m2 <- MCMCglmm(cbind(yc, y2) ~ trait - 1, random = ~ us(trait):g, rcov = ~ us(trait):units,
                 family = c("categorical", "gaussian"), data = d, verbose = FALSE, nitt = 3000, burnin = 1000,
                 prior = list(R = list(V = diag(4), nu = 4, fix = 1), G = list(G1 = list(V = diag(4), nu = 4))))
  g <- VCVglmm:::.MCMC_cat_groups(m2)
  expect_equal(g[[1]]$traits, 1:3)
  vp <- suppressMessages(glmm_varpart(m2, nmc = 1000))
  expect_true("y2" %in% vp$trait)
  expect_true("yc: reference" %in% vp$trait)
})

test_that("binary family = 'categorical' models are treated as logit", {
  set.seed(4)
  d <- data.frame(g = factor(rep(1:30, each = 10)))
  d$y <- rbinom(300, 1, plogis(rnorm(30)[d$g]))
  m <- MCMCglmm(y ~ 1, random = ~ g, family = "categorical", data = d, verbose = FALSE,
                prior = list(R = list(V = 1, fix = 1), G = list(G1 = list(V = 1, nu = 0.002))),
                nitt = 3000, burnin = 1000)
  expect_equal(VCVglmm:::.MCMC_family(m), "logit")
  vp <- glmm_varpart(m)
  expect_setequal(unique(vp$scale), c("latent", "link", "data"))
  expect_true(all(c("Non-additive", "Distribution-specific") %in% vp$component[vp$scale == "data"]))
})
