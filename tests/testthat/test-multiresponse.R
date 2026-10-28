suppressMessages(library(MCMCglmm))

set.seed(2)
n <- 300
d <- data.frame(g = factor(rep(1:30, each = 10)), x = rnorm(n))
u1 <- rnorm(30)[d$g]
u2 <- 0.5 * u1 + rnorm(30, 0, 0.5)[d$g]
d$y1 <- 1 + 0.5 * d$x + u1 + rnorm(n)
d$y2 <- rpois(n, exp(0.2 + 0.3 * d$x + u2))
d$y1[c(3, 50)] <- NA
mv <- MCMCglmm(cbind(y1, y2) ~ trait - 1 + trait:x, random = ~ us(trait):g, rcov = ~ us(trait):units,
               family = c("gaussian", "poisson"), data = d, verbose = FALSE, nitt = 4000, burnin = 1000,
               prior = list(R = list(V = diag(2), nu = 2), G = list(G1 = list(V = diag(2), nu = 2))))

test_that("multi-response models are split into traits", {
  infos <- VCVglmm:::.glmm_infos(mv)
  expect_equal(vapply(infos, `[[`, character(1), "trait"), c("y1", "y2"))
  expect_equal(vapply(infos, `[[`, character(1), "family"), c("gaussian", "poisson"))
  # each trait's variances are the diagonal elements belonging to it
  expect_equal(unname(infos[[1]]$comps[[1]]), unname(as.vector(mv$VCV[, "traity1:traity1.g"])))
  expect_equal(unname(infos[[2]]$comps[[1]]), unname(as.vector(mv$VCV[, "traity2:traity2.g"])))
  expect_equal(unname(infos[[2]]$comps[[2]]), unname(as.vector(mv$VCV[, "traity2:traity2.units"])))
  # and its fixed effects only
  expect_equal(colnames(infos[[2]]$beta), c("traity2", "traity2:x"))
  xb <- as.matrix(mv$Sol[, c("traity2", "traity2:x")]) %*% rbind(1, d$x)
  expect_equal(unname(infos[[2]]$mu), unname(rowMeans(xb)))
})

test_that("varpart, R2 and explain work per trait", {
  vp <- glmm_varpart(mv)
  expect_setequal(unique(vp$trait), c("y1", "y2"))
  expect_true(all(c("latent", "link", "data") %in% vp$scale[vp$trait == "y2"]))
  vp2 <- glmm_varpart(mv, trait = "y2", scale = "latent")
  expect_equal(unique(vp2$trait), "y2")
  r2 <- glmm_R2(mv)
  expect_equal(nrow(r2), 2 * 3 * 2) # traits x scales x (R2m, R2c)
  expect_s3_class(plot(vp), "ggplot")
  out <- capture.output(e <- glmm_explain(mv))
  expect_true(any(grepl("Trait: y1", out)))
  expect_true(any(grepl("Correlations", out)))
  expect_true(any(grepl("^  x \\(traity2:x\\)", out)))
  expect_equal(sort(unique(e$fixed$trait)), c("y1", "y2"))
})

test_that("glmm_cor matches the covariance matrix", {
  cr <- glmm_cor(mv)
  expect_equal(nrow(cr), 2)
  r <- mv$VCV[, "traity1:traity2.g"] / sqrt(mv$VCV[, "traity1:traity1.g"] * mv$VCV[, "traity2:traity2.g"])
  expect_equal(cr$mean[cr$level == "random"], mean(r))
  expect_equal(cr$effect1[1], "traity1")
  expect_equal(cr$effect2[1], "traity2")
})

test_that("ordinal and threshold fits reproduce the observed category frequencies", {
  l <- 0.3 + 0.8 * d$x + u1 + rnorm(n)
  d$yo <- factor(1 + (l > 0) + (l > 1) + (l > 2))
  pr <- list(R = list(V = 1, fix = 1), G = list(G1 = list(V = 1, nu = 1, alpha.mu = 0, alpha.V = 1000)))
  for(fam in c("threshold", "ordinal")){
    m <- MCMCglmm(yo ~ x, random = ~ g, family = fam, data = d, prior = pr, verbose = FALSE,
                  nitt = 13000, burnin = 3000)
    info <- VCVglmm:::.glmm_info(m)
    expect_equal(ncol(info$cp), 3)
    V <- info$Vf + rowSums(do.call(cbind, info$comps))
    pr.cat <- rowMeans(sapply(seq_len(info$S), function(s)
      VCVglmm:::.cat_probs(info$family, info$mu[s], V[s], info$cp[s, ])))
    expect_equal(unname(pr.cat), as.vector(table(d$yo)) / n, tolerance = 0.1)
    vp <- glmm_varpart(m)
    expect_true(all(c("latent", "link", "data") %in% vp$scale))
    expect_output(glmm_explain(m), "expected category")
  }
})

