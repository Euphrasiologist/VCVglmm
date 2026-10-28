# The data-scale formulas are checked against brute-force simulation of the
# latent model: l = mu + a + e, a ~ N(0, Va), e ~ N(0, V - Va).

mc_check <- function(family, mu, Va, V, h, rdist, n = 4e5, ngroups = 4000){
  set.seed(42)
  a <- rnorm(ngroups, 0, sqrt(Va))
  grp <- sample.int(ngroups, n, replace = TRUE)
  l <- mu + a[grp] + rnorm(n, 0, sqrt(V - Va))
  y <- rdist(l)
  # Var(E[y | a]) estimated from the exact conditional means
  cond <- vapply(a, function(ai){
    z <- VCVglmm:::.gh(64)
    sum(z$w * h(mu + ai + sqrt(V - Va) * z$x))
  }, numeric(1))
  list(ey = mean(y), vary = var(y), vcomp = var(cond))
}

test_that("Gauss-Hermite nodes integrate normal moments", {
  g <- VCVglmm:::.gh(32)
  expect_equal(sum(g$w), 1)
  expect_equal(sum(g$w * g$x^2), 1)
  expect_equal(sum(g$w * g$x^4), 3)
})

test_that("Poisson data-scale moments match simulation", {
  mu <- 0.5; Va <- 0.4; V <- 0.7
  mc <- mc_check("poisson", mu, Va, V, exp, function(l) rpois(length(l), exp(l)))
  expect_equal(VCVglmm:::.ey("poisson", mu, V), mc$ey, tolerance = 0.02)
  expect_equal(VCVglmm:::.vary("poisson", mu, V), mc$vary, tolerance = 0.03)
  expect_equal(VCVglmm:::.vcomp("poisson", mu, Va, V), mc$vcomp, tolerance = 0.05)
})

test_that("logit data-scale moments match simulation", {
  mu <- -0.3; Va <- 1.2; V <- 2.2
  mc <- mc_check("logit", mu, Va, V, plogis, function(l) rbinom(length(l), 1, plogis(l)))
  expect_equal(VCVglmm:::.ey("logit", mu, V), mc$ey, tolerance = 0.02)
  expect_equal(VCVglmm:::.vary("logit", mu, V), mc$vary, tolerance = 0.02)
  expect_equal(VCVglmm:::.vcomp("logit", mu, Va, V), mc$vcomp, tolerance = 0.05)
})

test_that("probit (ordinal) and threshold data-scale moments match simulation", {
  mu <- 0.4; Va <- 0.8; V <- 1.5
  # ordinal: extra N(0, 1) on top of V
  mc <- mc_check("probit", mu, Va, V, pnorm, function(l) rbinom(length(l), 1, pnorm(l)))
  expect_equal(VCVglmm:::.ey("probit", mu, V), mc$ey, tolerance = 0.02)
  expect_equal(VCVglmm:::.vcomp("probit", mu, Va, V), mc$vcomp, tolerance = 0.05)
  # threshold: y = 1 if l > 0, V includes the (units) residual
  set.seed(1)
  a <- rnorm(4000, 0, sqrt(Va))
  cond <- pnorm((mu + a) / sqrt(V - Va))
  expect_equal(VCVglmm:::.vcomp("threshold", mu, Va, V), var(cond), tolerance = 0.05)
  expect_equal(VCVglmm:::.ey("threshold", mu, V), pnorm(mu / sqrt(V)))
})

test_that("Gaussian scales are all the same", {
  expect_equal(VCVglmm:::.vcomp("gaussian", 1, 0.3, 1), 0.3)
  expect_equal(VCVglmm:::.vary("gaussian", 1, 1), 1)
})

test_that("ordinal/threshold category-score moments match simulation", {
  mu <- 0.4; Va <- 0.8; V <- 1.5; cp <- c(0, 0.9, 2.1)
  set.seed(7)
  ng <- 4000; n <- 4e5
  a <- rnorm(ng, 0, sqrt(Va))
  grp <- sample.int(ng, n, replace = TRUE)
  l <- mu + a[grp] + rnorm(n, 0, sqrt(V - Va))
  for(fam in c("probit", "threshold")){
    lat <- if(fam == "probit") l + rnorm(n) else l
    score <- rowSums(outer(lat, cp, `>`)) # 0, ..., K - 1
    expect_equal(VCVglmm:::.ey(fam, mu, V, cp), mean(score) + 1, tolerance = 0.01)
    expect_equal(VCVglmm:::.vary(fam, mu, V, cp = cp), var(score), tolerance = 0.02)
    expect_equal(VCVglmm:::.cat_probs(fam, mu, V, cp), as.vector(table(score)) / n, tolerance = 0.02)
    s <- sqrt(V - Va + (fam == "probit"))
    cond <- rowSums(pnorm(outer(mu + a, cp, `-`) / s))
    expect_equal(VCVglmm:::.vcomp(fam, mu, Va, V, cp), var(cond), tolerance = 0.05)
  }
  # with a single cutpoint at 0 this is the binary case
  expect_equal(VCVglmm:::.ey("probit", mu, V, 0), pnorm(mu / sqrt(V + 1)))
})

test_that("multinomial engine matches the exact logit maths with two categories", {
  Z <- VCVglmm:::.cat_Z(20000, 1)
  mu <- -0.3; Va <- 1.2; V <- 2.2
  vc <- VCVglmm:::.mn_vcomp(mu, matrix(Va), matrix(V), Z)
  expect_equal(vc[2], VCVglmm:::.vcomp("logit", mu, Va, V), tolerance = 0.03)
  expect_equal(vc[1], vc[2], tolerance = 1e-8) # the two indicators are complements
  pb <- VCVglmm:::.mn_pbar(mu, matrix(V), Z)
  expect_equal(pb[2], VCVglmm:::.ey("logit", mu, V), tolerance = 0.01)
})

test_that("multinomial engine matches brute-force nested simulation with three categories", {
  mu <- c(0.2, -0.5)
  Sa <- matrix(c(1, 0.4, 0.4, 0.8), 2)
  Stot <- Sa + matrix(c(1, 0.5, 0.5, 1), 2)
  vc <- VCVglmm:::.mn_vcomp(mu, Sa, Stot, VCVglmm:::.cat_Z(20000, 2))
  set.seed(9)
  La <- t(chol(Sa)); Lr <- t(chol(Stot - Sa))
  cond <- t(replicate(3000, {
    a <- as.vector(La %*% rnorm(2))
    e <- matrix(rnorm(4000), ncol = 2) %*% t(Lr)
    colMeans(VCVglmm:::.mn_p(sweep(e, 2, mu + a, "+")))
  }))
  expect_equal(vc, apply(cond, 2, var), tolerance = 0.06)
})

test_that("common random numbers don't disturb the user's RNG", {
  set.seed(1); a <- runif(1)
  set.seed(1); invisible(VCVglmm:::.cat_Z(10, 2)); b <- runif(1)
  expect_identical(a, b)
})
