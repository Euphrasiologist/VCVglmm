
## Overview

Functions that aid in exploration of models from the packages MCMCglmm
and lme4.

## Installation

``` r
# Install this development version from GitHub:
# install.packages("devtools")
devtools::install_github("Euphrasiologist/VCVglmm")
```

## Dependencies

Please install them as follows:

``` r
install.packages("MCMCglmm"); install.packages("lme4")
library(MCMCglmm); library(lme4)

install.packages("data.table"); install.packages("tidyverse"); install.packages("ape"); install.packages("coda"); install.packages("lattice")
library(data.table); library(ggplot2); library(plyr); library(dplyr); library(reshape2); library(ape); library("coda"); library("lattice")
```

## Simulation data and examples of how the functions can be used

Our toy data set will contain six parasites and six hosts, with 15
replicates of each parasite on each host. The response variable, Pterm,
is simulated through normal deviations, with each parasite having
different means, regardless of host (as we shall see…).

We can fit models to a very simple dataset about growth of parasites on
host species.

``` r
head(dat.1); tail(dat.1)
#>    Parasite Hosts     Pterm
#> 1:       P1    H1  3.641533
#> 2:       P2    H1 12.280153
#> 3:       P3    H1 11.834130
#> 4:       P4    H1 19.699902
#> 5:       P5    H1 23.683628
#> 6:       P6    H1 27.577708
#>    Parasite Hosts     Pterm
#> 1:       P1    H6  7.947823
#> 2:       P2    H6  8.176867
#> 3:       P3    H6 15.271369
#> 4:       P4    H6 20.314061
#> 5:       P5    H6 20.519166
#> 6:       P6    H6 28.016953
```

Firstly we will do a model in lme4.

``` r
library(lme4)
#> Loading required package: Matrix

mod.1 <- lmer(Pterm ~ 1 + (1 | Hosts) + (1 | Parasite), data = dat.1)
#> boundary (singular) fit: see ?isSingular

summary(mod.1)
#> Linear mixed model fit by REML ['lmerMod']
#> Formula: Pterm ~ 1 + (1 | Hosts) + (1 | Parasite)
#>    Data: dat.1
#> 
#> REML criterion at convergence: 2380.9
#> 
#> Scaled residuals: 
#>      Min       1Q   Median       3Q      Max 
#> -2.78200 -0.70653  0.02668  0.66659  2.71914 
#> 
#> Random effects:
#>  Groups   Name        Variance Std.Dev.
#>  Hosts    (Intercept)  0.000   0.000   
#>  Parasite (Intercept) 88.197   9.391   
#>  Residual              4.474   2.115   
#> Number of obs: 540, groups:  Hosts, 6; Parasite, 6
#> 
#> Fixed effects:
#>             Estimate Std. Error t value
#> (Intercept)   17.520      3.835   4.568
#> convergence code: 0
#> boundary (singular) fit: see ?isSingular
```

So this shows that almost all of the variance in our Pterm is being
explained by Parasite, and the rest by the residual. But let’s check
that further…

``` r
library(VCVglmm)
VarExpl.mer(mod.1)
#>               Var % Var Explained
#> Hosts     0.00000               0
#> Parasite 88.19652             100
```

And if we want that pesky p-value that lme4 doesn’t give you in the
`summary()`, don’t worry.

``` r
calc_pvals(mod.1)
#>         Levels Estimate Std. Error  t value pval_upperdf pval_lowerdf
#> 1: (Intercept) 17.51988   3.835063 4.568343 3.048217e-06 0.0003226639
```

And if we quickly translate that to `MCMCglmm()` language…

``` r
library(MCMCglmm)
#> Loading required package: coda
#> Loading required package: ape

mod.2 <- MCMCglmm(Pterm ~ 1, 
                  random = ~ Hosts + Parasite,
                  data = dat.1,
                  pr = TRUE,
                  verbose = FALSE)

summary(mod.2)
#> 
#>  Iterations = 3001:12991
#>  Thinning interval  = 10
#>  Sample size  = 1000 
#> 
#>  DIC: 2349.483 
#> 
#>  G-structure:  ~Hosts
#> 
#>       post.mean  l-95% CI  u-95% CI eff.samp
#> Hosts  0.001058 3.097e-17 0.0006547    151.2
#> 
#>                ~Parasite
#> 
#>          post.mean l-95% CI u-95% CI eff.samp
#> Parasite     144.7    16.05    388.7     1000
#> 
#>  R-structure:  ~units
#> 
#>       post.mean l-95% CI u-95% CI eff.samp
#> units     4.509    4.062    5.087     1000
#> 
#>  Location effects: Pterm ~ 1 
#> 
#>             post.mean l-95% CI u-95% CI eff.samp pMCMC  
#> (Intercept)    17.360    6.128   26.497     1000 0.012 *
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Looks sensible to me, we might want to add a prior for real data
however. Below, using `Solapply()` we can extract the posterior modes of
each of the levels of the random effects to see which levels are having
the greatest impact.

``` r
Solapply(mod.2)
#>        Variable Grouped_Value    Group
#>  1:    Hosts.H1 -6.041321e-04    Hosts
#>  2:    Hosts.H2 -5.608347e-07    Hosts
#>  3:    Hosts.H3  5.569833e-04    Hosts
#>  4:    Hosts.H4 -3.068158e-04    Hosts
#>  5:    Hosts.H5  3.354911e-04    Hosts
#>  6:    Hosts.H6 -7.292986e-04    Hosts
#>  7: Parasite.P1 -1.172435e+01 Parasite
#>  8: Parasite.P2 -8.089398e+00 Parasite
#>  9: Parasite.P3 -1.141637e+00 Parasite
#> 10: Parasite.P4  3.126249e+00 Parasite
#> 11: Parasite.P5  8.828054e+00 Parasite
#> 12: Parasite.P6  1.366192e+01 Parasite
```

Again we see that \~95%+ of the variability in our data is explained by
Parasite, which was pretty much 100% in the lmer model.

``` r
MCMCRepnorm2(mod = mod.2)
#>          % Var explained
#> Hosts      -5.659259e-10
#> Parasite    9.798530e+01
#> units       2.014334e+00
#> $Hosts
#>            lower        upper
#> var1 2.80567e-18 0.0004912091
#> attr(,"Probability")
#> [1] 0.95
#> 
#> $Parasite
#>         lower    upper
#> var1 89.11348 99.43389
#> attr(,"Probability")
#> [1] 0.95
#> 
#> $units
#>          lower    upper
#> var1 0.5661093 10.88652
#> attr(,"Probability")
#> [1] 0.95
```

## Improvements

Please tell me where I’ve gone wrong and contribute if you can/want to\!

## Citations

  - Common et al., 2020 Diversity in CRISPR‐based immunity protects
    susceptible genotypes by restricting phage spread and evolution,
    doi: <https://doi.org/10.1111/jeb.13638>
