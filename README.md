
Overview
--------

Functions that aid in exploration of models from the packages MCMCglmm and lme4.

Installation
------------

``` r
# Install this development version from GitHub:
# install.packages("devtools")
devtools::install_github("Euphrasiologist/VCVglmm")
```

Dependencies
------------

Please install them as follows:

``` r
install.packages("MCMCglmm"); install.packages("lme4")
library(MCMCglmm); library(lme4)

install.packages("data.table"); install.packages("tidyverse"); install.packages("ape"); install.packages("coda"); install.packages("lattice")
library(data.table); library(ggplot2); library(plyr); library(dplyr); library(reshape2); library(ape); library("coda"); library("lattice")
```

Simulation data and examples of how the functions can be used
-------------------------------------------------------------

We can fit models to a very simple dataset about growth of parasites on host species.

``` r
head(dat.1)
#>    Parasite Hosts     Pterm
#> 1:       P1    H1  4.038867
#> 2:       P2    H1 10.926203
#> 3:       P3    H1 13.660493
#> 4:       P4    H1 22.216541
#> 5:       P5    H1 23.202346
#> 6:       P6    H1 27.723256
```

Firstly we will do a model in lme4.

``` r
library(lme4)
#> Loading required package: Matrix

mod.1 <- lmer(Pterm ~ 1 + (1 | Hosts) + (1 | Parasite), data = dat.1)

summary(mod.1)
#> Linear mixed model fit by REML ['lmerMod']
#> Formula: Pterm ~ 1 + (1 | Hosts) + (1 | Parasite)
#>    Data: dat.1
#> 
#> REML criterion at convergence: 2327.4
#> 
#> Scaled residuals: 
#>      Min       1Q   Median       3Q      Max 
#> -2.47642 -0.74743  0.01831  0.68356  2.57984 
#> 
#> Random effects:
#>  Groups   Name        Variance Std.Dev.
#>  Hosts    (Intercept)  0.000   0.000   
#>  Parasite (Intercept) 87.327   9.345   
#>  Residual              4.048   2.012   
#> Number of obs: 540, groups:  Hosts, 6; Parasite, 6
#> 
#> Fixed effects:
#>             Estimate Std. Error t value
#> (Intercept)   17.490      3.816   4.583
```

So this shows that almost all of the variance in our Pterm is being explained by Parasite, and the rest by the residual. But let's check that further...

``` r
library(VCVglmm)
VarExpl.mer(mod.1)
#>               Var % Var Explained
#> Hosts     0.00000               0
#> Parasite 87.32686             100
```

And if we want that pesky p-value that lme4 doesn't give you in the `summary()`, don't worry.

``` r
calc_pvals(mod.1)
#>         Levels Estimate Std. Error  t value pval_upperdf pval_lowerdf
#> 1: (Intercept) 17.48953   3.816015 4.583191  2.84705e-06 0.0003144751
```

And if we quickly translate that to `MCMCglmm()` language...

``` r
library(MCMCglmm)
#> Loading required package: coda
#> Loading required package: ape

mod.2 <- MCMCglmm(Pterm ~ 1, 
                  random = ~ Hosts + Parasite,
                  data = dat.1,
                  pr = TRUE,
                  verbose = FALSE)
#> Warning: 'cBind' is deprecated.
#>  Since R version 3.2.0, base's cbind() should work fine with S4 objects

summary(mod.2)
#> 
#>  Iterations = 3001:12991
#>  Thinning interval  = 10
#>  Sample size  = 1000 
#> 
#>  DIC: 2295.57 
#> 
#>  G-structure:  ~Hosts
#> 
#>       post.mean  l-95% CI u-95% CI eff.samp
#> Hosts   0.00261 2.262e-17 0.008433    308.1
#> 
#>                ~Parasite
#> 
#>          post.mean l-95% CI u-95% CI eff.samp
#> Parasite     145.8    21.52    401.5     1000
#> 
#>  R-structure:  ~units
#> 
#>       post.mean l-95% CI u-95% CI eff.samp
#> units     4.067    3.609    4.574     1000
#> 
#>  Location effects: Pterm ~ 1 
#> 
#>             post.mean l-95% CI u-95% CI eff.samp pMCMC  
#> (Intercept)    17.720    8.851   28.601     1000  0.01 *
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Looks sensible to me, we might want to add a prior for real data however. Below, using `Solapply()` we can extract the posterior modes of each of the levels of the random effects to see which levels are having the greatest impact.

``` r
Solapply(mod.2)
#>        Variable Grouped_Value    Group
#>  1:    Hosts.H1 -1.089116e-04    Hosts
#>  2:    Hosts.H2  8.583315e-04    Hosts
#>  3:    Hosts.H3  8.712162e-04    Hosts
#>  4:    Hosts.H4  5.509145e-04    Hosts
#>  5:    Hosts.H5 -4.153542e-04    Hosts
#>  6:    Hosts.H6 -2.764527e-04    Hosts
#>  7: Parasite.P1 -1.275671e+01 Parasite
#>  8: Parasite.P2 -6.856658e+00 Parasite
#>  9: Parasite.P3 -3.075557e+00 Parasite
#> 10: Parasite.P4  2.274394e+00 Parasite
#> 11: Parasite.P5  7.623495e+00 Parasite
#> 12: Parasite.P6  1.251503e+01 Parasite
```

Again we see that 97% of the variability in our data is explained by Parasite.

``` r
MCMCRepnorm2(mod = mod.2)
#>          % Var explained
#> Hosts      -2.293452e-07
#> Parasite    9.632118e+01
#> units       3.678818e+00
#> $Hosts
#>            lower       upper
#> var1 8.95015e-18 0.006214869
#> attr(,"Probability")
#> [1] 0.95
#> 
#> $Parasite
#>        lower    upper
#> var1 90.1945 99.74526
#> attr(,"Probability")
#> [1] 0.95
#> 
#> $units
#>         lower    upper
#> var1 0.254738 9.798841
#> attr(,"Probability")
#> [1] 0.95
```

Improvements
------------

Please tell me where I've gone wrong and contribute if you can/want to!
