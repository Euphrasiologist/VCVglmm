
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

Our toy data set will contain six parasites and six hosts, with 15 replicates of each parasite on each host. The response variable, Pterm, is simulated through normal deviations, with each parasite having different means, regardless of host (as we shall see...).

We can fit models to a very simple dataset about growth of parasites on host species.

``` r
head(dat.1); tail(dat.1)
#>    Parasite Hosts     Pterm
#> 1:       P1    H1  5.311791
#> 2:       P2    H1 12.751105
#> 3:       P3    H1 14.749581
#> 4:       P4    H1 18.345519
#> 5:       P5    H1 22.446900
#> 6:       P6    H1 30.604581
#>    Parasite Hosts    Pterm
#> 1:       P1    H6 10.52930
#> 2:       P2    H6 10.97539
#> 3:       P3    H6 16.13317
#> 4:       P4    H6 20.67335
#> 5:       P5    H6 24.37440
#> 6:       P6    H6 33.45515
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
#> REML criterion at convergence: 2271.6
#> 
#> Scaled residuals: 
#>      Min       1Q   Median       3Q      Max 
#> -3.01663 -0.69041 -0.03029  0.68404  3.04178 
#> 
#> Random effects:
#>  Groups   Name        Variance Std.Dev.
#>  Hosts    (Intercept)  0.02715 0.1648  
#>  Parasite (Intercept) 88.20953 9.3920  
#>  Residual              3.62831 1.9048  
#> Number of obs: 540, groups:  Hosts, 6; Parasite, 6
#> 
#> Fixed effects:
#>             Estimate Std. Error t value
#> (Intercept)   17.536      3.836   4.572
```

So this shows that almost all of the variance in our Pterm is being explained by Parasite, and the rest by the residual. But let's check that further...

``` r
library(VCVglmm)
VarExpl.mer(mod.1)
#>                  Var % Var Explained
#> Hosts     0.02714517      0.03076404
#> Parasite 88.20953197     99.96923596
```

And if we want that pesky p-value that lme4 doesn't give you in the `summary()`, don't worry.

``` r
calc_pvals(mod.1)
#>         Levels Estimate Std. Error  t value pval_upperdf pval_lowerdf
#> 1: (Intercept) 17.53568   3.835731 4.571665 3.002056e-06 0.0003208126
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
#>  DIC: 2239.834 
#> 
#>  G-structure:  ~Hosts
#> 
#>       post.mean  l-95% CI u-95% CI eff.samp
#> Hosts   0.01578 1.165e-16  0.04298     1000
#> 
#>                ~Parasite
#> 
#>          post.mean l-95% CI u-95% CI eff.samp
#> Parasite     142.4    26.32    374.9     1000
#> 
#>  R-structure:  ~units
#> 
#>       post.mean l-95% CI u-95% CI eff.samp
#> units     3.658    3.217    4.128     1000
#> 
#>  Location effects: Pterm ~ 1 
#> 
#>             post.mean l-95% CI u-95% CI eff.samp pMCMC   
#> (Intercept)    17.652    8.233   26.363     1000 0.004 **
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Looks sensible to me, we might want to add a prior for real data however. Below, using `Solapply()` we can extract the posterior modes of each of the levels of the random effects to see which levels are having the greatest impact.

``` r
Solapply(mod.2)
#>        Variable Grouped_Value    Group
#>  1:    Hosts.H1  4.791777e-04    Hosts
#>  2:    Hosts.H2 -1.112145e-03    Hosts
#>  3:    Hosts.H3  6.895307e-04    Hosts
#>  4:    Hosts.H4  1.095474e-03    Hosts
#>  5:    Hosts.H5  4.187432e-04    Hosts
#>  6:    Hosts.H6  4.121890e-04    Hosts
#>  7: Parasite.P1 -1.257399e+01 Parasite
#>  8: Parasite.P2 -8.049871e+00 Parasite
#>  9: Parasite.P3 -3.546308e+00 Parasite
#> 10: Parasite.P4  1.674735e+00 Parasite
#> 11: Parasite.P5  8.145081e+00 Parasite
#> 12: Parasite.P6  1.025416e+01 Parasite
```

Again we see that ~95%+ of the variability in our data is explained by Parasite, which was pretty much 100% in the lmer model.

``` r
MCMCRepnorm2(mod = mod.2)
#>          % Var explained
#> Hosts      -6.960018e-06
#> Parasite    9.674593e+01
#> units       3.254335e+00
#> $Hosts
#>             lower      upper
#> var1 4.648611e-17 0.04011356
#> attr(,"Probability")
#> [1] 0.95
#> 
#> $Parasite
#>         lower    upper
#> var1 91.39255 99.68865
#> attr(,"Probability")
#> [1] 0.95
#> 
#> $units
#>         lower    upper
#> var1 0.294305 8.492434
#> attr(,"Probability")
#> [1] 0.95
```

Improvements
------------

Please tell me where I've gone wrong and contribute if you can/want to!
