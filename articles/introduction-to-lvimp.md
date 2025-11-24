# Introduction to \`lvimp\`

## Introduction

`lvimp` is a package that computes nonparametric estimates of summaries
of a nonparametric variable importance trajectory over time, and
provides inference on the true summaries of the variable importance
trajectory. The package depends heavily on the [`vimp`
package](https://github.com/bdwilliamson/vimp) for estimating and doing
inference on the cross-sectional variable importance at each timepoint
in the trajectory.

## Installation

A development version of the package may be downloaded and installed
from GitHub using the `remotes` package:

``` r
pak::pkg_install("bdwilliamson/lvimp")
```

## Quick Start

This section should serve as a quick guide to using the `lvimp` package.
We will cover the three main functions for estimating summaries of the
longitudinal variable importance trajectory using simulated data.

First, load the `lvimp` package:

``` r
library("lvimp")
```

Next, create some longitudinal data:

``` r
set.seed(4747)
p <- 2
n <- 5e4
T <- 3
timepoints <- seq_len(T) - 1
indices <- timepoints + 1
beta_01 <- rep(1, T)
beta_02 <- 1 + timepoints / 4
beta_0 <- lapply(as.list(seq_len(T)), function(t) {
  matrix(c(beta_01[t], beta_02[t]))
})
# generate 2 covariates
x <- lapply(as.list(1:T), function(t) as.data.frame(replicate(p, stats::rnorm(n, 0, 1))))
# apply the function to the x's
y <- lapply(as.list(1:T), function(t) as.matrix(x[[t]]) %*% beta_0[[t]] + rnorm(n, 0, 1))
```

In this scenario, there are three timepoints at which data are
collected. The above code block creates a list `x` containing 3
matrices, each with 2 columns and `n` rows; and a list `y` containing
three vectors of length `n`. Here, `x` contains the covariates of
interest and `y` contains the outcomes of interest.

Next, we use the `vimp` package to estimate the importance of variable 1
relative to variable 2 for predicting $Y$ at each timepoint:

``` r
library("vimp")
#> vimp version 2.3.6: Perform Inference on Algorithm-Agnostic Variable Importance
library("SuperLearner")
#> Loading required package: nnls
#> Loading required package: gam
#> Loading required package: splines
#> Loading required package: foreach
#> Loaded gam 1.22-6
#> Super Learner
#> Version: 2.0-29
#> Package created on 2024-02-06
set.seed(1234)
# in this case, glm is correctly specified (so only use one learner to speed things up)
vim_list_1 <- lapply(as.list(1:T), function(t) {
  vimp::cv_vim(Y = y[[t]], X = x[[t]], indx = 1, V = 10, type = "r_squared",
               SL.library = c("SL.glm"))
})
```

Finally, there are three available summaries in `lvimp`: \* The average
variable importance over a contiguous subset of the time series
(`lvim_average`) \* The linear trend in variable importance over a
contiguous subset of the time series (`lvim_trend`) \* The area under
the variable importance trajectory curve over a contiguous subset of the
time series (`lvim_autc`)

We now estimate and do inference on these three summary measures:

``` r
# set up an lvim object
lvim_obj <- lvim(vim_list_1, timepoints = 1:3)
# obtain the average
est_lvim <- lvim_average(lvim_obj, indices = 1:3)
# add on the linear trend
est_lvim <- lvim_trend(est_lvim, indices = 1:3)
# add on the AUTC based on a piecewise linear trajectory
est_lvim <- lvim_autc(est_lvim, indices = 1:3)
# inspect the estimates
est_lvim
#> Variable importance estimates:
#>       Timepoint               Estimate SE      95% CI             VIMP > 0
#> s = 1 1                       0.345    0.00598 [0.334, 0.357]     TRUE    
#> s = 1 2                       0.284    0.00561 [0.273, 0.295]     TRUE    
#> s = 1 3                       0.238    0.00507 [0.228, 0.248]     TRUE    
#>       Average                 0.289    0.00321 [0.283, 0.295]     <NA>    
#>       Linear trend: intercept  0.3961  0.00886 [ 0.3788,  0.4135] <NA>    
#>       Linear trend: slope     -0.0536  0.00392 [-0.0612, -0.0459] <NA>    
#>       AUTC                    0.575    0.00685 [0.562, 0.589]     <NA>    
#>       p-value 
#> s = 1 0       
#> s = 1 0       
#> s = 1 0       
#>       0       
#>       <NA>    
#>       1.39e-42
#>       0
```
