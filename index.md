# `R/lvimp`: inference on longitudinal summaries of algorithm-agnostic variable importance \<img src=“man/figures/lvimp_logo.png” align=“right” width=“120px”/

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/lvimp)](https://cran.r-project.org/package=lvimp)
[![R-CMD-check](https://github.com/bdwilliamson/lvimp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bdwilliamson/lvimp/actions/workflows/R-CMD-check.yaml)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**Software author:** [Brian Williamson](https://bdwilliamson.github.io/)

**Methodology authors:** [Brian
Williamson](https://bdwilliamson.github.io/), [Erica
Moodie](https://www.mcgill.ca/epi-biostat-occh/erica-moodie), and [Susan
Shortreed](https://www.kpwashingtonresearch.org/our-research/our-scientists/shortreed-susan-m)

## Introduction

In prediction settings where data are collected over time, it is often
of interest to understand both the importance of variables for
predicting the response at each time point and the importance summarized
over the time series. Building on recent advances in estimation and
inference for variable importance measures (specifically, the
[vimp](https://github.com/bdwilliamson/vimp) package), we define
summaries of variable importance trajectories. These measures can be
estimated and the same approaches for inference can be applied
regardless of the choice of the algorithm(s) used to estimate the
prediction function. This package provides functions that, given fitted
values from prediction algorithms, compute algorithm-agnostic estimates
that summarize population variable importance over time.

More detail may be found in our
[paper](https://arxiv.org/pdf/2311.01638.pdf).

## Issues

If you encounter any bugs or have any specific feature requests, please
[file an issue](https://github.com/bdwilliamson/lvimp/issues).

## R installation

You may install a development release of `lvimp` from GitHub via
[`pak`](https://pak.r-lib.org) by running the following code:

``` r
pak::pkg_install(repo = "bdwilliamson/lvimp")
```

## Example

This example shows how to use `lvimp` in a simple setting with simulated
data.

``` r
# load required functions and packages
library("vimp")
library("SuperLearner")

# generate some data from a simple setting -------------------------------------
set.seed(4747)
p <- 2
n <- 5e4
T <- 3
timepoints <- seq_len(T) - 1
beta_01 <- rep(1, T)
beta_02 <- 1 + timepoints / 4
beta_0 <- lapply(as.list(seq_len(T)), function(t) {
  matrix(c(beta_01[t], beta_02[t]))
})
# generate 2 covariates
x <- lapply(as.list(1:T), function(t) as.data.frame(replicate(p, stats::rnorm(n, 0, 1))))
# apply the function to the x's
y <- lapply(as.list(1:T), function(t) as.matrix(x[[t]]) %*% beta_0[[t]] + rnorm(n, 0, 1))
# "true" outcome variance
true_var <- unlist(lapply(as.list(1:T), function(t) mean((y[[t]] - mean(y[[t]])) ^ 2)))
# note that true difference in R-squareds for variable j, under independence, is
# beta_j^2 * var(x_j) / var(y)
mse_one <- unlist(lapply(as.list(1:T), function(t) mean((y[[t]] - beta_01[t] * x[[t]][, 1]) ^ 2)))
mse_two <- unlist(lapply(as.list(1:T), function(t) mean((y[[t]] - beta_02[t] * x[[t]][, 2]) ^ 2)))
mse_full <- unlist(lapply(as.list(1:T), function(t) mean((y[[t]] - as.matrix(x[[t]]) %*% beta_0[[t]]) ^ 2)))
r2_one <- 1 - mse_one / true_var
r2_two <- 1 - mse_two / true_var
r2_full <- 1 - mse_full / true_var

# estimate predictiveness, variable importance at each timepoint ---------------
set.seed(1234)
# in this case, glm is correctly specified (so only use one learner to speed things up)
vim_list_1 <- lapply(as.list(1:T), function(t) {
  vimp::cv_vim(Y = y[[t]], X = x[[t]], indx = 1, V = 10, type = "r_squared",
               SL.library = c("SL.glm"))
})
set.seed(5678)
vim_list_2 <- lapply(as.list(1:T), function(t) {
  vimp::cv_vim(Y = y[[t]], X = x[[t]], indx = 2, V = 10, type = "r_squared",
               SL.library = c("SL.glm"))
})

# obtain the average, linear trend, and AUTC for the time series ---------------
lvim_obj <- lvim(vim_list_1, timepoints = 1:3)
est_average <- lvim_average(lvim_obj, indices = 1:3)
est_trend <- lvim_trend(lvim_obj, indices = 1:3)
est_autc <- lvim_autc(lvim_obj, indices = 1:3)
```
