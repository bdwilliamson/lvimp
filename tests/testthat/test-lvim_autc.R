# load required functions and packages
library("testthat")
library("vimp")
library("SuperLearner")

# generate some data from a simple setting -------------------------------------
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
# true AUTC of trajectory based on piecewise linear interpolator
piecewise_linear_estimate <- function(x) {
  if (!is.matrix(x)) {
    indices <- seq_len(length(x))
    x <- matrix(x, nrow = 1)
  }
  indices <- seq_len(ncol(x))
  return(x[, range(indices)[1]] / 2 +
           x[, range(indices)[2]] / 2 +
           rowSums(x[, 2:(range(indices)[2] - 1), drop = FALSE]))
}
autc_full_linear <- piecewise_linear_estimate(r2_full)
autc_reduced_linear_1 <- piecewise_linear_estimate(r2_two)
partial_autc_full_linear <- piecewise_linear_estimate(r2_full[1:2])
partial_autc_reduced_linear_1 <- piecewise_linear_estimate(r2_two[1:2])
# true autc of trajectory based on spline interpolator
spline_full <- splinefun(x = indices, y = r2_full)
spline_reduced_1 <- splinefun(x = indices, y = r2_two)
autc_full_spline <- integrate(spline_full, lower = range(indices)[1], upper = range(indices)[2])$value
autc_reduced_spline_1 <- integrate(spline_reduced_1, lower = range(indices)[1], upper = range(indices)[2])$value

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

# test AUTC of piecewise linear interpolator ------------------------------------
test_that("AUTC of piecewise linear interpolator of VIMs across the time series works", {
  lvim_obj <- lvim(vim_list_1, timepoints = 1:3)
  est <- lvim_autc(lvim_obj, indices = 1:3)
  expect_equal(est$autc_full, autc_full_linear, tolerance = 0.1)
  expect_equal(est$autc_reduced, autc_reduced_linear_1, tolerance = 0.1)
  expect_equal(est$autc_vim, autc_full_linear - autc_reduced_linear_1, tolerance = 0.1)
  expect_true(est$autc_vim_se < 1)
})

# test AUTC of spline interpolator ----------------------------------------------
test_that("AUTC of spline interpolator of VIMs across the time series works", {
  lvim_obj <- lvim(vim_list_1, timepoints = 1:3)
  est <- lvim_autc(lvim_obj, indices = 1:3, interpolator = "spline")
  expect_equal(est$autc_full, autc_full_spline, tolerance = 0.1)
  expect_equal(est$autc_reduced, autc_reduced_spline_1, tolerance = 0.1)
  expect_equal(est$autc_vim, autc_full_spline - autc_reduced_spline_1, tolerance = 0.1)
  expect_true(est$autc_vim_se < 1)
})

# test AUTC of piecewise linear interpolator over a subset ----------------------
test_that("AUTC of piecewise linear interpolator of VIMs across the time series works", {
  lvim_obj <- lvim(vim_list_1[1:2], timepoints = 1:2)
  est <- lvim_autc(lvim_obj, indices = 1:2)
  expect_equal(est$autc_full, partial_autc_full_linear, tolerance = 0.1)
  expect_equal(est$autc_reduced, partial_autc_reduced_linear_1, tolerance = 0.1)
  expect_equal(est$autc_vim, partial_autc_full_linear - partial_autc_reduced_linear_1, tolerance = 0.1)
  est$autc_vim_se < 1
})
