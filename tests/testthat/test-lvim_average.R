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

# test averages ----------------------------------------------------------------
test_that("Averaging VIMs across the time series works", {
  lvim_obj <- lvim(vim_list_1, timepoints = 1:3)
  est <- lvim_average(lvim_obj, indices = 1:3)
  expect_equal(est$average_full, mean(r2_full), tolerance = 0.1)
  expect_equal(est$average_reduced, mean(r2_two), tolerance = 0.1)
  expect_equal(est$average_vim, mean(r2_full - r2_two), tolerance = 0.1)
})
