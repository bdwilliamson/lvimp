#' Linear Trend in the Longitudinal Variable Importance Trajectory
#'
#' Compute a nonparametric estimate of (and efficient influence function for) the
#' linear trend in the longitudinal variable importance over a contiguous subset of the time series.
#'
#' @inheritParams lvim_average
#'
#' @return The \code{lvim} object, with point estimates, CIs, and p-values
#'   related to the linear trend in variable importance filled in.
#' @export
lvim_trend <- function(lvim, indices = 1:length(lvim), delta = 0) {
  U <- cbind(1, matrix(indices))
  beta_matrix <- solve(t(U) %*% U) %*% t(U)
  # estimate linear trend in predictiveness, VIM across the time points
  lvim$trend_full <- beta_matrix %*% matrix(lvim$predictiveness_full)
  lvim$trend_reduced <- beta_matrix %*% matrix(lvim$predictiveness_reduced)
  lvim$trend_vim <- lvim$trend_full - lvim$trend_reduced
  # estimate trend predictiveness, VIM EIFs across time points
  lvim$trend_eif_full <- beta_matrix %*% t(lvim$eif_predictiveness_full)
  lvim$trend_eif_reduced <- beta_matrix %*% t(lvim$eif_predictiveness_reduced)
  lvim$trend_eif <- beta_matrix %*% t(lvim$eif)
  lvim$trend_full_se <- sqrt(rowMeans(lvim$trend_eif_full ^ 2, na.rm = TRUE) / sum(complete.cases(t(lvim$trend_eif_full))))
  lvim$trend_reduced_se <- sqrt(rowMeans(lvim$trend_eif_reduced ^ 2, na.rm = TRUE) / sum(complete.cases(t(lvim$trend_eif_reduced))))
  lvim$trend_vim_se <- sqrt(rowMeans(lvim$trend_eif ^ 2, na.rm = TRUE) / sum(complete.cases(t(lvim$trend_eif))))
  # obtain CIs, hypothesis test of zero trend variable importance
  lvim$trend_full_ci <- do.call(rbind, lapply(as.list(seq_len(length(lvim$trend_full))), function(i) {
    vimp::vimp_ci(est = lvim$trend_full[i], se = lvim$trend_full_se[i],
                  scale = lvim$vims[[1]]$scale, level = 1 - lvim$vims[[1]]$alpha,
                  truncate = FALSE)
  }))
  lvim$trend_reduced_ci <- do.call(rbind, lapply(as.list(seq_len(length(lvim$trend_reduced))), function(i) {
    vimp::vimp_ci(est = lvim$trend_reduced[i], se = lvim$trend_reduced_se[i],
                  scale = lvim$vims[[1]]$scale, level = 1 - lvim$vims[[1]]$alpha,
                  truncate = FALSE)
  }))
  lvim$trend_vim_ci <- do.call(rbind, lapply(as.list(seq_len(length(lvim$trend_vim))), function(i) {
    vimp::vimp_ci(est = lvim$trend_vim[i], se = lvim$trend_vim_se[i],
                  scale = lvim$vims[[1]]$scale, level = 1 - lvim$vims[[1]]$alpha,
                  truncate = FALSE)
  }))
  if (!is.na(lvim$vims[[1]]$p_value)) {
    trend_test_statistic <- (lvim$trend_full[2] - lvim$trend_reduced[2] - delta) / lvim$trend_vim_se[2]
    lvim$trend_vim_p_value <- 2 * pnorm(abs(trend_test_statistic), lower.tail = FALSE)
    trend_full_test_statistic <- (lvim$trend_full[2] - delta) / lvim$trend_full_se[2]
    lvim$trend_full_p_value <- 2 * pnorm(abs(trend_full_test_statistic), lower.tail = FALSE)
    trend_reduced_test_statistic <- (lvim$trend_reduced[2] - delta) / lvim$trend_reduced_se[2]
    lvim$trend_reduced_p_value <- 2 * pnorm(abs(trend_reduced_test_statistic), lower.tail = FALSE)
  }
  return(lvim)
}
