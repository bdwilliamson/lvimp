#' Average Longitudinal Variable Importance
#'
#' Compute a nonparametric estimate of (and efficient influence function for) the
#' average longitudinal variable importance over a contiguous subset of the time series.
#'
#' @param lvim an object of class \code{lvim} containing the cross-sectional
#'   variable importance objects
#' @param indices a numeric vector indicating the contiguous subset of the
#'   time series
#' @param delta null hypothesis value
#'
#' @return The \code{lvim} object, with point estimates, CIs, and p-values
#'   related to the average variable importance filled in.
#' @export
lvim_average <- function(lvim, indices = 1:length(lvim), delta = 0) {
  # estimate average predictiveness, VIM across the time points
  lvim$average_full <- mean(lvim$predictiveness_full)
  lvim$average_reduced <- mean(lvim$predictiveness_reduced)
  lvim$average_vim <- lvim$average_full - lvim$average_reduced
  # estimate average predictiveness, VIM EIFs across time points
  lvim$average_eif_full <- rowMeans(lvim$eif_predictiveness_full)
  lvim$average_eif_reduced <- rowMeans(lvim$eif_predictiveness_reduced)
  lvim$average_eif <- rowMeans(lvim$eif)
  lvim$average_full_se <- sqrt(mean(lvim$average_eif_full ^ 2) / length(lvim$average_eif_full))
  lvim$average_reduced_se <- sqrt(mean(lvim$average_eif_reduced ^ 2) / length(lvim$average_eif_reduced))
  lvim$average_vim_se <- sqrt(mean(lvim$average_eif ^ 2) / length(lvim$average_eif))
  # obtain CIs, hypothesis test of zero average variable importance
  lvim$average_full_ci <- vimp::vimp_ci(est = lvim$average_full, se = lvim$average_full_se,
                                        scale = lvim$vims[[1]]$scale, level = 1 - lvim$vims[[1]]$alpha)
  lvim$average_reduced_ci <- vimp::vimp_ci(est = lvim$average_reduced, se = lvim$average_reduced_se,
                                        scale = lvim$vims[[1]]$scale, level = 1 - lvim$vims[[1]]$alpha)
  lvim$average_vim_ci <- vimp::vimp_ci(est = lvim$average_vim, se = lvim$average_vim_se,
                                       scale = lvim$vims[[1]]$scale, level = 1 - lvim$vims[[1]]$alpha)
  if (!is.na(lvim$vims[[1]]$p_value)) {
    lvim$average_p_value <- vimp::vimp_hypothesis_test(
      predictiveness_full = lvim$average_full, predictiveness_reduced = lvim$average_reduced,
      se = lvim$average_vim_se, delta = delta, alpha = lvim$vims[[1]]$alpha

    )$p_value
  }
  return(lvim)
}
