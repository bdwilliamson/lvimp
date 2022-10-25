#' Area Under the Variable Importance Trajectory
#'
#' Compute a nonparametric estimate of (and efficient influence function for) the
#' area under the longitudinal variable importance trajectory over a contiguous subset of the time series.
#'
#' @inheritParams lvim_average
#' @param interpolator a string indicating the type of interpolator used to
#'   take the area under the trajectory
#' @param ... other arguments to be passed to the interpolator function
#'
#' @return The \code{lvim} object, with point estimates, CIs, and p-values
#'   related to the area under the trend in variable importance filled in.
#' @export
lvim_auc <- function(lvim, indices = 1:length(lvim), interpolator = "linear",
                     delta = 0, ...) {
  if (interpolator != "linear" | interpolator != "spline") {
    stop("Currently, the only available interpolators are piecewise linear (enter 'linear') and cubic splines (enter 'spline').")
  }
  if (interpolator == "linear") {
    # this is a special case where we can write down the exact form of the EIF, even though
    # the piecewise linear interpolator is non-differentiable
    piecewise_linear_estimate <- function(x) {
      if (!is.matrix(x)) {
        indices <- seq_len(length(x))
        x <- matrix(x, nrow = 1)
      }
      indices <- seq_len(ncol(x))
      return(x[, range(indices)[1]] / 2 + x[, range(indices)[2]] / 2 + sum(x[, 2:(range(indices)[2] - 1)]))
    }
    # estimate AUC of interpolated trajectory for predictiveness, VIM
    lvim$auc_full <- piecewise_linear_estimate(lvim$predictiveness_full)
    lvim$auc_reduced <- piecewise_linear_estimate(lvim$predictiveness_reduced)
    lvim$auc_vim <- lvim$auc_full - lvim$auc_reduced
    # estimate predictiveness, VIM EIFs
    lvim$auc_eif_full <- piecewise_linear_estimate(lvim$eif_predictiveness_full)
    lvim$auc_eif_reduced <- piecewise_linear_estimate(lvim$eif_predictiveness_reduced)
    lvim$auc_eif <- piecewise_linear_estimate(lvim$eif)
    lvim$auc_full_se <- sqrt(mean(lvim$auc_eif_full ^ 2) / length(lvim$auc_eif_full))
    lvim$auc_reduced_se <- sqrt(mean(lvim$auc_eif_reduced ^ 2) / length(lvim$auc_eif_reduced))
    lvim$auc_vim_se <- sqrt(mean(lvim$auc_eif ^ 2) / length(lvim$auc_eif))
  } else if (interpolator == "spline") {
    # estimate AUC of interpolated trajectory for predictiveness, VIM
    full_spline <- splinefun(x = indices, y = lvim$predictiveness_full, ...)
    reduced_spline <- splinefun(x = indices, y = lvim$predictiveness_reduced, ...)
    lvim$auc_full <- integrate(full_spline,
                               lower = range(indices)[1], upper = range(indices)[2])$value
    lvim$auc_reduced <- integrate(reduced_spline,
                                  lower = range(indices)[1], upper = range(indices)[2])$value
    lvim$auc_vim <- lvim$auc_full - lvim$auc_reduced
    # estimate predictiveness, VIM EIFs
    integrand_full <- function(x, eifs) {

    }
    lvim$auc_eif_full <- integrate(full_spline, deriv = 1,
                                   lower = range(indices)[1], upper = range(indices)[2])
    # lvim$auc_full_eif <-
  }
  # obtain CIs, hypothesis test of zero AUC VIM
  lvim$auc_full_ci <- vimp::vimp_ci(est = lvim$auc_full, se = lvim$auc_full_se,
                                    scale = lvim$vims[[1]]$scale,
                                    level = 1 - lvim$vims[[1]]$alpha)
  lvim$auc_reduced_ci <- vimp::vimp_ci(est = lvim$auc_reduced, se = lvim$auc_reduced_se,
                                       scale = lvim$vims[[1]]$scale,
                                       level = 1 - lvim$vims[[1]]$alpha)
  lvim$auc_vim_ci <- vimp::vimp_ci(est = lvim$auc_vim, se = lvim$auc_vim_se,
                                   scale = lvim$vims[[1]]$scale,
                                   level = 1 - lvim$vims[[1]]$alpha)
  if (!is.na(lvim$vims[[1]]$p_value)) {
    lvim$auc_vim_p_value <- vimp::vimp_hypothesis_test(
      predictiveness_full = lvim$auc_full, predictiveness_reduced = lvim$auc_reduced,
      se = lvim$auc_vim_se, delta = 0, alpha = lvim$vims[[1]]$alpha
    )
  }
}
