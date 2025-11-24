#' Area Under the Variable Importance Trajectory
#'
#' Compute a nonparametric estimate of (and efficient influence function for) the
#' area under the longitudinal variable importance trajectory (AUTC) over a contiguous subset of the time series.
#'
#' @inheritParams lvim_average
#' @param interpolator a string indicating the type of interpolator used to
#'   take the area under the trajectory
#' @param ... other arguments to be passed to the interpolator function
#'
#' @importFrom stats splinefun
#' @importFrom stats integrate
#' @importFrom stats complete.cases
#' @importFrom stats pnorm
#'
#' @return The \code{lvim} object, with point estimates, CIs, and p-values
#'   related to the area under the trend in variable importance filled in.
#' @export
lvim_autc <- function(lvim, indices = 1:length(lvim), interpolator = "linear",
                     delta = 0, ...) {
  if (interpolator != "linear" & interpolator != "spline") {
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
      return(x[, range(indices, na.rm = TRUE)[1]] / 2 +
               x[, range(indices, na.rm = TRUE)[2]] / 2 +
               rowSums(x[, 2:(range(indices, na.rm = TRUE)[2] - 1), drop = FALSE], na.rm = TRUE))
    }
    # estimate AUTC of interpolated trajectory for predictiveness, VIM
    lvim$autc_full <- piecewise_linear_estimate(lvim$predictiveness_full)
    lvim$autc_reduced <- piecewise_linear_estimate(lvim$predictiveness_reduced)
    lvim$autc_vim <- lvim$autc_full - lvim$autc_reduced
    # estimate predictiveness, VIM EIFs
    lvim$autc_eif_full <- piecewise_linear_estimate(lvim$eif_predictiveness_full)
    lvim$autc_eif_reduced <- piecewise_linear_estimate(lvim$eif_predictiveness_reduced)
    lvim$autc_eif <- piecewise_linear_estimate(lvim$eif)
  } else if (interpolator == "spline") {
    # estimate autc of interpolated trajectory for predictiveness, VIM
    full_spline <- splinefun(x = indices, y = lvim$predictiveness_full, ...)
    reduced_spline <- splinefun(x = indices, y = lvim$predictiveness_reduced, ...)
    lvim$autc_full <- integrate(full_spline,
                               lower = range(indices)[1], upper = range(indices)[2])$value
    lvim$autc_reduced <- integrate(reduced_spline,
                                  lower = range(indices)[1], upper = range(indices)[2])$value
    lvim$autc_vim <- lvim$autc_full - lvim$autc_reduced
    # estimate predictiveness, VIM EIFs
    lvim$autc_eif_full <- rowSums(
      integrate(full_spline, deriv = 1,
                lower = range(indices)[1], upper = range(indices)[2])$value *
        lvim$eif_predictiveness_full
    )
    lvim$autc_eif_reduced <- rowSums(
      integrate(reduced_spline, deriv = 1,
                lower = range(indices)[1], upper = range(indices)[2])$value *
        lvim$eif_predictiveness_reduced
    )
    lvim$autc_eif <- rowSums(
      (integrate(full_spline, deriv = 1,
                 lower = range(indices)[1], upper = range(indices)[2])$value -
         integrate(reduced_spline, deriv = 1,
                   lower = range(indices)[1], upper = range(indices)[2])$value) *
        lvim$eif
    )
  }
  lvim$autc_full_se <- sqrt(mean(lvim$autc_eif_full ^ 2, na.rm = TRUE) / length(lvim$autc_eif_full[complete.cases(lvim$autc_eif_full)]))
  lvim$autc_reduced_se <- sqrt(mean(lvim$autc_eif_reduced ^ 2, na.rm = TRUE) / length(lvim$autc_eif_reduced[complete.cases(lvim$autc_eif_reduced)]))
  lvim$autc_vim_se <- sqrt(mean(lvim$autc_eif ^ 2, na.rm = TRUE) / length(lvim$autc_eif[complete.cases(lvim$autc_eif)]))
  # obtain CIs, hypothesis test of zero AUTC VIM
  lvim$autc_full_ci <- vimp::vimp_ci(est = lvim$autc_full, se = lvim$autc_full_se,
                                    scale = lvim$vims[[1]]$scale,
                                    level = 1 - lvim$vims[[1]]$alpha)
  lvim$autc_reduced_ci <- vimp::vimp_ci(est = lvim$autc_reduced, se = lvim$autc_reduced_se,
                                       scale = lvim$vims[[1]]$scale,
                                       level = 1 - lvim$vims[[1]]$alpha)
  lvim$autc_vim_ci <- vimp::vimp_ci(est = lvim$autc_vim, se = lvim$autc_vim_se,
                                   scale = lvim$vims[[1]]$scale,
                                   level = 1 - lvim$vims[[1]]$alpha)
  if (!is.na(lvim$vims[[1]]$p_value)) {
    lvim$autc_vim_p_value <- vimp::vimp_hypothesis_test(
      predictiveness_full = lvim$autc_full, predictiveness_reduced = lvim$autc_reduced,
      se = lvim$autc_vim_se, delta = 0, alpha = lvim$vims[[1]]$alpha
    )$p_value
    autc_full_test_statistic <- (lvim$autc_full - delta) / lvim$autc_full_se
    lvim$autc_full_p_value <- 2 * pnorm(abs(autc_full_test_statistic), lower.tail = FALSE)
    autc_reduced_test_statistic <- (lvim$autc_reduced - delta) / lvim$autc_reduced_se
    lvim$autc_reduced_p_value <- 2 * pnorm(abs(autc_reduced_test_statistic), lower.tail = FALSE)
  }
  return(lvim)
}
