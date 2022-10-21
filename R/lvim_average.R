#' Average Longitudinal Variable Importance
#'
#' Compute a nonparametric estimate of (and efficient influence function for) the
#' average longitudinal variable importance over a contiguous subset of the time series.
#'
#' @param lvim an object of class \code{lvim} containing the cross-sectional variable importance objects
#' @param indices a numeric vector indicating the contiguous subset of the time series
#'
#' @return A named list of: (1) the estimated average variable importance; (2) the
#' estimated efficient influence function.
#' @export
lvim_average <- function(lvim, indices = 1:length(lvim)) {
  # estimate average predictiveness across the time points
  # estimate average predictiveness EIFs across time points
  # get EIFs
}
