#' Create a Longitudinal Variable Importance Object
#'
#' Create a longitudinal variable importance object from several constituent
#' cross-sectional variable importance objects.
#'
#' @param vim_list a list of individual, cross-sectional variable importance objects. Assumed
#'   to be in order over time.
#' @param timepoints a numeric vector of timepoints of interest
#'
#' @return an object of class \code{lvim}
#' @export
lvim <- function(vim_list = list(), timepoints = numeric()) {
  validate_lvim(new_lvim(vim_list = vim_list, timepoints = timepoints))
}

new_lvim <- function(vim_list = list(), timepoints = numeric()) {
  vim_list_classes <- unlist(
    lapply(seq_len(length(vim_list)), function(i) !any(grepl("vim", class(vim_list[[i]]))))
  )
  if (any(vim_list_classes)) {
    stop("Please enter only variable importance objects (from, e.g., a call to vimp::cv_vim).")
  }
  # obtain predictiveness, EIFs, VIMs across timepoints
  pred_full <- unlist(lapply(vim_list, function(x) x$predictiveness_full))
  pred_redu <- unlist(lapply(vim_list, function(x) x$predictiveness_reduced))
  vims <- pred_full - pred_redu
  eif_full <- do.call(cbind, lapply(vim_list, function(x) x$eif_full))
  eif_redu <- do.call(cbind, lapply(vim_list, function(x) x$eif_redu))
  eifs <- do.call(cbind, lapply(vim_list, function(x) x$eif))
  structure(
    list("vims" = vim_list, "timepoints" = timepoints,
         "predictiveness_full" = pred_full, "predictiveness_reduced" = pred_redu,
         "eif_predictiveness_full" = eif_full, "eif_predictiveness_reduced" = eif_redu,
         "vim" = vims, "eif" = eifs,
         "average_vim" = NA, "average_eif" = rep(NA, length(vim_list[[1]]$eif_full)),
         "average_vim_se" = NA, "average_vim_ci" = c(NA, NA), "average_vim_p_value" = NA,
         "average_full" = NA, "average_eif_full" = rep(NA, length(vim_list[[1]]$eif_full)),
         "average_reduced" = NA, "average_eif_reduced" = rep(NA, length(vim_list[[1]]$eif_redu)),
         "average_full_se" = NA, "average_reduced_se" = NA,
         "average_full_ci" = c(NA, NA), "average_reduced_ci" = c(NA, NA),
         "trend_vim" = NA, "trend_eif" = rep(NA, length(vim_list[[1]]$eif_full)),
         "trend_vim_se" = NA, "trend_vim_ci" = c(NA, NA), "trend_vim_p_value" = NA,
         "trend_full" = NA, "trend_eif_full" = rep(NA, length(vim_list[[1]]$eif_full)),
         "trend_reduced" = NA, "trend_eif_reduced" = rep(NA, length(vim_list[[1]]$eif_redu)),
         "trend_full_se" = NA, "trend_reduced_se" = NA,
         "trend_full_ci" = c(NA, NA), "trend_reduced_ci" = c(NA, NA),
         "auc_vim" = NA, "auc_eif" = rep(NA, length(vim_list[[1]]$eif_full)),
         "auc_vim_se" = NA, "auc_vim_ci" = c(NA, NA), "auc_vim_p_value" = NA,
         "auc_full" = NA, "auc_eif_full" = rep(NA, length(vim_list[[1]]$eif_full)),
         "auc_reduced" = NA, "auc_eif_reduced" = rep(NA, length(vim_list[[1]]$eif_redu)),
         "auc_full_se" = NA, "auc_reduced_se" = NA,
         "auc_full_ci" = c(NA, NA), "auc_reduced_ci" = c(NA, NA)),
    class = c("lvim", "list")
  )
}

validate_lvim <- function(x) {
  input_list <- unclass(x)
  if (length(input_list$vims) != length(input_list$timepoints)) {
    stop("The number of entered vim objects must be the same as the number of timepoints.")
  }
  x
}

is.lvim <- function(x) {
  inherits(x, "lvim")
}
