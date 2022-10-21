#' Create a Longitudinal Variable Importance Object
#'
#' Create a longitudinal variable importance object from several constituent
#' cross-sectional variable importance objects.
#'
#' @param ... a comma-separated list of individual, cross-sectional variable importance objects. Assumed
#'   to be in order over time.
#'
#' @return an object of class \code{lvim}
lvim <- function(..., timepoints = numeric()) {
  validate_lvim(new_lvim(..., timepoints = timepoints))
}

new_lvim <- function(..., timepoints = numeric()) {
  vim_list <- list(...)
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
         "slope_vim" = NA, "slope_eif" = rep(NA, length(vim_list[[1]]$eif_full)),
         "slope_vim_se" = NA, "slope_vim_ci" = c(NA, NA), "slope_vim_p_value" = NA,
         "slope_full" = NA, "slope_eif_full" = rep(NA, length(vim_list[[1]]$eif_full)),
         "slope_reduced" = NA, "slope_eif_reduced" = rep(NA, length(vim_list[[1]]$eif_redu)),
         "slope_full_se" = NA, "slope_reduced_se" = NA,
         "slope_full_ci" = c(NA, NA), "slope_reduced_ci" = c(NA, NA),
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
