#' Format a \code{lvim} object
#'
#' @param x the \code{lvim} object of interest
#' @param ... other options, see the generic \code{format} function
#' @export
format.lvim <- function(x, ...) {
  # create the output matrix
  output <- cbind(attr(x$vims[[1]], "type"), x$timepoints, do.call(rbind, lapply(x$vims, format)))
  col_nms <- colnames(output)
  col_nms[1] <- "Timepoint"
  colnames(output) <- col_nms
  if (!is.na(x$average_vim)) {
    output <- rbind(output, matrix(c("Average", x$average_vim, x$average_vim_ci, NA, x$average_p_value)))
  }
  if (!any(is.na(x$trend_vim))) {
    trend_vim_mat <- cbind(c("Linear trend: intercept", "Linear trend: slope"),
                           x$trend_vim, x$trend_ci, NA, c(NA, x$trend_p_value))
    output <- rbind(output, trend_vim_mat)
  }
  if (!is.na(x$auc_vim)) {
    output <- rbind(output, matrix(c("AUC", x$auc_vim, x$auc_vim_ci, NA, x$auc_p_value)))
  }
  return(output)
}
