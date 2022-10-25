#' Format a \code{lvim} object
#'
#' @param x the \code{lvim} object of interest
#' @param digits the number of digits to format to
#' @param ... other options, see the generic \code{format} function
#' @export
format.lvim <- function(x, digits = 3, ...) {
  # create the output matrix
  output <- cbind(
    attr(x$vims[[1]], "type"), x$timepoints,
    do.call(rbind, lapply(x$vims, format, digits = digits, ...))
  )
  col_nms <- colnames(output)
  col_nms[1] <- "Timepoint"
  colnames(output) <- col_nms
  if (!is.na(x$average_vim)) {
    output <- rbind(output, cbind("Average", format(x$average_vim, digits = digits, ...),
                                  format(x$average_vim_se, digits = digits, ...),
                                  paste0("[", format(x$average_vim_ci[1], digits = digits, ...), ", ",
                                         format(x$average_vim_ci[2], digits = digits, ...), "]"),
                                     NA, format(x$average_p_value, digits = digits, ...)))
  }
  if (!any(is.na(x$trend_vim))) {
    trend_vim_mat <- cbind(c("Linear trend: intercept", "Linear trend: slope"),
                           format(x$trend_vim, digits = digits, ...), format(x$trend_vim_se, digits = digits, ...),
                           paste0("[", format(x$trend_vim_ci[1], digits = digits, ...), ", ",
                                  format(x$trend_vim_ci[2], digits = digits, ...), "]"),
                           NA, c(NA, format(x$trend_p_value, digits = digits, ...)))
    output <- rbind(output, trend_vim_mat)
  }
  if (!is.na(x$auc_vim)) {
    output <- rbind(output, cbind("AUC", format(x$auc_vim, digits = digits, ...),
                                  format(x$auc_vim_se, digits = digits, ...),
                                  paste0("[", format(x$auc_vim_ci, digits = digits, ...), ", ",
                                         format(x$auc_vim_ci[2], digits = digits, ...), "]"),
                                     NA, format(x$auc_p_value, digits = digits, ...)))
  }
  return(output)
}
