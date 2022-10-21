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
  return(output)
}
