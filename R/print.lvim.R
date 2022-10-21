#' Print a \code{lvim} object
#'
#' @param x the \code{lvim} object of interest
#' @param ... other options, see the generic \code{print} function
#' @export
print.lvim <- function(x, ...) {
  cat("Variable importance estimates:\n")
  print(format(x, ...), quote = FALSE)
  invisible(x)
}
