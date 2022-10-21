#' Create a Longitudinal Variable Importance Object
#'
#' Create a longitudinal variable importance object from several constituent
#' cross-sectional variable importance objects.
#'
#' @param ... a comma-separated list of individual, cross-sectional variable importance objects. Assumed
#'   to be in order over time.
#'
#' @return an object of class \code{lvim}
lvim <- function(...) {
  validate_lvim(new_lvim(...))
}

new_lvim <- function(...) {
  vim_list <- list(...)
  vim_list_classes <- unlist(
    lapply(seq_len(length(vim_list)), function(i) grepl("vim", class(vim_list[[i]])))
  )
  if (any(vim_list_classes)) {
    stop("Please enter only variable importance objects (from, e.g., a call to vimp::cv_vim).")
  }
  structure(
    c(vim_list, list(average = NA, average_eif = rep(NA, ))),
    class = c("lvim", "list")
  )
}

validate_lvim <- function(x) {
  input_list <- unclass(x)

}

is.lvim <- function(x) {
  inherits(x, "lvim")
}
