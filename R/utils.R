# -------------------------------------
# release questions
# -------------------------------------
# @keywords internal
release_questions <- function() {
  c(
    "Have you run rhub::rhub_check() with 1,4,5?",
    "Have you run devtools::check_win_devel() and devtools::check_win_release()?"
  )
}
