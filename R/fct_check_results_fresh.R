#' check_results_fresh 
#'
#' @description A function to retrieve results' timestamps
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
check_results_fresh <- function(folder = "output", pattern = "results_by_cafeteria.*csv") {
  file.info(dir(folder, pattern, full.names = TRUE))$ctime
}