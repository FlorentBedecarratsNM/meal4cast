#' check_traindata_fresh 
#'
#' @description A function to retrieve training data time stamps.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

check_traindata_fresh <- function(path = index$path) {
  file.info(path)$ctime
}