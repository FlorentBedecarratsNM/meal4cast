#' load_traindata 
#'
#' @description A function to load the input data. Defaults to the index 
#' specified above.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

load_traindata <- function(name = index$name, path = index$path) {
  dt <- purrr::map(path, ~ arrow::read_csv_arrow(.)) %>%
    purrr::set_names(name)
}