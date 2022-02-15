#' load_results 
#'
#' @description A function to load the outputs of the model forecasts.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
load_results <- function(folder = "output", pattern = "results_by_cafeteria.*csv") {
  prev_results <- dir(folder, pattern = pattern, full.names = TRUE) %>%
    dplyr::tibble(filename = .)
  if (nrow(prev_results) > 0 ) {
    prev_results <- prev_results %>%
      dplyr::mutate(created = stringr::str_extract(filename, 
                                                   "[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}-[0-9]{2}"),
                    variable = stringr::str_extract(filename, 
                                                    "(?<=cafeteria_)[a-z]*"),
                    training_type = stringr::str_extract(filename, 
                                                         "xgb_interval|xgb"),
                    file_contents = purrr::map(filename, ~ arrow::read_csv_arrow(.))) %>%
      tidyr::unnest(cols = c(file_contents)) %>%
      dplyr::arrange(dplyr::desc(created), dplyr::desc(training_type)) %>%
      dplyr::distinct(date_str, variable, cantine_nom, cantine_type, .keep_all = TRUE)
  } else {
    prev_results <- NA
  }
  return(prev_results)
}