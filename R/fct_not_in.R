#' not_in 
#'
#' @description A function to check that mapping include all occurrences 
#' and display a meaningful message
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
not_in <- function(x, y, index = index) {
  # extract function argument
  my_x <- deparse(substitute(x))
  my_y <- deparse(substitute(y))
  # check that arguments literals have the right format
  if (!(stringr::str_detect(my_x, "(.*[:alpha:]|_)*\\$.*$") & 
        stringr::str_detect(my_y, "([:alpha:]|_)*\\$.*$"))) {
    print(my_x)
    print(my_y)
    print("argument for {not in} must look like 'dataframe$column'")
    stop()
  }
  # parse argument to provide helpful messages
  # left part of the argument
  x_ds <- my_x %>%
    stringr::str_remove("dt\\(\\)\\$") %>%
    stringr::str_extract("([:alpha:]|_)*\\$") %>%
    stringr::str_remove("\\$")
  y_ds <- my_y %>%
    stringr::str_remove("dt\\(\\)\\$") %>%
    stringr::str_extract("([:alpha:]|_)*\\$") %>%
    stringr::str_remove("\\$")
  # right part of the function argument
  x_col <- stringr::str_extract(my_x, "([:alpha:]|_)*$")
  y_col <- stringr::str_extract(my_y, "([:alpha:]|_)*$")
  # Extract the missmatches
  x <- unique(x)
  missings <- x[!(x %in% y)]
  n_miss <- length(missings)
  
  # Prepare message element
  if (n_miss > 0) {
    out <- paste0(n_miss, " établissement(s) mentionné(s) dans le champ ",
                  x_col, " du fichier ", my_x,
                  " mais pas dans le champ ", y_col,
                  " du fichier ", my_y, " : ",
                  paste(missings, collapse = ", "), ".")
  }
}