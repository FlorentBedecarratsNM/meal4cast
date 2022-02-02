#' create_folder 
#'
#' @description This function checks if a temp folder exists and creates it 
#' otherwise.
#'
#' @return Does not return any value, only creates the folder if missing.
#'
#' @noRd
create_folder <- function(folder) {
  # Creating a temp folder if needed to handle downloads
  if (!(dir.exists(folder))) {
    dir.create(folder)
  }
}