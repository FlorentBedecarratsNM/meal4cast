#' create_folder 
#'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAWElEQVR42mNgGPTAxsZmJsVqQApgmGw1yApwKcQiT7phRBuCzzCSDSHGMKINIeDNmWQlA2IigKJwIssQkHdINgxfmBBtGDEBS3KCxBc7pMQgMYE5c/AXPwAwSX4lV3pTWwAAAABJRU5ErkJggg==
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
