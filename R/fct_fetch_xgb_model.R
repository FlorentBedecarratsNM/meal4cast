#' fetch_xgb_model 
#'
#' @description A function that retrieves the python XGBoost model from the
#' Github repository and copies it to the project filesystem. 
#' @param gh_account String. The name of the github account to read from.
#' @param gh_repo String. The name of the github repository to read from.
#' @param gh_branch String. The name of the github branch to read from.
#' @param to_keep Character vector. The repository folder to fetch.
#' @param keep_main Logical. Whether the file main.py should be fetched.
#' @return Returns TRUE if the operation completes.
#' 
#' @noRd

fetch_xgb_model <- function(gh_account = "nantesmetropole",
                            gh_repo = "school_meal_forecast_xgboost",
                            gh_branch = "dev",
                            to_keep = c("app/", "tests/"),
                            main = TRUE) {
  # construct paths from parameters
  destfile <- paste0(gh_branch, ".zip")
  gh_url <- paste("https://github.com", gh_account, gh_repo, 
                   "archive/refs/heads", destfile, sep = "/")
  unz_folder <- paste0(gh_repo, "-", gh_branch)
  unz_files <- paste0(unz_folder, "/", to_keep)
  # fetch
  download.file(url = gh_url, destfile = destfile)
  unzip(zipfile = destfile)
  # copy
  purrr::map2(unz_files, to_keep, arrow::copy_files) 
  if (main) {
    file.rename(from = paste0(unz_folder, "/main.py"), to = "main.py")
  }
  # clean
  unlink(unz_folder, recursive = TRUE)
  file.remove(destfile)
}
