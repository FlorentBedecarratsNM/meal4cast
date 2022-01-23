#' model_source_xgboost UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_model_source_xgboost_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' model_source_xgboost Server Functions
#'
#' @noRd 
mod_model_source_xgboost_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Loading content from prevision repo if not present
    if (!file.exists("main.py")) {
      # Change these parameters to adjust for repository or branch changes
      repo <- "school_meal_forecast_xgboost"
      branch <- "dev"
      # What follows remain as is
      destfile <- paste0(branch, ".zip")
      to_keep <- c("app/", "tests/")
      unz_folder <- paste0(repo, "-", branch)
      unz_files <- paste0(unz_folder, "/", to_keep)
      download.file(url = paste0("https://github.com/nantesmetropole/",
                                 repo, "/archive/refs/heads/", destfile),
                                 destfile = destfile)
      unzip(zipfile = destfile)
      purrr::map2(unz_files, to_keep, arrow::copy_files) 
      file.rename(from = paste0(unz_folder, "/main.py"), to = "main.py")
      unlink(unz_folder, recursive = TRUE)
      file.remove(destfile)
    }
    
    # Load python environment
    reticulate::use_python("/opt/venv/bin/python3", required = TRUE)
    reticulate::use_virtualenv("/opt/venv/bin/venv", required = TRUE)
    reticulate::source_python("main.py")
 
  })

}
    
## To be copied in the UI
# mod_model_source_xgboost_ui("model_source_xgboost_ui_1")
    
## To be copied in the server
# mod_model_source_xgboost_server("model_source_xgboost_ui_1")
