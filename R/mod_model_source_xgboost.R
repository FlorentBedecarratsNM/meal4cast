#' model_source_xgboost UI Function
#'
#' @description This shiny modules launches the python/reticulate parameters.
#' Server side only.
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
    # Load python environment
    # reticulate::use_python("/opt/venv/bin/python3", required = TRUE)
    reticulate::use_virtualenv("/opt/venv", required = TRUE)
    # reticulate::use_virtualenv("venv_shiny_app", required = TRUE)
    reticulate::source_python("main.py")
  })

}
    
## To be copied in the UI
# mod_model_source_xgboost_ui("model_source_xgboost_ui_1")
    
## To be copied in the server
# mod_model_source_xgboost_server("model_source_xgboost_ui_1")
