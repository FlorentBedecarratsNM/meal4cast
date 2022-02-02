#' model_fetch_xgboost UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_model_fetch_xgboost_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' model_fetch_xgboost Server Functions
#'
#' @noRd 
mod_model_fetch_xgboost_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Loading content from prevision repo if not present
    if (!file.exists("main.py")) {
      fetch_xgb_model()
    }
  })
}
    
## To be copied in the UI
# mod_model_fetch_xgboost_ui("model_fetch_xgboost_ui_1")
    
## To be copied in the server
# mod_model_fetch_xgboost_server("model_fetch_xgboost_ui_1")
