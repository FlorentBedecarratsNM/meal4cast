#' ui_03_run_model UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ui_03_run_model_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' ui_03_run_model Server Functions
#'
#' @noRd 
mod_ui_03_run_model_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_ui_03_run_model_ui("ui_03_run_model_ui_1")
    
## To be copied in the server
# mod_ui_03_run_model_server("ui_03_run_model_ui_1")
